"""
Batch process PDFs through Azure Computer Vision Read API with rate limiting.
Includes progress tracking and will resume after a crash
"""
import os
import json
import csv
import time
from pathlib import Path
from dotenv import load_dotenv
from azure.core.credentials import AzureKeyCredential
from azure.ai.vision.imageanalysis import ImageAnalysisClient
from pdf2image import convert_from_path
import logging
from datetime import datetime

# set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('ocr_processing.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

# load environment variables
load_dotenv()

AZURE_CV_ENDPOINT = os.getenv("AZURE_CV_ENDPOINT")
AZURE_CV_KEY = os.getenv("AZURE_CV_KEY")
DATA_FOLDER = "data"
OUTPUT_FOLDER = "output"
CHECKPOINT_FILE = os.path.join(OUTPUT_FOLDER, "checkpoint.json")
BATCH_SIZE = 10  # save results 
RATE_LIMIT_DELAY = 0.5
MAX_RETRIES = 3

def init_vision_client():
    """initialize Azure CV client"""
    return ImageAnalysisClient(
        endpoint=AZURE_CV_ENDPOINT, 
        credential=AzureKeyCredential(AZURE_CV_KEY)
    )

def load_checkpoint():
    """load checkpoint to resume processing"""
    if os.path.exists(CHECKPOINT_FILE):
        with open(CHECKPOINT_FILE, 'r') as f:
            return json.load(f)
    return {"processed_files": [], "total_pages": 0, "last_batch": 0}

def save_checkpoint(checkpoint):
    """save checkpoint for resume capability."""
    with open(CHECKPOINT_FILE, 'w') as f:
        json.dump(checkpoint, f, indent=2)

def extract_text_from_pdf(client, pdf_path, retry_count=0):
    """
    extract text from all pages of a PDF using Azure OCR
    includes retry logic for rate limiting.
    """
    try:
        # convert PDF to images
        images = convert_from_path(pdf_path)
        extracted_data = {
            "filename": os.path.basename(pdf_path),
            "page_count": len(images),
            "pages": [],
            "processed_at": datetime.now().isoformat()
        }
        
        for page_num, image in enumerate(images, 1):
            # save image temporarily
            temp_image_path = f"temp_page_{page_num}.jpg"
            image.save(temp_image_path, "JPEG")
            
            try:
                # read image file
                with open(temp_image_path, "rb") as image_file:
                    image_data = image_file.read()
                
                # analyze with Azure
                result = client.analyze_image_in_stream(
                    image_stream=image_data,
                    visual_features=["READ"]
                )
                
                # extract text
                page_text = ""
                if result.read:
                    for block in result.read.blocks:
                        for line in block.lines:
                            page_text += line.text + "\n"
                
                extracted_data["pages"].append({
                    "page_number": page_num,
                    "text": page_text.strip()
                })
                
                logger.info(f"Extracted page {page_num}/{len(images)} from {os.path.basename(pdf_path)}")
                
            finally:
                # clean up temp image
                if os.path.exists(temp_image_path):
                    os.remove(temp_image_path)
            
            # rate limiting
            time.sleep(RATE_LIMIT_DELAY)
        
        return extracted_data
    
    except Exception as e:
        if retry_count < MAX_RETRIES:
            logger.warning(f"Retry {retry_count + 1}/{MAX_RETRIES} for {pdf_path}: {str(e)}")
            time.sleep(2 ** retry_count)  # exponential backoff
            return extract_text_from_pdf(client, pdf_path, retry_count + 1)
        else:
            logger.error(f"Failed to process {pdf_path} after {MAX_RETRIES} retries: {str(e)}")
            return None

def save_batch_results(results, batch_num):
    """save batch results to both JSON and CSV"""
    os.makedirs(OUTPUT_FOLDER, exist_ok=True)
    
    json_output = os.path.join(OUTPUT_FOLDER, f"batch_{batch_num:05d}.json")
    csv_output = os.path.join(OUTPUT_FOLDER, f"batch_{batch_num:05d}.csv")
    
    # save JSON (structured)
    with open(json_output, 'w', encoding='utf-8') as f:
        json.dump(results, f, indent=2, ensure_ascii=False)
    
    # save CSV (for easy analysis)
    with open(csv_output, 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow(['filename', 'page_number', 'text', 'processed_at'])
        
        for doc in results:
            if doc:
                for page in doc['pages']:
                    writer.writerow([
                        doc['filename'],
                        page['page_number'],
                        page['text'],
                        doc.get('processed_at', '')
                    ])
    
    logger.info(f"Saved batch {batch_num} to {json_output} and {csv_output}")
    return len(results)

def process_pdfs_with_resume():
    """
    process all PDFs with checkpoint/resume capability.
    """
    os.makedirs(OUTPUT_FOLDER, exist_ok=True)
    
    # load checkpoint
    checkpoint = load_checkpoint()
    processed_files = set(checkpoint["processed_files"])
    
    # get list of PDFs
    pdf_files = sorted(list(Path(DATA_FOLDER).glob("*.pdf")))
    remaining_pdfs = [p for p in pdf_files if p.name not in processed_files]
    total_pdfs = len(pdf_files)
    
    if len(remaining_pdfs) == 0:
        logger.info(f"All {total_pdfs} PDFs already processed!")
        return
    
    logger.info(f"Total PDFs: {total_pdfs}, Remaining: {len(remaining_pdfs)}")
    
    client = init_vision_client()
    batch_results = []
    batch_num = checkpoint["last_batch"]
    
    for idx, pdf_path in enumerate(remaining_pdfs, 1):
        logger.info(f"Processing {idx}/{len(remaining_pdfs)}: {pdf_path.name}")
        
        result = extract_text_from_pdf(client, str(pdf_path))
        if result:
            batch_results.append(result)
            processed_files.add(pdf_path.name)
        
        # save batch and checkpoint
        if idx % BATCH_SIZE == 0:
            batch_num += 1
            save_batch_results(batch_results, batch_num)
            
            # update checkpoint
            checkpoint["processed_files"] = list(processed_files)
            checkpoint["last_batch"] = batch_num
            checkpoint["total_pages"] += sum(doc['page_count'] for doc in batch_results)
            save_checkpoint(checkpoint)
            
            logger.info(f"Progress: {len(processed_files)}/{total_pdfs} PDFs processed")
            batch_results = []
    
    # save remaining results
    if batch_results:
        batch_num += 1
        save_batch_results(batch_results, batch_num)
        checkpoint["processed_files"] = list(processed_files)
        checkpoint["last_batch"] = batch_num
        checkpoint["total_pages"] += sum(doc['page_count'] for doc in batch_results)
        save_checkpoint(checkpoint)
    
    logger.info(f"✓ All {total_pdfs} PDFs processed! Total pages: {checkpoint['total_pages']}")

if __name__ == "__main__":
    process_pdfs_with_resume()