# Multimodal RAG Pipeline for Extracting Eviction Case Outcomes from Court Documents

## Project Summary

This project builds a scalable retrieval-augmented generation (RAG) pipeline to extract structured behavioral and legal outcome measures from unstructured eviction court documents.

The pipeline processes approximately **195,000 PDFs across 8,000+ eviction cases** filed in Pierce County, Washington (2022–2024). These documents contain critical information about tenant behavior, legal representation, procedural events, and case outcomes that are not captured in administrative datasets.

The system combines:

- Cloud-based OCR for large-scale document digitization  
- Rule-based document classification  
- Large language model (LLM) extraction of legal and behavioral variables  
- Structured data integration for statistical modeling  

This project demonstrates how multimodal machine learning pipelines can expand measurement capabilities in complex administrative and legal systems.

---

## Pipeline Overview

There are four main stages in the pipeline:

1. Text extraction (OCR)
2. Document identification and classification
3. LLM variable estimation
4. Data integration and analysis

---

## Setup

Requires Azure credentials for OCR and LLM inference.

Create a `.env` file containing:

- Azure OpenAI credentials  
- Azure Document Intelligence credentials  

You will also need sufficient Azure credits to run OCR and LLM inference at scale.

---

## OCR Text Extraction

These scripts use a pre-built OCR model in Azure Document Intelligence to extract text from all documents in the dataset.

If necessary, `rerun_ocr.R` can be used to selectively rerun failed jobs.

### Scripts (in `/scripts`)
- `test_ocr.py`
- `rerun_ocr.R`
- `get_ocr_results.R`
- `create_ocr_final.R`

---

## Algorithmic Document Identification

These scripts loop through case folders containing labeled PDF documents to identify document types and estimate case-level procedural outcomes based on document presence.

This method is **>90% accurate** for many case outcomes but cannot capture certain hearing outcomes and performs less reliably for complex measures such as access to legal representation.

### Scripts (in `/scripts`)
- `algorithmic_doc_search.R`
- `get_leaf_dirs.R`
- `doctype_search.R`

### Data wrangling
- `algorithmic_data_wrangling.R`

---

## LLM Variable Estimation

These scripts use GPT-4o deployed through Azure OpenAI to estimate tenant behavior and case outcome variables from OCR-extracted text.

Model outputs are returned as JSON and transformed into structured variables during downstream merging.

### Scripts (in `/scripts`)
- `data_from_llm.py`
- `extract_llm_data_agreement.R`
- `extract_llm_data_appearance.R`
- `extract_llm_data_complaint.R`
- `extract_llm_data_minute_entry.R`
- `extract_llm_data_summons.R`

### Data cleaning
- `clean_llm_data_agreement.R`
- `clean_llm_data_appearance.R`
- `clean_llm_data_complaint.R`
- `clean_llm_data_minute_entry.R`
- `clean_llm_data_summons.R`
- `fix_by_path.R`
- `merge_llm_data.R`

---

## Data Integration

These scripts merge algorithmic document classification results with LLM-estimated variables to generate final case-level datasets.

### Scripts (in `/scripts`)
- `merge_algorithmic_w_llm_data.R`

---

## Research Output

Working papers and extended abstracts are being prepared for dissemination in computational social science and NLP venues.

---

## Notes on Data Access

Court documents are not included due to privacy and legal restrictions. This repository focuses on the processing architecture and analytical methods.
