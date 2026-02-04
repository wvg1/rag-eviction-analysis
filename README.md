# Using retrieval-augmented generation (RAG) to analyze eviction case outcomes

This repo contains code that extracts and processes all text from approximately 195,000 PDF documents filed in eviction cases in Pierce County, Washington between 2022-2024 to create novel measures of several important outcomes in eviction proceedings. In order to run this code, you will need to have access to the Microsoft Azure portal and enough account credits to run the OCR and the LLM analysis. There are four main stages in the pipeline: text extraction, document identification, LLM variable estimation, and data analysis.

## Setup
Create .env file and fill in your Azure credentials.

## OCR text extraction
These scripts use a pre-built OCR model in the Azure portal to extract text from all of the documents in our dataset. If necessary, rerun_ocr.R can be used 

Scripts (in /scripts): test_ocr.py, rerun_ocr.R, get_ocr_results.R, create_ocr_final.R

## Algorithmic document identification
These scripts loop through case folders with all labeled PDF documents to search for certain types of documents and estimate case-level outcomes based on which documents are included in each case folder. This method is >90% effective for many case outcomes, but does not allow us to measure some hearing outcomes and is less accurate for other more complex measures such as tenants' access to legal representation.

Scripts (in /scripts): algorithmic_doc_search.R, get_leaf_dirs.R, doctype_search.R
Data wrangling prior to merging with LLM estimated outcomes: algorithmic_data_wrangling.R

## LLM variable estimation
These scripts use ChatGPT-4o through the Azure portal to estimate a series of tenant behavior and case outcome variables. GPT outputs JSON strings that are then transformed to variables of interest during merging with algorithmic document identification data.

Scripts (in /scripts): data_from_llm.py, extract_llm_data_agreement.R, extract_llm_data_appearance.R, extract_llm_data_complaint.R, extract_llm_data_minute_entry.R, extract_llm_data_summons.R

Data cleaning: clean_llm_data_agreement.R, clean_llm_data_appearance.R, clean_llm_data_complaint.R, clean_llm_data_minute_entry.R, clean_llm_data_summons.R, fix_by_path.R, merge_llm_data

## Data merging
These scripts merge the algorithmic document identification data with the LLM variable estimates to create the most accurate estimates possible.

Script (in /scripts): merge_algorithmic_w_llm_data.R

## Document layout analysis
The repo contains a few scripts related to using Azure document layout analysis to measure procedural outcomes. We found that document analysis was very limited in its accuracy and general usefulness for our research objectives.
