#this script creates the original dataframe from OCR output and merges it with the rerun of that same OCR script for documents that initially returned a response of NA
#working directory should be the main eviction-data repo

#load packages
library(tidyverse)

#load in original ocr data
ocr_2023 <- read_delim("data/all_docs_all_text-2023.csv", delim = "||$%^||")
ocr_2022_24 <- read_delim("data/all_docs_all_text-2022.csv", delim = "||$%^||")
ocr_combined <- rbind(ocr_2023, ocr_2022_24)

#load in rerun of OCR for documents that initially returned NA
ocr_rerun <- read_rds("data/ocr_rerun.rds")

#drop resolved_path column from ocr_rerun
ocr_rerun <- ocr_rerun %>%
  select(-resolved_path)

#replace NA text values in ocr_combined with rerun results, and drop remaining NAs
ocr_final <- ocr_combined %>%
  rows_update(ocr_rerun, by = c("direc", "document"))

#drop duplicate header rows
ocr_final <- ocr_final %>%
  filter(document != "document")

#remove attempted OCR observations of .csv, .html, .gif docs and unecessary .pkl files
ocr_final <- ocr_final[!is.na(ocr_final$text),]

#create rds for future use
saveRDS(ocr_final, "data/ocr_final.rds")
