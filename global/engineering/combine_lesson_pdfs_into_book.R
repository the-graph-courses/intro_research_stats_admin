# Load packages 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, pdftools)

# set version based on year, counting from 2023 (2023 is 0) and month, such as V012 for December 2023 and V102 for February 2024
version <- paste0("v", as.numeric(format(Sys.Date(), "%y")) - 23, format(Sys.Date(), "%m"))

# Set working directory
dir <- here::here("")
dir <- stringr::str_replace(dir, "_staging", "") # Remove _staging from path

# Define the list of PDF files
pdfs_to_combine_EN <-
  c(
    "EPIREP_EN_00_cover.pdf",
    "EPIREP_EN_demo_pyramid/EPIREP_EN_demo_pyramid.pdf",
    "EPIREP_EN_parts_to_a_whole/EPIREP_EN_parts_to_a_whole.pdf",
    "EPIREP_EN_labels/EPIREP_EN_labels.pdf",
    "EPIREP_EN_choropleth_maps/EPIREP_EN_choropleth_maps.pdf",
    "EPIREP_EN_choropleth_maps_labeling/EPIREP_EN_choropleth_maps_labeling.pdf",
    "EPIREP_EN_grammar_of_tables/EPIREP_EN_grammar_of_tables.pdf",
    "EPIREP_EN_grammar_of_tables_2/EPIREP_EN_grammar_of_tables_2.pdf",
    "EPIREP_EN_time_series/EPIREP_EN_time_series.pdf",
    "EPIREP_EN_automating_visualization/EPIREP_EN_automating_visualization.pdf",
    "EPIREP_EN_parameterizing_reports/EPIREP_EN_parameterizing_reports.pdf"
  )

# Combine PDFs
final_output_name_EN <- paste0("epidemiological_reporting_with_r_", version, ".pdf")
output_file_EN <- file.path(dir, final_output_name_EN)
pdf_combine(file.path(dir, pdfs_to_combine_EN), output = output_file_EN)

# Print success message
cat("PDFs combined into:", output_file_EN)


# Define the list of PDF files FR
pdfs_to_combine_FR <-
  c(
    "EPIREP_FR_00_cover.pdf",
    "EPIREP_FR_demo_pyramid/EPIREP_FR_demo_pyramid.pdf",
    "EPIREP_FR_parts_to_a_whole/EPIREP_FR_parts_to_a_whole.pdf",
    "EPIREP_FR_labels/EPIREP_FR_labels.pdf",
    "EPIREP_FR_choropleth_maps/EPIREP_FR_choropleth_maps.pdf",
    "EPIREP_FR_choropleth_maps_labeling/EPIREP_FR_choropleth_maps_labeling.pdf",
    "EPIREP_FR_grammar_of_tables/EPIREP_FR_grammar_of_tables.pdf",
    "EPIREP_FR_grammar_of_tables_2/EPIREP_FR_grammar_of_tables_2.pdf",
    "EPIREP_FR_time_series/EPIREP_FR_time_series.pdf",
    "EPIREP_FR_automating_visualization/EPIREP_FR_automating_visualization.pdf",
    "EPIREP_FR_parameterizing_reports/EPIREP_FR_parameterizing_reports.pdf"
  )

# Combine PDFs
final_output_name_FR <- paste0("rapports_epidemiologiques_avec_r_", version, ".pdf")
output_file_FR <- file.path(dir, final_output_name_FR)
pdf_combine(file.path(dir, pdfs_to_combine_FR), output = output_file_FR)

# Print success message
cat("PDFs combined into:", output_file_FR)


