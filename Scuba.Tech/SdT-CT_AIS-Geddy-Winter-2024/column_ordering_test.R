rm(list=ls())
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(gt) 
library(readr)
library(stringr)
library(dplyr)
library(scales)
library(patchwork)
library(cowplot)
library(viridis)
library(stargazer)
library(broom)
###############################################################################################
#file conversion

convert_txt_to_csv <- function(directory) {
  # Check if the directory exists
  if (!dir.exists(directory)) {
    stop("Directory does not exist: ", directory)
  }
  
  # Create the output directory for CSV files
  output_directory <- file.path(directory, "csv_files")
  if (!dir.exists(output_directory)) {
    dir.create(output_directory)
  }
  
  # Get a list of all .txt files in the directory
  txt_files <- list.files(directory, full.names = TRUE, pattern = "\\.txt$")
  
  # Check if there are .txt files
  if (length(txt_files) == 0) {
    cat("No .txt files found in the directory.\n")
    return(NULL)
  }
  
  # Process each .txt file and save as a .csv
  for (file in txt_files) {
    tryCatch({
      # Read the .txt file
      data <- read.table(file, header = TRUE, fill = TRUE, sep = "\t", stringsAsFactors = FALSE)
      
      # Ensure the "Transect" column exists before applying the split
      if ("Transect" %in% names(data)) {
        # Split the "Transect" column by spaces
        split <- do.call(rbind, strsplit(data$Transect, " "))
        split <- as.data.frame(split, stringsAsFactors = FALSE)
        
        # Optionally rename the split columns (adjust names as needed)
        colnames(split) <- paste0("Transect_Split_", seq_len(ncol(split)))
        
        # Combine the split columns back with the original data
        data <- cbind(data, split)
      } else {
        cat("Skipping file (no 'Transect' column found):", file, "\n")
      }
      
      # Generate the output .csv file path
      output_file <- file.path(output_directory, paste0(tools::file_path_sans_ext(basename(file)), ".csv"))
      
      # Write the data to a .csv file
      write.csv(data, output_file, row.names = FALSE)
      
      # Notify that the file was successfully processed
      cat("Converted and transformed:", file, "to", output_file, "\n")
      
    }, error = function(e) {
      # Handle any errors and continue
      cat("Error processing file:", file, "\nError message:", e$message, "\n")
    })
  }
  
  cat("All .txt files have been processed and saved as .csv files in:", output_directory, "\n")
}

convert_txt_to_csv("~/Dropbox/Scuba.Tech/SdT-CT_AIS-Geddy-Winter-2024/transect_data/cleaned")



##########################################################################################

setwd("~/Dropbox/Scuba.Tech/SdT-CT_AIS-Geddy-Winter-2024")



d <- read_csv("aggregate_unprocessed.csv")
data <- read.table("transect_data/cleaned/aggregate.txt", header = TRUE, fill = TRUE, sep = "\t", stringsAsFactors = FALSE)
sev <- read.table("transect_data/cleaned/2007a.txt", header = TRUE, fill = TRUE, sep = "\t", stringsAsFactors = FALSE)



split <- do.call(rbind, strsplit(sev$Transect, " "))
split <- as.data.frame(split, stringsAsFactors = FALSE)


split$V4 <- ifelse(grepl("^[A-Za-z]", split$V5), 
                           paste(split$V4, split$V5, sep = " "), 
                           split$V4)

shift_based_on_character <- function(data) {
  # Define a helper function to check if a value is a number
  is_number <- function(x) {
    suppressWarnings(!is.na(as.numeric(x)))
  }
  
  # Loop through each row
  for (i in seq_len(nrow(data))) {
    # Check if V5 is not a number
    if (!is_number(data$V5[i])) {
      # Shift the row 1 space
      for (col in 5:(ncol(data) - 1)) {
        data[i, col] <- data[i, col + 1]
      }
      # Clear the last column
      data[i, ncol(data)] <- NA
    }
    # Check if V6 is not a number
    else if (!is_number(data$V6[i])) {
      # Shift the row 2 spaces
      for (col in 5:(ncol(data) - 2)) {
        data[i, col] <- data[i, col + 2]
      }
      # Clear the last two columns
      data[i, (ncol(data) - 1):ncol(data)] <- NA
    }
  }
  
  return(data)
}

split <- shift_based_on_character(split)










