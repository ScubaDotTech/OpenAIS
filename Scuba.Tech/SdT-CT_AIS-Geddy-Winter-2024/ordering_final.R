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

setwd("/Users/geddylucier/Dropbox/Scuba.Tech/SdT-CT_AIS-Geddy-Winter-2024")
getwd()





data <- readLines("transect_data/cleaned/2007a.txt")
data <- as.data.frame(do.call(rbind, strsplit(data, "\\s+", perl = TRUE)))


colnames(data) <- data[1, ]

data <- data[-1, ]


fix_lat_long_issue <- function(data) {
  for (i in 1:nrow(data)) {
    # Check if the "Longitude" column exists and starts with '4.'
    if (!is.na(data[i, "Longitude"]) && grepl("^4", as.character(data[i, "Longitude"]))) {
      # Shift all columns after "Longitude" to the right
      data[i, 5:ncol(data)] <- c(data[i, 6:(ncol(data) - 1)], NA)
    }
  }
  return(data)
}
data_fixed <- fix_lat_long_issue(data)




d <- read_csv("aggregate_unprocessed.csv")
data <- read.table("transect_data/cleaned/aggregate.txt", header = TRUE, fill = TRUE, sep = "\t", stringsAsFactors = FALSE)

column_to_split <- as.character(d[[1]])  # Convert to character if necessary

# Step 2: Split the column by spaces
split_result <- do.call(rbind, strsplit(column_to_split, " "))

# Step 3: Convert the result into a data frame
split <- as.data.frame(split_result, stringsAsFactors = FALSE)



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

combined_data <- bind_rows(processed_data_list, .id = "file_id")

# Save the combined data
write.table(combined_data, "combined_output.txt", sep = "\t", row.names = FALSE, quote = FALSE)




