# Load required libraries
library(tesseract)
library(magick)
library(stringr)
library(fs)
setwd("~/Dropbox/devlab/restore_econ/data")


setwd("~/Dropbox/Scuba.Tech/SdT-CT_AIS-Geddy-Winter-2024/transect_data")

process_and_ocr <- function(input_dir, output_dir, aggregate_file) {
  dir_create(output_dir)
   
   aggregate_text <- character()
  
      files <- dir_ls(input_dir, regexp = "\\.png$")
  
  for (file in files) {
    tryCatch({
      image <- image_read(file)
      image <- image %>%
        image_resize("4000x4000") %>%      # Increase resolution
        image_convert(colorspace = "gray") %>% # Convert to grayscale
        image_enhance() %>%                 # Enhance image quality
        image_contrast() %>%                # Increase contrast
        image_threshold(type = "white", threshold = "50%") # Binarize
      
      # Save preprocessed image temporarily
      temp_file <- tempfile(fileext = ".png")
      image_write(image, temp_file)
      
      eng <- tesseract("eng")
      ocr_text <- ocr(temp_file, engine = eng)
      
      output_file <- file.path(output_dir, basename(file))
      output_file <- str_replace(output_file, "\\.png$", ".txt")
      
      # Append OCR text to previous year's text if exists
      if (file_exists(output_file)) {
        previous_text <- readLines(output_file)
        combined_text <- c(previous_text, ocr_text)
      } else {
        combined_text <- ocr_text
      }
      
      # Save combined text to output file
      writeLines(combined_text, output_file)
      
      # Add to aggregate text
      aggregate_text <- c(aggregate_text, combined_text)
      
      # Remove temporary file
      file_delete(temp_file)
      
    }, error = function(e) {
      message("Error processing file: ", file, " - ", e$message)
    })
  }
  
  # Save aggregate text to the aggregate file
  writeLines(aggregate_text, aggregate_file)
}

 process_and_ocr(
  input_dir = "~/Dropbox/Scuba.Tech/SdT-CT_AIS-Geddy-Winter-2024/transect_data", 
   output_dir = "~/Dropbox/Scuba.Tech/SdT-CT_AIS-Geddy-Winter-2024/transect_data/cleaned",
  aggregate_file = "~/Dropbox/Scuba.Tech/SdT-CT_AIS-Geddy-Winter-2024/transect_data/cleaned/aggregate.txt"
)
 
 
 
 
