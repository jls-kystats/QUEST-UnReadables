# CleanUnreadables_use_cols.r
# 
# This script prompts the user for the location of two files produced by QUEST
# during processing of a quarterly UI Extract file: 
#    1) Records_Rejected_Unreadable.txt (RRU), and 
#    2) Unreadable_Character_Positions.csv (UCP)
# Note that the script expects these files to be named such that the RRU file
# has "Records_Rejected" in the name, and the UCP has "Unreadable_Character".
# Also note that the RRU file's sort order MUST NOT be changed.
# 
# The UCP file contains a list of lines #'s for the rejected records in the RRU
# file. For each, a comma-separated list of column positions of the  
# "unreadable" character(s) for that record is provided. This script processes
# each line in the RRU and replaces the unreadable characters as follows:: 
#   DC2 (Hex 12/Dec 18) -> replaced by apostrophe (')
#   SYN (Hex 16/Dec 22) -> replaced by dash (-)
#   TAB/NUL/any other unprintable char -> replaced by a space
# 
# The newly-corrected RRU file is then written back into a .txt file with 
# "_Cleaned" appended to the file name. 
#
# Written by Larry Sturm, KYStats, March 2023
#
library(tidyverse)
library(readr)
library(fs)
library(tools)

# Custom func to write msg to user and immediately exit the script
exit <- function(msg) {
  if (!is.na(msg)) print(msg)
  invokeRestart("abort")
}

cat("Opening 'Choose Files' dialog box...\n")

#  Get a char vector of files produced by QUEST (prompt user for files)
RRU_file_list <-  choose.files(caption = "Select the RRU & UCP files (2)",
                               multi = TRUE)
num_files <- length(RRU_file_list)
if (num_files != 2) {
  exit("Must choose 2 files - exiting script.")
}

col_pos_path <- NA
RRU_file_path <- NA
for (q_file in RRU_file_list) {
  if (str_detect(q_file,regex("Unreadable_Character", ignore_case = TRUE))) {
       col_pos_path <- q_file
  } else if (str_detect(q_file,regex("Records_Rejected", ignore_case = TRUE))) {
       RRU_file_path <- q_file
  } else {
       exit("Unrecognized file name - see script comments. Exiting script.")
  }
}
if (is.na(RRU_file_path) | is.na(col_pos_path)) {
  exit("Missing/unexpected file name - see script comments. Exiting script.")
}

#
# Parse the RRU file path into components
#
folder <- paste(path_dir(RRU_file_path),"/",sep="")
RRU_file_name <- path_file(RRU_file_path)
RRU_base_name <- file_path_sans_ext(RRU_file_name)
RRU_ext <- file_ext(RRU_file_name)
#  create path string for output file using RRU file name and folder as a base
output_path <- paste(folder,RRU_base_name,"_Cleaned.",RRU_ext,sep="")

#
# Read data from the unreadable_character_positions file and tidy the format
# to create one observation (row) per 'bad char' column position
#
col_df <- read.csv(col_pos_path)
colnames(col_df)[1] = "line"
colnames(col_df)[2] = "column"
col_tbl <- as_tibble(col_df) %>% 
  separate_rows(column, convert = TRUE) %>% 
  drop_na()
num_bad_chars <- nrow(col_tbl)  # Note the total number of bad characters

# 
# Read data from the RRU file as raw (HEX) b/c NUL characters create havoc
# 
RRU_raw <- read_lines_raw(RRU_file_path)

cat("Beginning to clean",length(RRU_raw),"RRU records with",num_bad_chars,
    "unreadable characters\n")
# 
# Loop through each line in the RRU file: remove all unprintable characters, 
# then write the cleaned record to the output file
# 
cleaned_counter <- 0  # keep track of cleaned chars
for (line_nbr in 1:length(RRU_raw)) {
    #  convert raw vector to integer vector for easy replacement and conversion
    #  this allows each character to be referred to by its ASCII decimal value
    line_as_int <- as.integer(RRU_raw[[line_nbr]])

    #  filter col pos tbl to only those rows with the current RRU rec's line #
    col_subset <- col_tbl %>% filter(line == line_nbr) 
     
    #  loop thru all 'bad char' column positions for this record
    for (cPos in col_subset$column) {
        if (line_as_int[cPos] == 18) {          # this is a DC2 
             line_as_int[cPos] <- 39            #   replace w/'
        } else if (line_as_int[cPos] == 22) {   # this is a SYN2 
             line_as_int[cPos] <- 45            #   replace w/-
        } else {line_as_int[cPos] <- 32}# repl all others (e.g. TAB) with space
        cleaned_counter <- cleaned_counter + 1
    } #  End of loop thru 'bad char' column positions
    
    line_as_int[line_as_int == 0] <- 32 # lastly, convert all NUL chars to space
    
    #  write result to file
    if (line_nbr == 1) {
        # Convert back to char & write 1st rec to output file (overwrite any 
        # existing file)
        write(rawToChar(as.raw(line_as_int)),output_path,ncolumns=1,
              append=FALSE,sep="")
    } else {
        #  Convert to char and append this cleaned record to output file
        write(rawToChar(as.raw(line_as_int)),output_path,ncolumns=1,
              append=TRUE,sep="")
    }
}   #  End of loop thru raw RRU records
# Finished
cat(cleaned_counter,"characters were replaced.\n",
    "RRU file successfully cleaned and saved as",output_path,"\n")