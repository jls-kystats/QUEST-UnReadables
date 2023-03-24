# CleanUnreadables.r 
# This script prompts the user for the location of a 
# "Records_Rejected_Unreadable.txt" (RRU) file produced by QUEST as a by-product
# of loading a quarterly UI Extract file. It assumes that the unreadable 
# character is one of 3 possible characters: DC2, SYN, or TAB. It does not 
# reference the companion file also produced by QUEST 
# (Unreadable_Character_Positions.csv)
# 
# The user will be prompted for the RRU file, and the file will be read into
# a character vector. 
# 
# Using regex strings, each appearance of the 3 characters will be handled as 
# follows: 
#   DC2 -> replaced by apostrophe (')
#   SYN -> replaced by dash (-)
#   TAB -> replaced by a space
# 
# The quarter and year will be taken from the file, and the newly-corrected
# result will be written back into a .txt file. 
# 
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

# Get a char vector of files produced by QUEST (prompt user for files)
RRU_file_list <-  choose.files(caption = "Select RRU files (2)",
                               multi = TRUE)

numFIles <- length(RRU_file_list)
if (numFIles != 2) {
  exit("2 files were not chosen - exiting script.")
}

#
# Parse this file path into components
#
folder <- paste(path_dir(RRU_file),"/",sep="")
fName <- path_file(RRU_file)
baseName <- file_path_sans_ext(fName)
fExt <- file_ext(fName)
newPath <- paste(folder,baseName,"_Cleaned.",fExt,sep="")

cat("Reading file",fName,"\n")

#
# Read data from the unreadable_character_positions file
#
col_file_path <- "L:\\Larry\\unreadable characters\\Q3_2023_Unreadable_Character_Positions.csv"
col_df <- read.csv(col_file_path)
colnames(col_df)[1] = "line"
colnames(col_df)[2] = "column"
col_tbl <- as_tibble(col_df) %>% 
  separate_rows(column, convert = TRUE) %>% 
  drop_na()
num_bad_chars <- nrow(col_tbl)


cat(format(nrow(lehd_file), nsmall=0, big.mark=","),
  "Observations were found. \nBegining cleaning of",fName,
  "- This may take several minutes.\n")
#
# Clean this file
#
lehd_file %>% 
#
#  Remove any lines w/o 'KY' in FIPS col (e.g. lines w/NULL values)
#
subset(FIPS=='KY') %>% 
#
# Replace values in FIPS col with '21'
#
mutate(FIPS='21') %>%
#
# For each UIN that ends with an alpha character, pre-pend with a zero and 
# remove the alpha character
#
mutate(UIN=str_replace(UIN,"([0-9]{9})[A-Z]","0\\1")) %>% 
#
# Look for the unprintable 'DC2' chars in the Name col. 
# If found repl w/"'"
#
mutate(Name=str_replace_all(Name,"\x12","'")) %>%
#
# Look for the unprintable 'SYN' char in the Name col. If found repl w/'-'
#
mutate(Name=str_replace_all(Name,"\x16","-")) %>% 
#
# Write results to new non-delimited file in same folder as source, but
# with "_Cleaned" appended to the file name
#
write_delim(newPath, 
            col_names = FALSE, 
            delim = "",
            quote = "none",
            escape = "none",
            eol="\r\n")

cat("File successfully cleaned and saved as",newPath,"\n")