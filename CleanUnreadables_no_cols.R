# CleanUnreadables.r 
# This script prompts the user for the location of a 
# "Records_Rejected_Unreadable.txt" (RRU) file produced by QUEST as a by-product
# of loading a quarterly UI Extract file. It assumes that the unreadable 
# character is one of 4 possible characters: NUL, DC2, SYN, or TAB. It does not 
# reference the companion file also produced by QUEST 
# (Unreadable_Character_Positions.csv)
# 
# The user will be prompted for the RRU file, and the file will be read into
# a list of raw vectors b/c the records contain NULL characters. 
# 
# Each appearance of the 3 characters will be handled as 
# follows: 
#   DC2 -> replaced by apostrophe (')
#   SYN -> replaced by dash (-)
#   NULL/TAB -> replaced by a space
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

# Get the path of the QUEST Records_Rejected file)
cat("Opening 'Choose File' dialog box...\n")
RRU_file <- NA
RRU_file <-  file.choose(new = FALSE)

if (is.na(RRU_file)) {
  exit("2 files were not chosen - exiting script.")
}

#
# Parse this file path into components
#
folder <- paste(path_dir(RRU_file),"/",sep="")
fName <- path_file(RRU_file)
baseName <- file_path_sans_ext(fName)
fExt <- file_ext(fName)

outFile <- paste(folder,baseName,"_Cleaned.",fExt,sep="")

cat("Reading file",fName,"\n")
#
# Read data from the unreadable_characters file
#
RRU_raw <- read_lines_raw(RRU_file)  # Read as raw b/c file contains nulls

cat("Processing",fName,"\n")
for (lineNbr in 1:length(RRU_raw)) {
  line_as_int <- as.integer(RRU_raw[[lineNbr]]) # convert raw vector to int vctr
  line_as_int[line_as_int == 0 | line_as_int == 9] <- 32  # null/TAB -> space
  line_as_int[line_as_int == 18] <- 39  # DC2 -> '
  line_as_int[line_as_int == 22] <- 45  # SYN2 -> -
  # write cleaned line to output file
  if (lineNbr == 1) {
    write(rawToChar(as.raw(line_as_int)),outFile,ncolumns=1,append=FALSE,sep="")
  } else {
    write(rawToChar(as.raw(line_as_int)),outFile,ncolumns=1,append=TRUE,sep="")
  }
}

cat("File successfully cleaned and saved as",outFile,"\n")