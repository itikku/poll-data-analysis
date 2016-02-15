#First install dplyr (iteration on plyr) and ggplot2, as they are not default libraries

library(MASS)
library(plyr)
library(ggplot2)

# IMPORT DATA #

# What do we want this script to eventually accomplish? #

#   1. Create a 2-D top-level data frame poll_results, with three columns. Height of frame = number of CSV files
#      in a particular folder 'data'
#   2. For each CSV in data
#     a) Load the data into a data frame tmp
#     b) Add a record at row determined by a counter into poll_results with the electoral district number and name
#     c) Strip extra columns (number and name) from tmp
#     d) Make tmp the value of the third column in the record
#     e) Increment counter
#   3. ...



#For now, read a single CSV file from polling station 10001
#The script currently accomplishes the following:

# 1. Read CSV file with values
# 2. Clean and organize data to prepare it for analysis. Our objective of this analysis is to determine if there is any
#    correlation between voter turnout at a poll and the number of votes a particular candidate gets
# 3. Store the results of the analysis in a data frame, organized by candidate name

bureau1 <- read.csv(file="~/Coding Projects/poll-data-analysis/data/pollbypoll_bureauparbureau10001.csv", header = TRUE, sep=",")
head(bureau1)
head(bureau1[1,])
head(bureau1[,1])
#Rename certain columns so they aren't as unwieldly
col_names <- names(bureau1)
num_columns <- length(col_names)
names(bureau1)[c(1:4, (num_columns - 2):num_columns)] <- c("DistrictName", "DistrictNumber", "StationNumber", "StationName", "Rejected", "VotesCast", "Electors")
#Extract candidate names from column names, and then use regex to clean up the names and properly format them
cdt_names <- names(bureau1[5:(num_columns - 3)])
gsub("\\.", " ", cdt_names, perl=TRUE)
#Clean data so that polling stations with values of 'NA' for num. of voters and/or electors are filtered out
bureau1 <- bureau1[which(!is.na(bureau1$VotesCast) & !is.na(bureau1$Electors)),]
num_rows <- length(bureau1[,1])

#function to organize the data for eventual regression
# Calculates turnout by taking votes cast divided by total available electors at a polling station
# Calculates vote % for a candidate by taking votes receives divided by votes cast
# Returns data frame with corresponding vote % and turnout
organize <- function(x, cdt_num){
  turnout <- x$VotesCast / x$Electors
  cdt_vote_pct <- as.numeric(as.character(x[4+cdt_num])) / x$VotesCast
  data.frame(CandidateVotePct = cdt_vote_pct, Turnout = turnout)
}


num_candidates <- num_columns - 7
cdt_analysis <- data.frame(CandidateName = character(num_candidates))
cdt_analysis$CandidateName <- cdt_names

# This is the list in which we will store our vectors of residuals
resids <- vector("list", 5)

for (i in 1:num_candidates){
  bureau1.cdt <- ddply(bureau1, "StationNumber", organize, i)
  bureau1.cdt.regress <- rlm(CandidateVotePct ~ Turnout, data=bureau1.cdt)
  bureau1.cdt.resid <- resid(bureau1.cdt.regress)
  resids[[i]] <- bureau1.cdt.resid
}

#Bind the list of residuals to the corresponding candidate in the data frame
cdt_analysis$Residuals <- resids