#------------------#
# Setup            #
# Kevin Potter     #
# Updated 04/19/16 #
#------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Load in useful packages
library(Rcpp)
library( utilityf )

# Load in useful functions
source("F1_useful_functions.R")

# Load in data
load("Cleaned_data.RData")

# Select main experimental trials
curData = allData[ allData$BlockType == 2, ]