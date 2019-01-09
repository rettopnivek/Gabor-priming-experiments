#------------------#
# Setup            #
# Kevin Potter     #
# Updated 05/17/16 #
#------------------#

# Save current working directory
orig_dir = getwd()

# Load in useful packages
library(Rcpp)
library( utilityf )

# Load in useful functions
source("F1_useful_functions.R")

# Load in data
if ( Pilot ) load("Cleaned_pilot_data.RData") else load("Cleaned_data.RData")

# Select main experimental trials
cD = allData[ allData$BlockType == 2, ]