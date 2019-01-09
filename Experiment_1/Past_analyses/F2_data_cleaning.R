#--------------------------#
# Script for data cleaning #
# Kevin Potter             #
# Updated 04/20/2016       #
#--------------------------#

# Index
# Lookup - 01:  Initial setup
# Lookup - 02:  Remove timeout responses
# Lookup - 03:  Remove excessively fast responses
# Lookup - 04
# Lookup - 05
# Lookup - 06

### Initial setup ###
# Lookup - 01

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Load in useful functions
library(utilityf)
library(rtclean)
source("F1_useful_functions.R")

# Load in data
load("All_subject_csv_data.RData")

# Save original data
rawData = allData

# Select which code segments to run
runCodeMain = c(
  T,
  T,
  T,
  T
)

### Remove timeout responses ###
# Lookup - 02

if (runCodeMain[1]) {
  # Trim data from test blocks only
  sel = allData$BlockType == 2
  
  timeout = 4.995 # If RT >= ~5 seconds, indicates a timeout
  N_timeout = sum( allData$RT[sel] >= timeout ) # Total number of timeouts
  keep = allData$RT < timeout | !sel
  allData = allData[ keep, ]
}

### Remove excessively fast responses ###
# Lookup - 03

if (runCodeMain[2]) {
  # Trim data from test blocks only
  sel = allData$BlockType == 2
  
  # Somewhat arbitrarily, we'll define fast responses to be equal to or under 100 ms
  too_fast = .1
  N_too_fast = sum( allData$RT[sel] <= too_fast ) # Total number of fast responses
  keep = allData$RT > too_fast | !sel
  allData = allData[ keep, ]
}

### Probabilistically trim unlikely responses ###
# Lookup - 04

if (runCodeMain[3]) {
  
  newData = c()
  too_slow = numeric( nrow( allData ) )
  
  # Data from a specific subject
  subj = sort( unique( allData$Subject ) )
  
  # Create a progress bar using a base R function
  pb = txtProgressBar( min = 1, max = length(subj), style = 3 )
  
  for (s in 1:length(subj)) {
    curData = allData[ allData$Subject == subj[s], ]
    
    # Trim data from test blocks only
    sel = curData$BlockType == 2
    mainData = curData[sel, ]
    
    keep = rep(F,nrow(mainData))
    inc = 1
    while( sum(keep) == 0 & inc <= 20 ) {
      tst = rtclean( mainData$RT, exclude = 1 )
      keep = !tst$exclude_1
      inc = inc + 1
    }
    too_slow[ allData$Subject == subj[s] ][sel] = keep
    
    if (sum(keep) == 0) break();
    
    newData = rbind( newData,
                     rbind( curData[ !sel, ],
                            mainData[ keep, ] ) )
    
    # Update the progress bar
    setTxtProgressBar(pb,s)
  }
  close(pb)
  
  # Total number of responses that were too slow
  N_too_slow = nrow(allData) - nrow(newData)
  allData = newData
}

# Check trimming
# aggregate( rep(1,nrow(allData)), list(allData$Subject), sum )$x

### Remove aberrant subjects ###
# Lookup - 05

if (runCodeMain[4]) {
  
  # Trim subjects with poor accuracy during the practice trials
  # For each subject, compare a SDT model for 2AFC with d' fixed 
  # to 0 across conditions against a model in which d' is free 
  # to vary
  # If the constrained model is notably better based on the LOO
  # measure (an approximation to leave-one-out CV), remove that 
  # subject
  source('F2_2AFC_SDT_1_subject.R')
}

### Save new dataset ###
# Lookup - 06

# Record of cleaning
if ( sum(runCodeMain[1:4])==4 ) {
  data_cleaning_report = list(
    c( timeout = timeout, N_timeout = N_timeout),
    c( too_fast = too_fast, N_too_fast = N_too_fast ),
    list( too_slow = too_slow, N_too_slow = N_too_slow ),
    c( N_removed = sum( purge ) )
  )
  
  N = N - sum(purge) # Adjust N
  save(allData,rawData,data_cleaning_report,N,file='Cleaned_data.RData')
}