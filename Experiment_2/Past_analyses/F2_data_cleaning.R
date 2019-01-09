#--------------------------#
# Script for data cleaning #
# Kevin Potter             #
# Updated 05/17/2016       #
#--------------------------#

# Index
# Lookup - 01:  Initial setup
# Lookup - 02:  Remove timeout responses
# Lookup - 03:  Remove excessively fast responses
# Lookup - 04:  Probabilistically trim unlikely responses
# Lookup - 05:  Remove aberrant subjects
# Lookup - 06:  Save new dataset

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
Pilot = T # Indicates if pilot data should be loaded
if (Pilot) {
  inName = "Pilot_subject_csv_data.RData" 
  outName = "Cleaned_pilot_data.RData"
} else {
  inName ="Subject_csv_data.RData"
  outName = "Cleaned_data.RData"
}
load( inName ); rm( inName )

# Save original data
rawData = allData

# Clean out empty log blocks
sel = allData$Angle != 0 & allData$FixTime != 0
allData = allData[ sel, ]

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
  
  # If the logged response == 2, this indicates a timeout (i.e. RT >= ~5 seconds )
  timeout = 4.995
  N_timeout = sum( allData$Choice[sel] == 2 ) # Total number of timeouts
  keep = allData$Choice != 2 | !sel
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
  
  if (Pilot) {
    
    sbj = paste("S",unique( allData$Subject ), sep = "" )
    
    beta_crit = 1.5
    alpha_crit = .5
    
    allAlpha = numeric( N )
    allBeta = numeric( N )
    
    for ( s in 1:N ) {
      
      mat = matData[[ sbj[s] ]]
      
      alphaAdaptive = mat$alpha.grid[ 
        which( mat$alpha.prior == max(mat$alpha.prior) ), 1 ]
      betaAdaptive = mat$beta.grid[ 
        1, which( mat$beta.prior == max(mat$beta.prior) ) ]
      
      allAlpha[s] = alphaAdaptive
      allBeta[s] = betaAdaptive
    }
    
    # Keep subjects who had an sufficiently high cut-off for the 
    # psychometric model that was fit to the data.
    keep = which( allBeta > beta_crit & allAlpha > alpha_crit )
    
  } else keep = 1:length(sbj)
  
  # Index of subjects
  sbj = unique( allData$Subject )
  
  newData = c()
  for ( s in sbj ) {
    
    if ( sum( s == sbj[ keep ] ) > 0 ) {
      sel = allData$Subject == s
      newData = rbind( newData, allData[ sel, ] )
    }
    
  }
  allData = newData
  
  # Trim subjects with poor accuracy during the practice trials
  # For each subject, compare a SDT model for 2AFC with d' fixed 
  # to 0 across conditions against a model in which d' is free 
  # to vary
  # If the constrained model is notably better based on the LOO
  # measure (an approximation to leave-one-out CV), remove that 
  # subject
  # source("F2_2AFC_SDT_1_subject.R")
}

### Save new dataset ###
# Lookup - 06

# Record of cleaning
if ( sum(runCodeMain[1:3])==3 & !runCodeMain[4] ) {
  data_cleaning_report = list(
    c( timeout = timeout, N_timeout = N_timeout),
    c( too_fast = too_fast, N_too_fast = N_too_fast ),
    list( too_slow = too_slow, N_too_slow = N_too_slow )
  )
  
  N = length( unique( allData$Subject ) ) # Adjust N
  save(allData,rawData,data_cleaning_report,N,matData,file=outName)
}

# Record of cleaning
if ( sum(runCodeMain[1:4])==4 ) {
  data_cleaning_report = list(
    c( timeout = timeout, N_timeout = N_timeout),
    c( too_fast = too_fast, N_too_fast = N_too_fast ),
    list( too_slow = too_slow, N_too_slow = N_too_slow ),
    c( N_removed = length(sbj) - length( keep ) )
  )
  
  N = length( unique( allData$Subject ) ) # Adjust N
  save(allData,rawData,data_cleaning_report,N,matData,file=outName)
}