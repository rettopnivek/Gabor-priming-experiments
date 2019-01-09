#-----------------------------#
# Script to create R data set #
# Kevin Potter                #
# Updated 05/17/16            #
#-----------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate if extracting pilot data or current data
Pilot = T
if ( Pilot == T ) {
  folderName = "~/Experiments/Gabor_priming_2016_v2/Pilot_subjects"
  fileName = "Pilot_subject_csv_data.RData"
} else {
  folderName = "~/Experiments/Gabor_priming_2016_v2/Subjects"
  fileName = "Subject_csv_data.RData"
}

# Set working directory to location of data
setwd(folderName)
subjDir = getwd();

# Load in data from every subject
allData = c()
inc = c( 1, 1, 1 )
for (i in 1:length(dir())) {
  
  tmp = strsplit( dir()[i], split = "_" )[[1]]
  if ( tmp[1] == "Subject" ) {
    if ( length(grep( ".csv", dir()[i] )) > 0 ) {
      tmpData = read.table( dir()[i],header=T,sep=',')
      if ( Pilot ) {
        sbjN = unique( tmpData$Subject )
        if ( sbjN == 1 | sbjN == 2 | sbjN == 3 ) {
          if ( inc[ sbjN ] == 1 ) {
            inc[ sbjN ] = 2
          } else {
            tmpData$Subject = sbjN*10 # Correct mislabeling of subjects
          }
        }
      }
      allData = rbind( allData, tmpData )
    }
  }
  
}

# Sample size
N = length( unique( allData$Subject ) )

# Load in matlab files for subjects and extract
# adaptive training trial data
library(R.matlab) # Package for loading .mat files into R
matData = c()
for ( i in 1:length(dir()) ) {
  
  tmp = strsplit( dir()[i], split = "_" )[[1]]
  if ( tmp[1] == "Subject" ) {
    if ( length(grep( ".mat", dir()[i] )) > 0 ) {
      tmpData = readMat(dir()[i])
      matData = c( matData, list( tmpData ) )
    }
  }
  
}
names( matData ) = paste( "S", unique( allData$Subject ), sep = "" )

# Clean up workspace
rm( i, tmp, tmpData, Pilot )

# Save extracted data
setwd( orig_dir )
save(allData,matData,N,file=fileName)