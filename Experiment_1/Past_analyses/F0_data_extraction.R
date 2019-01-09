#-----------------------------#
# Script to create R data set #
# Kevin Potter                #
# Updated 04/07/16            #
#-----------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Set working directory to location of data
setwd("~/Experiments/Gabor_priming_2016/Subjects")
subjDir = getwd();
N = (length(dir())-1)/3 # Number of subjects

# Note that subjects 10, 20, and 30 ended up with the 
# same rng state as subjects 1, 2, and 3

# Load in data from every subject
allData = c()
for (i in 1:length(dir())) {
  
  tmp = strsplit( dir()[i], split = "_" )[[1]]
  if ( tmp[1] == "Subject" ) {
    if ( length(grep( ".csv", dir()[i] )) > 0 ) {
      tmpData = read.table( dir()[i],header=T,sep=',')
      sel = tmpData$RT != 0 & tmpData$Angle != 0 & tmpData$PrimeTime != 0
      tmpData = tmpData[ sel, ] # Trim out block of zeros
      if ( tmp[2] == "10.csv" | 
           tmp[2] == "20.csv" | 
           tmp[2] == "30.csv" | 
           tmp[2] == "40.csv" ) {
        sn = as.numeric( strsplit( tmp[2], split = ".c" )[[1]][1] )
        tmpData$Subject = sn # Correct mislabeled subject numbers
      }
      allData = rbind( allData, tmpData )
    }
  }
  
}

# Clean up workspace
rm( i, tmp, tmpData, sn )

# Save extracted data
setwd( orig_dir )
save(allData,N,file="All_subject_csv_data.RData")