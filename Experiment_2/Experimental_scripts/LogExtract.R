#-----------------------------------------------------------#
# Experiment log consolidation script for David Huber's lab #
# Kevin Potter                                              #
# Updated 04/27/2016                                        #
#-----------------------------------------------------------#

# Note that the source computers need to actually be on for this script to work

# Pathway to destination folder
destinationPath = 'C:\\Users\\lab\\Documents\\MATLAB\\Gabor_priming_2016_v2-master\\Subjects'

# Computer names for remote computers with source folders
compNames = paste('CC',2:8,sep='')
compIndex = as.character(2:8)

# Source folder path on a given computer
folderPath = '\\Users\\lab\\Documents\\MATLAB\\Gabor_priming_2016_v2\\Subjects'

# Variable for the final log
FinalLog = c()
Header = NULL # Variable to indicate first time a log is read
logs = NULL

for ( computerSelect in compIndex ) {
  
  # Select a computer
  i = which( compIndex == computerSelect )
  
  # Create the full path by concatenating the strings
  sourcePath = paste( '\\\\', compNames[i], folderPath, sep = '' )
  
  # Track progress
  cat( 'Computer ', compIndex[i], ' progress: ' )
  
  # Change working directory to source path
  # setwd(sourcePath)
  oldPath = tryCatch( setwd(sourcePath), warning = function(w) print( 'Computer is off!' ), 
                      error = function(e) { 'Stop' } )
  if (oldPath != 'Stop' ) {
    nFiles = length( dir() )
    
    if (nFiles > 0 ) {
      
      # Read in the log as a .csv file
      # tmp = scan( file = 'Experiment_log.csv', what = 'character', sep = '\n' )
      tmp = read.table( file = 'Experiment_log.csv', sep = ',', header = T )
      # Extract header
      # if (length(Header)==0) Header = tmp[1]
      # Extract individual logs
      logs = rbind( logs, tmp )
      
      cat( 'Done', '\n' )
      
    } else {
      cat( 'Empty', '\n' )
    }
  } else {
    cat('Off', '\n')
  }
  
}

write.table( logs, file = paste(destinationPath,'\\','Experiment_log.csv',sep=''),sep=',',row.names = F, col.names = T, quote = F )