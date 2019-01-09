#------------------------------------------------#
# Mass file copying script for David Huber's lab #
# Kevin Potter                                   #
# Updated 02/18/2016                             #
#------------------------------------------------#

# Note that the source computers need to actually be on for this script to work

# Pathway to destination folder
destinationPath = 'C:\\Users\\lab\\Documents\\MATLAB\\Gabor_priming_2016-master\\Pilot'

# Computer names for remote computers with source folders
compNames = paste('CC',8:8,sep='')
compIndex = as.character(8:8)

# Source folder path on a given computer
folderPath = '\\Users\\lab\\Documents\\MATLAB\\Gabor_priming_2016\\Subjects'

for ( computerSelect in compIndex ) {
  
  # Select a computer
  i = which( compIndex == computerSelect )
  
  # Create the full path by concatenating the strings
  sourcePath = paste( '\\\\', compNames[i], folderPath, sep = '' )
  
  # Track progress
  cat( 'Computer ', compIndex[i], ' progress: ', '\n' )
  
  # Change working directory to source path
  # setwd(sourcePath)
  oldPath = tryCatch( setwd(sourcePath), warning = function(w) print( 'Computer is off!' ), 
            error = function(e) { 'Stop' } )
  if (oldPath != 'Stop' ) {
    nFiles = length( dir() )
    
    if (nFiles > 0 ) {
      
      # create progress bar
      pb <- txtProgressBar(min = 1, max = nFiles, style = 3)
      
      # Loop over all files in the folder
      for ( fileSelect in 1:length( dir() ) ) {
        # fileSelect = 1
        
        # The file's name
        currentFile = dir()[fileSelect]
        
        # Based on some metric, determine if the file should be copied
        # Here, I copy any file that starts with the phrase 'Subject'
        fileHead = strsplit( currentFile, '_' )[[1]] # Extract the first word in the filename
        if ( fileHead[1] == 'Subject' ) {
          OriginalFile = paste( sourcePath, '\\', currentFile, sep = '' )
          CopiedFile = paste( destinationPath, '\\', currentFile, sep = '' )
          
          # Copy the source file to the destination folder
          file.copy( OriginalFile, CopiedFile, overwrite = F, recursive = FALSE,
                     copy.mode = TRUE, copy.date = FALSE)
        }
        
        # update progress bar
        setTxtProgressBar(pb, fileSelect)
      }
      close(pb)
    } else {
      cat( 'Empty', '\n' )
    }
  } else {
    cat('Off', '\n')
  }
    
}