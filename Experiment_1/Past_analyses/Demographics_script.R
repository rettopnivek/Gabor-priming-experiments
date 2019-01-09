
# Current working directory
orig_dir = getwd()

# Change to folder with subject data
setwd('..') # Move up one folder
setwd('Subjects')

# Extract file names
allFiles = dir()

# Loop over all files
SubjNum = c()
Gender = c()
Race = c()
for (i in 1:length(allFiles)) {
  
  fname = allFiles[i]
  tmp = strsplit( fname, '_' )[[1]]
  if ( tmp[1] == 'Subject' ) {
    if ( length( tmp ) == 3 ) {
      SubjNum = c( SubjNum, as.numeric( tmp[2] ) )
      
      tmp3 = scan( file = fname, what = 'character', sep ='\n' )
      Gender = c( Gender, strsplit( tmp3[1], split = ', ' )[[1]][2] )
      Race = c( Race, strsplit( tmp3[3], split = ', ' )[[1]][2] )
    }
  }
}

# Clean up workspace
rm( i, fname, tmp, tmp3, allFiles )

# Return to original directory
setwd( orig_dir )

# Save demographic info
demographics = as.data.frame( cbind( SubjNum ) )
colnames( demographics ) = 'Subjects'
demographics$Gender = as.factor( Gender )
demographics$Race = as.factor( Race )
save( demographics, file = 'demographics.RData' )
