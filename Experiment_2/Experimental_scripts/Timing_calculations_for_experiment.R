#--------------#
# Study timing #
#--------------#

library(seqmodels)

# Number of durations
nD = 3
# Number of possible prime/target combinations
nC = 4

# Practice Blocks and Trials
nP = 1
nPT = 96

# Adaptive blocks and Trials
nA = 3
nATlong = 84
nATshort = 36

# Experimental blocks and Trials
nE = 6
DxC = nD*nC; # Number of conditions to test per block
nT = 6;
nET = DxC*nT
cat( c('Trials per block: ', nET, '\n' ) )
cat( c('Trials per condition: ', nET*nE/DxC, '\n' ) )

# Total number of trials
trialTotal = nPT + nATlong + (nA-1)*nATshort + 
  nE*nET

# Trial length
trialLength = 2000 # ms

nRep = 1000
EL = numeric(nRep)
for (e in 1:nRep) {
  # Descriptive model of RT
  RTguess = rinvgauss( trialTotal, 1.673, 3.310, 1 ) + .25
  tmp = runif(trialTotal, 0, 5 )
  u = runif(trialTotal)
  a = .94
  RTguess[ u > a ] = tmp[ u > a ]
  
  # Experiment length in minutes
  ExpLength = sum( ((trialLength + RTguess)/1000)/60 )
  EL[e] = ExpLength
}
print( max(EL) )
