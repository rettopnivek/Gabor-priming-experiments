#---------------------------#
# Sequential sampling model #
# using MLE                 #
# Kevin Potter              #
# 04/19/2016                #
#---------------------------#

### Create a meta-subject ###

# Extract conditions
cnd = aggregate( rep(1,nrow(curData)), list(
  curData$PrimeDuration,
  curData$Prime == curData$Target,
  curData$Target ),
  sum )
colnames( cnd ) = c( 'Duration','Prime','Target','Trials')
Subjects = subj_ind_create(curData)
Drt = curData$PrimeDuration
Prm = curData$Prime == curData$Target
Crr = curData$Target

# Simulate from meta-subject data using aggregate empirical CDFs

Nsim = 22 # Number of trials for each simulated condition

# Create a progress bar using a base R function
pb = txtProgressBar( min = 0, max = nrow(cnd), style = 3 )

subjMeta = c()

# Loop through conditions
for (i in 1:nrow(cnd)) {
  
  # Select condition
  sel = Drt == cnd[i,'Duration'] & 
    Prm == cnd[i,'Prime'] & 
    Crr == cnd[i,'Target']
  
  # Simulate data from meta-subject
  sim = meta_subject( curData$RT[sel], 
                      curData$Choice[sel], 
                      Subjects[sel], 
                      lngth = Nsim )
  subjMeta = rbind( subjMeta,
                    cbind( sim, 
                           cnd[i,'Duration'],
                           cnd[i,'Prime'],
                           cnd[i,'Target'] ) )
  
  # Update the progress bar
  setTxtProgressBar(pb,i)
}
close(pb)

colnames( subjMeta ) = c('RT','Choice','Duration','Prime','Target')
subjMeta = as.data.frame( subjMeta )
subjMeta$Accuracy = subjMeta$Choice == subjMeta$Target

subjMeta = na.omit( subjMeta )

# Clean up workspace
rm( cnd, Nsim, Drt, Prm, Crr, i, Subjects )

### Likelihood function ###

# Useful packages
library( seqmodels )
library( maxLik )
library( msm )

# kappa_prime (short|long)
# kappa_unprime (short|long)


# Coefficients
# kappaTarget -> 2
# xiTarget    -> 10
# kappaFoil   -> 2
# xiFoil      -> 10
# tau         -> 1

# Dur = curData$PrimeDuration
# DurBin = as.numeric( curData$PrimeDuration < 100 )
# Pri = as.numeric( curData$Prime == curData$Target )
# Tar = curData$Target
# Int = rep(1,nrow(curData))

Dur = subjMeta$Duration
DurBin = as.numeric( Dur < 100 )
Pri = as.numeric( subjMeta$Prime )
Tar = subjMeta$Target
PriSide = Tar
PriSide[ Pri == 0 ] = 1 - Tar[ Pri == 0 ]
Int = rep(1,nrow(subjMeta))

X_k = designCoding( DurBin, Levels=0:1, type = 'Intercept' )
cvrt = cov_create( cbind( Dur, Pri ) )
Levels = aggregate(cvrt,list(Dur,Pri),unique)$x
X_x = designCoding( cvrt, Levels = Levels, type = 'Intercept' )
rm( cvrt, Levels )

# Design matrix
X = cbind( X_k*PriSide, X_k*(1-PriSide), X_x*Tar, X_x*(1-Tar), # Right
           X_k*(1-PriSide), X_k*PriSide, X_x*(1-Tar), X_x*Tar, # Left
           Int )
X = t(X)

# Create small design matrix for later plotting etc...
cnd = aggregate( t(X), list(
  Dur, Pri, Tar ), unique )
X_small = t( as.matrix( cnd[,-(1:3)] ) )

# Clean up workspace
rm( Dur, DurBin, Pri, Tar, Int, X_k, X_x )

# Coefficients
# k_sh_1 k_l_1 k_sh_0 k_l_0 (1:4)
# xi_T_FP [ 7 - 563 ] xi_T_TP [7 - 563 ] (5:14)
# xi_F_FP [ 7 - 563 ] xi_F_TP [7 - 563 ] (15:24)
# tau (25)

# Create index for linear algebra function
Clm = c( 1:4, 5:24, 1:4+24, 5:24+24, 25+24 )
Rws = c( rep(1,4),
         rep(2, length( 5:24 ) ),
         rep(3,4),
         rep(4, length( 5:24 ) ),
         5 )
# Create index for parameter selection
parSel = c( 1:24, 1:24, 25 )

index = cbind( Rws, Clm )
rm( Clm, Rws )
index = rbind( index, c(5,length(parSel)) )

# Check design matrix assignment
# coef = 1:25
# tst = param_est( X, coef, NULL, index, parSel )
# head( t(tst) )

# Define likelihood function
logLik = function( coef, dat, design, priors = NULL ) {
  
  # Extract data
  t = dat[,1]
  y = dat[,2]
  
  # Transform coefficients
  coef = exp( coef ); # Restrict to be positive
  
  # Determine parameters
  par = param_est( design$X, coef, NULL, design$index, design$parSel )
  
  # Sum of the log-likelihood
  logLik = sum( dwaldrace( t, y, par[1,], par[2,], par[5,],
                      par[3,], par[4,], par[5,], ln = 1 ) )
  if (is.na( logLik ) ) logLik = -Inf
  
  # Priors
  if ( length( priors ) > 0 ) {
    prs = sum( dtnorm( coef, priors[,1], priors[,2], lower=0.0, log = T ) )
  } else {
    prs = 1
  }
  
  return( logLik + prs )
}

# Define function to generate starting values
stF = function( minVal = .2 ) {
  
  startVal = runif(
    25, # Number of parameters
    c( rep(.5,4), rep(.5,20), 0 ), # Lower boundary
    c( rep(2,4), rep(3,20), minVal )  # Upper boundary
  )
  # Transform values
  startVal = log( startVal )
  
  startVal
}

# Function for expected value
#Eval = function( t, par ) {
#  t*dwaldrace( t, 1, par[1], par[2], par[5], par[3], par[4], par[5] )
#}

### Fit meta-subject ###

# Define priors
priors = cbind(
  c( rep(1,4), rep(1.8,10), rep(1,10), .1 ),
  c( rep(.5,4), rep(.5,10), rep(1,10), .025 )
)

# Data
dat = cbind( t = subjMeta$RT, y = subjMeta$Choice )

# Variables for design
design = list( X = X,
               index = index,
               parSel = parSel )

# Generate starting value
chk = -Inf
inc = 1
while (chk==-Inf & inc <= 20) {
  startVal = stF(minVal = min( dat[,1] ) )
  chk = logLik( startVal, dat, design, priors = priors )
  inc = inc + 1
}

# Estimate coefficients using MLE
results = maxLik( logLik, dat = dat, design = design, priors = priors,
                  start = startVal, method = 'NM',
                  control = list(iterlim = 50000 ) )

### Extract model predictions across conditions ###

tmp = exp( coef( results ) ) # Best-fitting coefficients

# Parameters by condition
tst = param_est( X_small, tmp, NULL, design$index, design$parSel )

# Accuracy
pred_ac = pwaldrace( Inf, 1, 
                     tst[1,], tst[2,], tst[5,], 
                     tst[3,], tst[4,], tst[5,] )
# Quantiles
prb = c(.01,seq(.1,.9,.2),.99)
pred_qnt1 = matrix( NA, length( pred_ac ), length( prb ) )
pred_qnt0 = matrix( NA, length( pred_ac ), length( prb ) )
for ( i in 1:length( prb ) ) {
  pred_qnt1[,i] = qwaldrace( prb[i]*pred_ac, 1,
                             tst[1,], tst[2,], tst[5,], 
                             tst[3,], tst[4,], tst[5,] )
  pred_qnt0[,i] = qwaldrace( prb[i]*(1-pred_ac), 0,
                             tst[1,], tst[2,], tst[5,], 
                             tst[3,], tst[4,], tst[5,] )
}

### Plot observed vs. predicted by condition ###

datSel = subjMeta

# Extract RT quantiles
jntRT = aggregate( datSel$RT, list(
  datSel$Duration,
  datSel$Prime,
  datSel$Accuracy ),
  quantile, prob = c(0,seq(.1,.9,.2),1) )
colnames(jntRT)=c('Duration','Prime','Accuracy','Q')

# Extract accuracy
jntAC = aggregate( datSel$Accuracy, list(
  datSel$Duration,
  datSel$Prime ),
  mean )
colnames(jntAC)=c('Duration','Prime','P')

pdf('MLE_est_ex.pdf',width=14,height=7)

# Create joint CDF plots
# x11( width = 12, height = 6 )
layout( rbind( 1:5, 6:10 ) )

ttl = cbind(
  paste( jntAC$Duration, 'ms' ),
  rep( c('Foil','Target'), each = 5 ) )

drawCDF = function(qnt,prb,ac, pts = c(19,15), 
                   clr = c('black','black'), ... ) {
  
  points( qnt[2,], prb*ac, pch = pts[1], col = clr[1], ... )
  points( qnt[1,], prb*(1-ac), pch = pts[2], col = clr[2], ... )
  
}

for (i in 1:5) {
  
  if (i == 1) par( mar=c(2,5,2,1) ) else par( mar=c(2,2,2,1) )
  plot( c(0,1.5), c(0,1), type = 'n',
        bty = 'n', xlab = ' ', ylab = ' ',
        main = paste( 'Primed for', ttl[i,1] ) )
  
  sel = jntRT$Duration == jntAC$Duration[i] & 
    jntRT$Prime == jntAC$Prime[i]
  sel2 = cnd[,1] == jntAC$Duration[i] & 
    cnd[,2] == jntAC$Prime[i]
  
  prb = c(0, seq(.1,.9,.2), 1 )
  drawCDF( jntRT[sel,-(1:3)], prb, jntAC$P[i], cex = 1.5 )
  drawCDF( pred_qnt1[sel2,], prb, pred_ac[i+10], cex = 1.5,
           clr = c('blue','blue') )
  
  if (i==1) legend('topleft',c('Target racer','Foil racer'),
                   pch = c(19,15),bty='n')
  if (i==5) legend('bottomright',c('Observed','Predicted'),
                   fill=c('black','blue'),bty='n')
  if (i==5) legend('topright','Foil primed',bty='n')
}

for (i in 6:10) {
  
  if (i == 6) par( mar=c(3.5,5,.5,1) ) else par( mar=c(3.5,2,.5,1) )
  plot( c(0,1.5), c(0,1), type = 'n',
        bty = 'n', xlab = ' ', ylab = ' ' )
  
  sel = jntRT$Duration == jntAC$Duration[i] & 
    jntRT$Prime == jntAC$Prime[i]
  sel2 = cnd[,1] == jntAC$Duration[i] & 
    cnd[,2] == jntAC$Prime[i]
  
  prb = c(0, seq(.1,.9,.2), 1 )
  drawCDF( jntRT[sel,-(1:3)], prb, jntAC$P[i], cex = 1.5 )
  drawCDF( pred_qnt1[sel2,], prb, pred_ac[i+10], cex = 1.5,
           clr = c('blue','blue') )
  if (i==10) legend('topright','Target primed',bty='n')
}

mtext( 'RT (ms)', side = 1, outer = T, cex = 1, line = -1 )
mtext( 'Joint CDF', side = 2, outer = T, cex = 1, line = -2 )

### Plots of parameters ###
# x11(width = 12, height=6)
layout( cbind(1,2) )

par( mar = c( 3, 5, 3, 1 ) )
yl = lowerUpper( .5, tmp[5:24] )
blankPlot( c(.5,10.5), yl )
mtext('Drift rates',side=3,cex=1.5)
points( 1:10, tmp[5:14], pch = 19 )
points( 1:10, tmp[15:24], pch = 4 )
abline( v = 5.5 )
axis( 1, 1:10, rep( unique( cnd[,1] ), 2 ) )
axis( 2, seq(yl[1],yl[2],.2) )
legend( 'bottomleft', c('Target racer','Foil racer'),
        pch = c(19,4), bty = 'n' )
legend( 'topleft', 'Foil primed', bty = 'n' )
legend( 'topright', 'Target primed', bty = 'n' )

par( mar = c( 3, 5, 3, 1 ) )
yl = lowerUpper( .2, tmp[1:4] )
blankPlot( c(.5,2.5), yl )
mtext('Thresholds',side=3,cex=1.5)
points( 1:2, tmp[1:2], pch = 19 )
points( 1:2, tmp[3:4], pch = 4 )
axis( 1, 1:2, c('Short','Long') )
axis( 2, seq(yl[1],yl[2],.2) )
legend( 'bottomleft', c('Primed','Unprimed'),
        pch = c(19,4), bty = 'n' )

dev.off()