#---------------------------#
# Analyses of gabor priming #
# Kevin Potter              #
# Updated 03/09/2016        #
#---------------------------#

# Clear workspace
rm(list = ls())

# Load in useful functions
library(Rcpp)
library( utilityf )
# library(devtools)
# install_github("rettopnivek/seqmodels")
library( seqmodels )

# Set working directory
# setwd("~/Experiments/Gabor_priming_2016/Subjects")
setwd("~/Experiments/Gabor_priming_2016/Pilot")
subjDir = getwd();

subjNum = 4

# Read in data
fname = paste('Subject_',subjNum,'.csv',sep='')
fname2 = paste('Subject_',subjNum,'.mat',sep='')
dat.gabor = read.table( fname,header=T,sep=',')

# Adjust response times by target presentation time
dat.gabor$RT = dat.gabor$RT + .2;

# Data cleaning
setwd("~/Experiments/Gabor_priming_2016/Analyses")
analysisDir = getwd();

source("Data_cleaning_script.R")

# Extract priming durations
primeTimes = sort( unique( dat.sel$PrimeDuration ) )

# Return to folder with subject data
setwd( subjDir)

### Save plots to a PDF ###
pdf( file = paste('Pilot_priming_S',subjNum,'.pdf',sep=''),
     width = 12, height = 6 )

# Define code to control which parts are run

runCode = c(
  T,  # Measurement model based on adaptive block
  T,  # Plot of accuracy and response times
  T,  # Model fit to RTs and choice with ddiff
  F ) # Model fit to RTs and choice with dwaldrace

###
### Fit a two-parameter logistic model
### to the adaptive blocks
###

if (runCode[1]) {
  
  # Functions for the likelihood
  f.alpha.beta = function(x,alpha,beta) {
    theta = .5 + .5/(1+exp(-alpha*(x+beta)))
    theta
  }
  
  g.alpha.beta = function(theta,alpha,beta) {
    x = log( .5/(theta-.5) - 1 )/(-alpha) - beta
    x
  }
  
  L = function(alpha,beta,y,x, ln = 0 ) {
    out = y*f.alpha.beta(x,alpha,beta) + 
      (1-y)*(1-f.alpha.beta(x,alpha,beta))
    out[ is.na(out) ] = 0
    if (alpha <= 0.0 ) out = 0
    if (ln == 1 ) out = log( out )
    out
  }
  
  # Function to generate random parameter values
  st.f = function() {
    runif(2, c( .001, 0 ), c( 6, 6 ) )
  }
  
  ### Curve predicted from adaptive trials ###
  
  # Contrast levels
  x_lev = log( c(.005, 1) )
  x = seq( x_lev[1], x_lev[2], length = 100 )
  
  # Read in Matlab file for subject to get initial posterior estimates
  
  # install.packages('R.matlab')
  library(R.matlab)
  mat = readMat(fname2)
  
  # Plot the curve predicted by the adaptive section
  alpha.adaptive = mat$alpha.grid[ which( mat$alpha.prior == max(mat$alpha.prior) ),1]
  beta.adaptive = mat$beta.grid[ 1, which( mat$beta.prior == max(mat$beta.prior) ) ]
  
  # x11( width = 12, height = 6 )
  xl = c( exp( min(x_lev) ), exp( max(x_lev) )-.001 )
  plot( log( xl ), c(0,1), type ='n', xlab = 'Relative difference',
        ylab = 'Accuracy', bty = 'n', xaxt = 'n', yaxt ='n',
        main = paste('Subject',subjNum) )
  abline( v = log(xl[1]) ); abline( h = 0 )
  axis(2, seq(0,1,.2), tick = F, line = -.5 )
  axis(1, seq(-7,0,1), round( exp(seq(-7,0,1)), 2 ), tick = F, line = -.5 )
  
  lines( x, f.alpha.beta( x, alpha.adaptive, beta.adaptive ),
         lwd = 2, col = 'red' )
  
  CI.a = matrix( NA, length(x), 2 )
  ppc.a = matrix(NA, length(x), 2 )
  
  j.p = as.vector( exp( mat$posterior )/sum( exp( mat$posterior ) ) )
  prb = cumsum( j.p )
  a.v = as.vector( mat$alpha.grid )
  b.v = as.vector( mat$beta.grid )
  
  f.1 = function( u ) {
    sel = max( which( prb <= u ) )
    c( a.v[ sel ], b.v[ sel ] )
  }
  
  tst = apply( cbind( runif(4000) ), 1, f.1 )
  
  # Credible intervals
  for ( xn in 1:length(x) ) {
    
    prb = f.alpha.beta( x[xn], tst[1,], tst[2,] )
    CI.a[xn, c(1,2) ] = hdi( prb )
    
    ppc.a[xn,1] = qbinom( .025, 40, CI.a[xn,1] )
    ppc.a[xn,2] = qbinom( .975, 40, CI.a[xn,2] )
    
  }
  
  polygon( c( x, rev(x) ), c( CI.a[,1], rev( CI.a[,2] ) ),
           border = NA, col = rgb( .8, 0, 0, .2 ) )
  
  lines( x, ppc.a[,1]/40, lty = 2, col = 'red' )
  lines( x, ppc.a[,2]/40, lty = 2, col = 'red' )
  
  segments( log(xl[1]), .7, 
            g.alpha.beta( .7, alpha.adaptive, beta.adaptive ),
            .7, lty = 2 )
  segments( g.alpha.beta( .7, alpha.adaptive, beta.adaptive ), 0,
            g.alpha.beta( .7, alpha.adaptive, beta.adaptive ), .7,
            lty = 2 )
  
}

###
### Priming results for Accuracy and RTs ###
###

if (runCode[2]) {
  
  # x11(width = 12, height = 6 )
  layout( cbind(1,2) )
  
  # Accuracy
  ac = aggregate( dat.sel$Accuracy, list(
    dat.sel$PrimeDuration,
    dat.sel$Target==dat.sel$Prime ), mean )
  colnames( ac ) = c('Duration','Prime','P')
  
  plot( lowerUpper(1, log( ac$Duration ) ),
        lowerUpper( .1, ac$P ), type = 'n',
        xlab = 'Duration (ms)', ylab = 'Accuracy',
        xaxt = 'n', bty = 'n')
  axis(1, log( unique(ac$Duration) ),
       unique(ac$Duration) )
  
  # Target was primed
  lines( log( ac$Duration[ ac$Prime==T ] ),
         ac$P[ ac$Prime==T ], lwd = 2, col = 'green' )
  points( log( ac$Duration[ ac$Prime==T ] ),
          ac$P[ ac$Prime==T ], pch = 19, col = 'green' )
  
  # Target was not primed
  lines( log( ac$Duration[ ac$Prime==F ] ),
         ac$P[ ac$Prime==F ], lwd = 2, col = 'red' )
  points( log( ac$Duration[ ac$Prime==F ] ),
          ac$P[ ac$Prime==F ], pch = 19, col = 'red' )
  
  legend( 'topleft', c( 'Primed', 'Unprimed' ),
          fill = c( 'green', 'red' ), bty = 'n' )
  
  
  rt = aggregate( dat.sel$RT, list(
    dat.sel$PrimeDuration,
    dat.sel$Target==dat.sel$Prime,
    dat.sel$Accuracy ), median )
  colnames( rt ) = c('Duration','Prime','Accuracy','RT')
  
  plot( lowerUpper(1, log( rt$Duration ) ),
        lowerUpper( .2, rt$RT ), type = 'n',
        xlab = 'Duration (ms)', ylab = 'RT',
        xaxt = 'n', bty = 'n')
  axis(1, log( unique(rt$Duration) ),
       unique(rt$Duration) )
  
  # Target was primed
  lines( log( rt$Duration[ rt$Prime==T & rt$Accuracy == 1 ] ),
         rt$RT[ rt$Prime==T & rt$Accuracy == 1 ], lwd = 2, col = 'green' )
  points( log( rt$Duration[ rt$Prime==T & rt$Accuracy == 1 ] ),
          rt$RT[ rt$Prime==T & rt$Accuracy == 1 ], pch = 19, col = 'green' )
  
  # Target was not primed
  lines( log( rt$Duration[ rt$Prime==F & rt$Accuracy == 1 ] ),
         rt$RT[ rt$Prime==F & rt$Accuracy == 1 ], lwd = 2, col = 'red' )
  points( log( rt$Duration[ rt$Prime==F & rt$Accuracy == 1 ] ),
          rt$RT[ rt$Prime==F & rt$Accuracy == 1 ], pch = 19, col = 'red' )
  
  # Target was primed
  lines( log( rt$Duration[ rt$Prime==T & rt$Accuracy == 0 ] ),
         rt$RT[ rt$Prime==T & rt$Accuracy == 0 ], lwd = 2, lty = 2, col = 'green' )
  points( log( rt$Duration[ rt$Prime==T & rt$Accuracy == 0 ] ),
          rt$RT[ rt$Prime==T & rt$Accuracy == 0 ], pch = 4, col = 'green' )
  
  # Target was not primed
  lines( log( rt$Duration[ rt$Prime==F & rt$Accuracy == 0 ] ),
         rt$RT[ rt$Prime==F & rt$Accuracy == 0 ], lwd = 2, lty = 2, col = 'red' )
  points( log( rt$Duration[ rt$Prime==F & rt$Accuracy == 0 ] ),
          rt$RT[ rt$Prime==F & rt$Accuracy == 0 ], pch = 4, col = 'red' )
  
  
  legend( 'topleft', c( 'Primed', 'Unprimed' ),
          fill = c( 'green', 'red' ), bty = 'n' )
  
  mtext( paste('Subject',subjNum), side=3, cex=1.5, outer = T,
         line = -1.5 )
   
}

###
### Model estimation ###
###

# Useful functions for estimation
cov.create = function(mat) {
  # Purpose:
  # Creates a single variable with a set of unique levels based on 
  # a set of covariates, the number of all possible combinations for each 
  # of the covariates
  # Arguments:
  # mat - A matrix of the covariates of interest
  # Returns:
  # A vector specifying the corresponding combination level for each
  # observation
  
  # Determine the different levels for each covariate
  lstLevel = lapply(as.data.frame(mat),unique)
  
  # Determine the possible combinations of the different covariates
  unq = expand.grid(lstLevel)
  Levels = 1:nrow(unq)
  
  # Output variable
  out = numeric( nrow(mat) )
  
  for ( k in Levels ) {
    
    for ( n in 1:nrow(mat) ) {
      if ( sum(mat[n,] == unq[k,])==ncol(mat) ) out[n] = k
    }
  }
  
  out
}

# Define the coefEst function
cppFunction('
NumericMatrix coefEst( arma::mat X, 
                       NumericVector param, 
                       IntegerMatrix nCov, 
                       IntegerVector parSel) {
  // Purpose:
  // Allows the efficient computation of the set 
  // of parameter values per trial via matrix algebra 
  // Arguments:
  // X      - a N by K design matrix
  // param  - a vector of M coefficients
  // nCov   - a 2 by P matrix giving the indices
  // parSel - A vector of K indices indicating which 
  //          of the M coefficients correspond to the 
  //          kth column of X (K >= M)
  // Returns:
  // A N x P matrix of parameters
  
  int N = X.n_rows;
  int K = X.n_cols;
  int P = nCov.ncol();
  
  // Define an output matrix
  arma::mat out(N,P);
  
  // Define a arma matrix for the parameters
  arma::mat pm(K,P);
  pm.fill(0.0);
  
  for (int p = 0; p < P; p++) {
    for (int ps = nCov(0,p)-1; ps < nCov(1,p); ps++) {
      pm(ps,p) = param( parSel(ps)-1 );
    }
  }
  
  out = X*pm;
  
  return( wrap(out) );
}', depends = "RcppArmadillo")


### Estimation of a Wiener process ###

if (runCode[3]) {
  
  ### Data
  rt = dat.sel$RT
  ch = dat.sel$Choice
  minRT = min(rt)
  
  #### Covariates
  Correct = dat.sel$Target;
  Correct[ Correct == 0 ] = -1 # Flip for drift rate
  Congruency = as.numeric( dat.sel$Prime == dat.sel$Target )
  Duration = dat.sel$PrimeDuration
  Intercept = rep( 1, nrow( dat.sel ) )
  # Collapse prime times and type
  PrimexDuration = cov.create( cbind( Congruency, Duration ) )
  # Determine the ordering of the levels for the 
  # collapsed covaraites
  Levels = aggregate( PrimexDuration, 
                      list( Congruency, Duration ), unique )$x
  # Create a design matrix for the covariates of interest
  X.PxD = designCoding(PrimexDuration,
                       Levels = Levels, 
                       type='Intercept')
  X.D = designCoding(Duration,
                     Levels = primeTimes, 
                     type='Intercept')
  
  # Parameters
  # alpha.D1     - threshold for each condition
  # alpha.D2
  # alpha.D3
  # alpha.D4
  # theta.D1FP   - starting point for each condition
  # theta.D1TP
  # theta.D2FP
  # theta.D2FP
  # theta.D3FP
  # theta.D3FP
  # theta.D4FP
  # theta.D4FP
  # xi.D1FP      - drift rate for each condition
  # xi.D1TP
  # xi.D2FP
  # xi.D2TP
  # xi.D3FP
  # xi.D3TP
  # xi.D4FP
  # xi.D4TP
  # tau          - residual latency
  
  # Labels for parameters
  cnd = paste( rep( c('FP','TP'), 4 ),
               rep( primeTimes, each = 2 ),
               sep = '' )
  parNames = c( paste( 'alpha',primeTimes,sep='.'),
                paste( 'theta', cnd, sep = '.' ),
                paste( 'xi', cnd, sep = '.' ),
                'tau' )
  
  ### Define variables for coefficient estimation
  
  X = cbind( 
    X.D, # alpha
    X.PxD, # theta
    X.PxD*Correct, # xi
    Intercept # tau
  )
  
  # Col 1 - 4: alpha
  # Col 5 - 12: theta
  # Col 13 - 20: xi
  # Col 21: tau
  nCov = cbind( c(1,4), c(5,12), c(13,20), c(21,21) )
  parSel = 1:21
  parType = c( rep(2,4), rep(3,8), rep(1,8), 4 )
  
  ### Define functions for model fitting
  
  tran.par = function( par, reverse = F ) {
    # Purpose:
    # Transforms parameters values for easier estimation
    # Arguments:
    # par     - a vector of parameters
    # parType - a vector indicating what types of transformations 
    #           should be applied
    # reverse - Indicates whether the transformation should be 
    #           reversed or not.
    # Returns:
    # A vector of transformed parameters
    
    if (reverse) {
      
      par[ parType == 2 ] = log( par[ parType== 2 ] )
      par[ parType == 3 ] = logit( par[ parType== 3 ] )
      par[ parType == 4 ] = logit( par[ parType== 4 ]/minRT )
      
    } else {
      
      par[ parType == 2 ] = exp( par[ parType== 2 ] )
      par[ parType == 3 ] = logistic( par[ parType== 3 ] )
      par[ parType == 4 ] = logistic( par[ parType== 4 ] )*minRT
      
    }
    
    par
  }
  
  st.f = function() {
    # Purpose:
    # Generates random starting values
    # Arguments:
    # parType - a vector indicating what types of transformations 
    #           should be applied
    # Returns:
    # A vector of raw parameter values
    
    tmp = runif( length(parType),
                 c( rep(.8,4), rep(.3,8), rep(0,8), .1*minRT ),
                 c( rep(1.6,4), rep(.7,8), rep(3,8), .9*minRT ) )
    tran.par( tmp, reverse = T )
  }
  
  # coef = coefEst( X, st.f(parType,min(rt)), nCov, parSel )
  
  mle.f = function( par, dat ) {
    # Purpose:
    # Calculates the log-likelihood for the data given a set of parameters
    # Arguments:
    # Forthcoming
    # Returns:
    # Forthcoming
    
    # Extract data
    rt = dat[,1]; ch = dat[,2];
    
    # Transform parameters
    par = tran.par( par )
    
    # Determine coefficients
    coef = coefEst( X, par, nCov, parSel )
    
    # Calculate log-likelihood
    logLik = ddiff( rt, ch, coef[,1], coef[,2], coef[,3], coef[,4],
                    ln = 1 )
    out = sum( logLik )
    if (is.na( out ) ) out = -Inf
    
    out
  }
  
  # Fit the model
  results = MLE( cbind(rt,ch), st.f, mle.f, nRep = 4, 
                 unDef = -Inf, hessian = T, 
                 control = list( fnscale = -1, 
                                 maxit = 10000 ) )
  # Extract best fitting parameters
  sel = which( results$logLikSum == max( results$logLikSum ) )
  best.par = results$MLE[[ sel ]]
  # Convert parameters to mean and standard deviation
  est = best.par$par; est = tran.par( est )
  # Calculate the standard errors and confidence intervals
  parVar = mleSE( best.par, fnscale = -1 )
  # print( round( est, 2 ) )
  print( results$run.time )
  
  # print( round( tran.par( parVar$CI[1,] ), 2 ) )
  # print( round( tran.par( parVar$CI[2,] ), 2 ) )
  
  library(data.table)
  DT = data.table( Parameter = parNames,
                   LowerCI = round( tran.par( parVar$CI[1,] ), 2 ),
                   Estimates = round( est, 2 ),
                   UpperCI = round( tran.par( parVar$CI[2,] ), 2 ) )
  print( DT )
  
  ### Plot results ###
  
  parCnd = cbind( rep(1:4,each=2), # alpha
                  5:12, # theta
                  13:20, # xi
                  rep(21,8) # tau
  )
  
  # Fit to raw data
  # x11( width = 14, height = 7 );
  layout( rbind( seq(2,8,2), seq(1,7,2) ) )
  inc = 2
  for ( cnd in 1:8 ) {
    sel = dat.sel$Accuracy == 1 & PrimexDuration==Levels[cnd]
    curRT = rt[ sel ]
    curAc = mean( dat.sel$Accuracy[ PrimexDuration == Levels[cnd] ] )
    plot( sort( curRT ), 
          ( 1:length( curRT )/length( curRT ) )*curAc,
          ylim = c(0,1), type = 'l', bty = 'l',
          ylab = 'Joint CDF', xlab = 'Time (s)',
          xlim = c(.2,2.2), xaxt = 'n' )
    if ( cnd == inc ) {
      mtext( paste(primeTimes[inc/2],'ms'), side = 3, line = -1 )
      inc = inc + 2
    }
    axis(1,seq(.2,2.2,.25))
    
    val = seq(.2,2.2,length=1000)
    xa = val[ ( val >= min(curRT) ) & ( val <= max(curRT) ) ]
    if ( length(xa) > 0 ) {
      lines( xa, 
             pdiff( val[ ( val >= min(curRT) ) & ( val <= max(curRT) ) ], 1, 
                    est[ parCnd[cnd,1] ],
                    est[ parCnd[cnd,2] ], 
                    est[ parCnd[cnd,3] ], 
                    est[ parCnd[cnd,4] ] ),
             col = 'blue' )
    }
    
    sel = dat.sel$Accuracy == 0 & PrimexDuration==Levels[cnd]
    curRT = rt[ sel ]
    curAc = 1 - mean( dat.sel$Accuracy[ PrimexDuration == Levels[cnd] ] )
    lines( sort( curRT ), 
           ( 1:length( curRT )/length( curRT ) )*curAc, lty = 2 )
    
    xa = val[ ( val >= min(curRT) ) & ( val <= max(curRT) ) ]
    if ( length(xa) > 0 ) {
      lines( xa, 
             pdiff( val[ ( val >= min(curRT) ) & ( val <= max(curRT) ) ], 0, 
                    est[ parCnd[cnd,1] ],
                    est[ parCnd[cnd,2] ], 
                    est[ parCnd[cnd,3] ], 
                    est[ parCnd[cnd,4] ] ),
             col = 'blue', lty = 2 )
    }
  }
  
  # Parameter estimates
  # x11( width = 14, height = 7 );
  layout( cbind( c(1,3), c(2,4) ) )
  pltRng = c(.2,.2,.2,.05)
  
  parGrep = c('alpha','theta','xi','tau')
  pltLabels = c(expression(alpha),expression(theta),
                expression(xi),expression(tau))
  for (i in 1:4) {
    sel = grep(parGrep[i],DT$Parameter)
    prm = cbind( DT$LowerCI[sel],
                 DT$Estimates[sel],
                 DT$UpperCI[sel] )
    xl = c( 1 - .5, nrow(prm) + .5 )
    if ( nrow(prm) > 4 ) {
      xl = c( .5, 4.5 )
    }
    yl = lowerUpper( pltRng[i], as.vector( prm[,-2] ) )
    plot( xl, yl, type='n',las=1,bty='n',xaxt='n',
          ylab = pltLabels[i] )
    if (xl[2]==4.5) axis(1,1:4,c(17,50,400,1000))
    if ( nrow(prm) <= 4 ) {
      
      if ( nrow(prm) > 1 ) {
        lines( 1:nrow(prm), prm[,2] )
      }
      points( 1:nrow(prm), prm[,2], pch = 19 )
      segments( 1:nrow(prm), prm[,1],
                1:nrow(prm), prm[,3] )
    } else {
      
      # Foil primed
      sel = c(1,3,5,7)
      lines( 1:4, prm[sel,2], lty = 2 )
      points(1:4, prm[sel,2], pch = 15 )
      segments(1:4,prm[sel,1],
               1:4,prm[sel,3])
      
      # Target primed
      sel = c(2,4,6,8)
      lines( 1:4, prm[sel,2], lty = 1 )
      points(1:4, prm[sel,2], pch = 19 )
      segments(1:4,prm[sel,1],
               1:4,prm[sel,3])
      
      legend('topleft',c('Primed','Unprimed'),
             lty = c(1,2), pch = c(19,15), bty = 'n' )
      
    }
  }

}

if (runCode[4]) {
  
  ### Data
  rt = dat.sel$RT
  ch = dat.sel$Choice
  minRT = min(rt)
  
  #### Covariates
  Intercept = rep( 1, nrow( dat.sel ) )
  Correct = dat.sel$Target; # The position of the correct choice
  Congruency = as.numeric( dat.sel$Prime == dat.sel$Target ) # Which element was primed
  Duration = dat.sel$PrimeDuration # Prime duration
  PrimexDuration = cov.create( cbind( Congruency, Duration ) )
  X.D = designCoding(PrimexDuration, Levels = c(5,6,3,4,7,8,1,2),type='Intercept')
  
  # X.D = designCoding(Duration,type='Intercept')
  
  # X.xi1 = Co
  
  "
  xi.TP.17
  xi.TU.17
  xi.FU.17
  xi.FP.17
  
  xi.TP.50
  xi.TU.50
  xi.FU.50
  xi.FP.50
  
  xi.TP.400
  xi.TU.400
  xi.FU.400
  xi.FP.400
  
  xi.TP.1000
  xi.TU.1000
  xi.FU.1000
  xi.FP.1000
  "
  
  # Parameters
  # kappa.0  - threshold for left
  # kappa.1  - threshold for right
  # xi.17F   - Drift rates for different conditions
  # xi.17T   - 
  # xi.50F   - 
  # xi.50T   - 
  # xi.400T  - 
  # xi.400F  - 
  # xi.1000F - 
  # xi.1000T - 
  # tau   - residual latency
  # parNames = c('alpha','theta','xi.400F','xi.400TP','xi.50FP','xi.50TP',
  #              'xi.17FP','xi.17TP','xi.1000FP','xi.1000TP','tau')
  
  
  ### Define variables for coefficient estimation
  
  X = cbind( 
    Intercept, # kappa.1
    Intercept, # kappa.0
    X.D*Correct, # xi.1 Right
    X.D*(1 - Correct), # xi.1 Left
    X.D*(1 - Correct), # xi.0 Right
    X.D*(Correct), # xi.0 Left
    Intercept # tau
  )
  
  # Col 1: kappa.1
  # Col 2: kappa.0
  # Col 3 - 18: xi.1
  # Col 19 - 34: xi.0
  # Col 35: tau
  nCov = cbind( c(1,1), c(2,2), c(3,18), c(19,34), c(35,35) )
  parSel = c( 1:2, c(3:10,11:18), c(11:18,3:10), 19 )
  parType = c( 2, 2, rep(2,8), rep(2,8), 4 )
  
  ### Define functions for model fitting
  
  tran.par = function( par, reverse = F ) {
    # Purpose:
    # Transforms parameters values for easier estimation
    # Arguments:
    # par     - a vector of parameters
    # parType - a vector indicating what types of transformations 
    #           should be applied
    # reverse - Indicates whether the transformation should be 
    #           reversed or not.
    # Returns:
    # A vector of transformed parameters
    
    if (reverse) {
      
      par[ parType == 2 ] = log( par[ parType== 2 ] )
      par[ parType == 3 ] = logit( par[ parType== 3 ] )
      par[ parType == 4 ] = logit( par[ parType== 4 ]/minRT )
      
    } else {
      
      par[ parType == 2 ] = exp( par[ parType== 2 ] )
      par[ parType == 3 ] = logistic( par[ parType== 3 ] )
      par[ parType == 4 ] = logistic( par[ parType== 4 ] )*minRT
      
    }
    
    par
  }
  
  st.f = function() {
    # Purpose:
    # Generates random starting values
    # Arguments:
    # parType - a vector indicating what types of transformations 
    #           should be applied
    # Returns:
    # A vector of raw parameter values
    
    tmp = runif( length(parType),
                 c( .5, .5, rep(.5,8), rep(.5,8), .1*minRT ),
                 c( 3, 3, rep(3,8), rep(3,8), .9*minRT ) )
    tran.par( tmp, reverse = T )
  }
  
  mle.f = function( par, dat ) {
    # Purpose:
    # Calculates the log-likelihood for the data given a set of parameters
    # Arguments:
    # Forthcoming
    # Returns:
    # Forthcoming
    
    # Extract data
    rt = dat[,1]; ch = dat[,2];
    
    # Transform parameters
    par = tran.par( par )
    
    # Determine coefficients
    coef = coefEst( X, par, nCov, parSel )
    
    # Calculate log-likelihood
    logLik = dwaldrace( rt, ch, coef[,1], coef[,3], coef[,5],
                                coef[,2], coef[,4], coef[,5], ln = 1 )
    out = sum( logLik )
    if (is.na( out ) ) out = -Inf
    
    out
  }
  
  # Fit the model
  results = MLE( cbind(rt,ch), st.f, mle.f, nRep = 4, 
                 unDef = -Inf, hessian = T, 
                 control = list( fnscale = -1, 
                                 maxit = 10000 ) )
  # Extract best fitting parameters
  sel = which( results$logLikSum == max( results$logLikSum ) )
  best.par = results$MLE[[ sel ]]
  # Convert parameters to mean and standard deviation
  est = best.par$par; est = tran.par( est )
  # Calculate the standard errors and confidence intervals
  parVar = mleSE( best.par, fnscale = -1 )
  # print(c('a','w','v1','v2','v3','v4','v5','v6','v7','v8','rl'))
  # print( round( est, 2 ) )
}

dev.off()