#--------------------------------------------------#
# Analyses of psychometric curve for gabor priming #
# Kevin Potter                                     #
# Updated 03/09/2016                               #
#--------------------------------------------------#

# Clear workspace
rm(list = ls())

# Load in useful functions
# library(devtools)
# install_github("rettopnivek/utilityf")
library( utilityf )
# install_github("rettopnivek/seqmodels")
library(seqmodels)

# Set working directory
setwd("~/Experiments/Gabor_priming_2016/Pilot")
# setwd("~/Experiments/Gabor_priming_2016/Subjects")
subjDir = getwd();

subjNum = 3

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

# Return to folder with subject data
setwd( subjDir)

### Plot PDF ###
pdf( file = paste( 'Pilot_psychometric_S', subjNum, '.pdf', sep='' ),
     width = 12, height = 6 )

###
### Psychometric estimation trials
###

dat.sel = dat.sel[ dat.sel$BlockType==2,]

# Plot data
acc = aggregate( dat.sel$Accuracy, list(dat.sel$Contrast), mean )
colnames( acc ) = c('C','P')

# x11();
xl = c( min( dat.sel$Contrast ), 1 )
plot( log( xl ), c(0,1), type ='n', xlab = 'Relative difference',
      ylab = 'Accuracy', bty = 'n', xaxt = 'n', yaxt ='n' )
abline( v = log(xl[1]) ); abline( h = 0 )
axis(2, seq(0,1,.2), tick = F, line = -.5 )
axis(1, seq(-7,0,1), round( exp(seq(-7,0,1)), 2 ), tick = F, line = -.5 )

points( log( acc$C ), acc$P, pch = 19 )

abline( h = .5, lty = 2 )

###
### Fit a two-parameter logistic model
###

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
x_lev = log( c( min( dat.sel$Contrast ), max( dat.sel$Contrast ) ) )
x = seq( x_lev[1], x_lev[2], length = 100 )

# Read in Matlab file for subject to get initial posterior estimates

# install.packages('R.matlab')
library(R.matlab)
mat = readMat(fname2)

# Plot the curve predicted by the adaptive section
alpha.adaptive = mat$alpha.grid[ which( mat$alpha.prior == max(mat$alpha.prior) ),1]
beta.adaptive = mat$beta.grid[ 1, which( mat$beta.prior == max(mat$beta.prior) ) ]

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
          g.alpha.beta( .7, alpha.adaptive, beta.adaptive ), 1,
          lty = 2 )

### Adpative Metropolis-Hastings ###

# install.packages('MHadaptive')
library(MHadaptive)

# Define the function for the likelihood and priors
li_func = function( par, dat, priors ) {
  
  # Extract data
  y = dat[,1]; x = dat[,2]
  
  # Extract parameters
  alpha = par[1]
  beta = par[2]
  
  # Truncated normal prior on alpha
  alpha.prior = dnorm( alpha, priors[1], priors[2], log=T)
  alpha.prior = alpha.prior - log(
    pnorm(Inf,priors[1], priors[2]) - 
      pnorm(0,priors[1], priors[2]) )
  
  # Normal prior on beta
  beta.prior = dnorm( beta, priors[3], priors[4], log = T )
  
  out = sum( L( alpha, beta, y, x, ln = 1 ) ) + 
    alpha.prior + beta.prior
  if (is.na(out)) out = -Inf
  out
}

obs.dat = cbind( dat.sel$Accuracy, 
                 log( dat.sel$Contrast ) )

results = Metro_Hastings(li_func,
                         st.f(),par_names = c('alpha','beta'),
                         iterations = 5000,
                         burn_in = 1000,
                         adapt_par = c(100, 20, 0.5, 0.75),
                         quiet = FALSE, dat = obs.dat,
                         priors = c( 2, 3, 3.5, 3 ) )

# Extract samples from posterior
post = results$trace
colnames( post ) = c('alpha','beta')

tmp = density( post[,'alpha'] )
aMode = tmp$x[which( tmp$y == max( tmp$y ) ) ]
tmp = density( post[,'beta'] )
bMode = tmp$x[which( tmp$y == max( tmp$y ) ) ]

lines( x, f.alpha.beta(x,aMode,bMode), col = 'blue', lwd = 2 )

pb = txtProgressBar( min = 0, max = length(x), style = 3 )

CI = matrix( NA, length(x), 2 )
ppc = matrix( NA, length(x), 2 )

cnt = aggregate( rep(1,nrow(dat.sel)), list(dat.sel$Contrast), sum )

for ( xn in 1:length(x) ) {
  
  prb = f.alpha.beta( x[xn], post[,'alpha'], post[,'beta'] )
  CI[xn, c(1,2) ] = hdi( prb )
  
  ppc[xn,1] = qbinom( .025, 40, CI[xn,1] )
  ppc[xn,2] = qbinom( .975, 40, CI[xn,2] )
  
  setTxtProgressBar(pb,xn)
}
close(pb)

polygon( c( x, rev(x) ), c( CI[,1], rev( CI[,2] ) ),
         border = NA, col = rgb( 0, 0, .8, .2 ) )

lines( x, ppc[,1]/40, lty = 2, col = 'blue' )
lines( x, ppc[,2]/40, lty = 2, col = 'blue' )


# Estimates for the marginal posteriors
# x11()
layout( cbind( 1, 2 ) )

tmp = hist( post[,'alpha'], breaks = 40, plot = F )
plot(tmp$mids, tmp$density/max(tmp$density), type = 'n',
     xlim = c(0,4), bty = 'l', xlab = expression(alpha), ylab = 'Overlaid densities' )
segments( tmp$mids, rep(0,length(tmp$density)),
          tmp$mids, tmp$density/max(tmp$density) )
lines( mat$alpha.grid[,1], mat$alpha.prior/max(mat$alpha.prior) )

tmp = hist( post[,'beta'], breaks = 40, plot = F )
plot(tmp$mids, tmp$density/max(tmp$density), type = 'n',
     xlim = c(0,4), bty = 'l', xlab = expression(beta), ylab = 'Overlaid densities' )
segments( tmp$mids, rep(0,length(tmp$density)),
          tmp$mids, tmp$density/max(tmp$density) )
lines( mat$beta.grid[1,], mat$beta.prior/max(mat$beta.prior) )

# Close PDF
dev.off()