#--------------------------------------#
# Data cleaning for Gabor priming task #
#--------------------------------------#

###
### Single subject
###

### Exclude practice blocks ###
# Lookup - 01
dat.sel = dat.gabor[ dat.gabor$Block >= 2, ]

### Remove timeout responses ###
# Lookup - 02
time.out = 5 # If RT > 5 seconds, indicates a timeout
N.timeout = sum( dat.sel$RT >= time.out )
dat.sel = dat.sel[ dat.sel$RT < time.out, ]

### Remove excessively fast responses ###
# Lookup - 03

# Somewhat arbitrarily, we'll define fast responses to be equal to or under 120 ms
too.fast = .001
N.too.fast = sum( dat.sel$RT <= too.fast )

# Check if accuracy falls within guessing by chance
# print( N.too.fast )
# print( round( mean( dat.sel$Accuracy[ 
#  dat.sel$RT <= too.fast ] ), 3 ) )
#print( round( qbinom( c(.1,.9),
#                      N.too.fast,.5)/N.too.fast, 3 ) )

dat.sel = dat.sel[ dat.sel$RT > too.fast, ]

### Remove excessively slow response ###
# Lookup - 04

# We can use a descriptive mixture model to determine which responses are
# too slow.

# Analysis 1 - truncated exponentially modified gaussian distribution
# Analysis 2 - shifted inverse gaussian distribution

# Analysis = 1
Analysis = 2
plotYes = F

if (Analysis==1) {
  ### 1) Vizualize data
  
  # Extract RTs
  rt = dat.sel$RT
  
  if (plotYes) {
    # Plot of the overall RT distribution
    x11();
    hist( rt, breaks = 40, freq = F, col = 'grey',
          border = 'white', xlab = 'RT (s)', bty = 'l', 
          main = ' ' )
  }
  
  ### 2) Fit an truncated exponentially modified gaussian distribution
  
  # Function to transform values
  tran.par = function( par ) {
    
    par[1] = par[1]
    par[2] = exp( par[2] )
    par[3] = exp( par[3] )
    
    par
  }
  # Function to reverse transformations
  par.tran = function( par ) {
    
    par[1] = par[1]
    par[2] = log( par[2] )
    par[3] = log( par[3] )
    
    par
  }
  # Function to generate random starting values
  st.f = function() {
    tmp = runif( 3,
                 c(-3,.01,0.01),
                 c(3,4,2 ) )
    par.tran( tmp )
  }
  # The likelihood function
  mle.f = function( par, dat ) {
    
    par = tran.par( par )
    
    mu = par[1];
    sigma = par[2];
    lambda = par[3]
    
    logLik = demg( dat, mu, sigma, lambda, ln = 1 )
    # Truncate density to be above 0
    logLik = logLik - log( pemg( Inf, mu, sigma, lambda ) - 
                             pemg( 0, mu, sigma, lambda ) )
    out = sum( logLik )
    # out = sum( logLik ) + dnorm(mu,priors[1,1],priors[1,2],log=T) + 
    #   dunif(sigma,priors[2,1],priors[2,2],log=T) + 
    #   dunif(lambda,priors[3,1],priors[3,2],log=T);
    if (is.na(out)) out = -Inf;
    
    out
  }
  # Fit the model
  results = MLE( rt, st.f, mle.f, nRep = 10, 
                 unDef = -Inf, hessian = T, 
                 control = list( fnscale = -1, 
                                 maxit = 10000 ) )
  # Extract best fitting parameters
  sel = which( results$logLikSum == max( results$logLikSum ) )
  best.par = results$MLE[[ sel ]]
  # Convert parameters to mean and standard deviation
  est = best.par$par; est = tran.par( est )
  round( est, 3 )
  
  # Plot results
  if (plotYes) {
    val = seq( 0 , max( rt ), length = 1000 )
    lines( val, demg( val, est[1],
                      est[2], est[3] ),
           col = 'blue', lwd = 2 )
  }
  
  ### 3) Fit a mixture of a truncated exponentially modified gaussian 
  ###    and a uniform distribution
  
  # Function to transform values
  tran.par = function( par ) {
    
    par[1] = par[1]
    par[2] = exp( par[2] )
    par[3] = exp( par[3] )
    par[4] = logistic( par[4] )
    
    par
  }
  # Function to reverse transformations
  par.tran = function( par ) {
    
    par[1] = par[1]
    par[2] = log( par[2] )
    par[3] = log( par[3] )
    par[4] = logit( par[4] )
    
    par
  }
  # Function to generate random starting values
  st.f = function() {
    tmp = runif( 4,
                 c(-3,.01,0.01,.8),
                 c(3,4,2,.99 ) )
    par.tran( tmp )
  }
  # density function for mixture
  mdemg = function( x, mu, sigma, lambda, alpha, summation = T,
                    sep = F ) {
    
    logLik = demg( x, mu, sigma, lambda, ln = 1)
    # Truncate density to be above 0
    logLik = logLik - log( pemg( Inf, mu, sigma, lambda ) - 
                             pemg( 0, mu, sigma, lambda ) )
    # Uniform density
    U = dunif( x, min(x), max(x) )
    if (sep) return( cbind( alpha*exp(logLik), (1-alpha)*U ) )
    out = alpha*exp(logLik) + (1-alpha)*U
    
    if (summation) return( sum( log(out) ) ) else return( out );
  }
  # The likelihood function
  mle.f = function( par, dat ) {
    
    par = tran.par( par )
    
    mu = par[1];
    sigma = par[2];
    lambda = par[3]
    alpha = par[4]
    
    out = mdemg( dat, mu, sigma, lambda, alpha )
    # out = sum( logLik ) + dnorm(mu,priors[1,1],priors[1,2],log=T) + 
    #   dunif(sigma,priors[2,1],priors[2,2],log=T) + 
    #   dunif(lambda,priors[3,1],priors[3,2],log=T);
    if (is.na(out)) out = -Inf;
    
    out
  }
  # Fit the model
  results = MLE( rt, st.f, mle.f, nRep = 10, 
                 unDef = -Inf, hessian = T, 
                 control = list( fnscale = -1, 
                                 maxit = 10000 ) )
  # Extract best fitting parameters
  sel = which( results$logLikSum == max( results$logLikSum ) )
  best.par = results$MLE[[ sel ]]
  # Convert parameters to mean and standard deviation
  est = best.par$par; est = tran.par( est )
  round( est, 3 )
  
  # Plot results
  if (plotYes) {
    val = seq( 0 , max( rt ), length = 1000 )
    lines( val, mdemg( val, est[1],
                       est[2], est[3], est[4], summation = F ),
           col = 'red', lwd = 2 )
    # The fit should be improved now
  }
  
  # Identify which observations are more likely under the uniform
  # distribution
  comp = mdemg( rt, est[1], est[2], est[3], est[4], sep = T )
  sel = which( comp[,2] > comp[,1] )
  # Trim out these responses from analysis
  dat.sel = dat.sel[ -sel, ]
}

print('CHECK')

if (Analysis==2) {
  
  ### 1) Vizualize data
  
  # Extract RTs
  rt = dat.sel$RT
  
  if (plotYes) {
    # Plot of the overall RT distribution
    x11();
    hist( rt, breaks = 40, freq = F, col = 'grey',
          border = 'white', xlab = 'RT (s)', bty = 'l', 
          main = ' ' )
  }
  
  ### 2) Fit a shifted inverse gaussian distribution
  
  # Function to transform values
  tran.par = function( par ) {
    
    par[1] = exp( par[1] )
    par[2] = exp( par[2] )
    par[3] = logistic( par[3] )*min(rt)
    
    par
  }
  # Function to reverse transformations
  par.tran = function( par ) {
    
    par[1] = log( par[1] )
    par[2] = log( par[2] )
    par[3] = logit( par[3]/min(rt) )
    
    par
  }
  # Function to generate random starting values
  st.f = function() {
    tmp = runif( 3,
                 c(.01,.01,0.01),
                 c(4,4,.99 ) )
    par.tran( tmp )
  }
  # The likelihood function
  mle.f = function( par, dat ) {
    
    par = tran.par( par )
    
    kappa = par[1];
    xi = par[2];
    sigma = 1;
    tau = par[3]
    
    logLik = dinvgauss( dat - tau, kappa, xi, sigma, ln = 1 )
    out = sum( logLik )
    if (is.na(out)) out = -Inf;
    
    out
  }
  # Fit the model
  results = MLE( rt, st.f, mle.f, nRep = 10, 
                 unDef = -Inf, hessian = T, 
                 control = list( fnscale = -1, 
                                 maxit = 10000 ) )
  # Extract best fitting parameters
  sel = which( results$logLikSum == max( results$logLikSum ) )
  best.par = results$MLE[[ sel ]]
  # Convert parameters to mean and standard deviation
  est = best.par$par; est = tran.par( est )
  print( round( est, 3 ) )
  
  # Plot results
  if (plotYes) {
    val = seq( 0 , max( rt ), length = 1000 )
    lines( val, dinvgauss( val - est[3], est[1],
                           est[2], 1 ),
           col = 'blue', lwd = 2 )
  }

  ### 3) Fit a mixture of a shifted inverse gaussian 
  ###    and a uniform distribution
  
  # Function to transform values
  tran.par = function( par ) {
    
    par[1] = exp( par[1] )
    par[2] = exp( par[2] )
    par[3] = logistic( par[3] )*min(rt)
    par[4] = logistic( par[4] )
    
    par
  }
  # Function to reverse transformations
  par.tran = function( par ) {
    
    par[1] = log( par[1] )
    par[2] = log( par[2] )
    par[3] = logit( par[3]/min(rt) )
    par[4] = logit( par[4] )
    
    par
  }
  # Function to generate random starting values
  st.f = function() {
    tmp = runif( 4,
                 c(.01, .01, 0.01, .8 ),
                 c(4, 4, min(rt)*.9, .99 ) )
    par.tran( tmp )
  }
  # density function for mixture
  mdinvgauss = function( t, kappa, xi, sigma, tau, alpha, summation = T,
                    sep = F ) {
    
    logLik = dinvgauss( t - tau, kappa, xi, sigma, ln = 1 )
    # Uniform density
    U = dunif( t, min(t), max(t) )
    
    if (sep) return( cbind( alpha*exp(logLik), (1-alpha)*U ) )
    
    out = alpha*exp(logLik) + (1-alpha)*U
    
    if (summation) return( sum( log(out) ) ) else return( out );
  }
  # The likelihood function
  mle.f = function( par, dat ) {
    
    par = tran.par( par )
    
    kappa = par[1];
    xi = par[2];
    sigma = 1;
    tau = par[3]
    alpha = par[4]
    
    out = mdinvgauss( dat, kappa, xi, sigma, tau, alpha )
    if (is.na(out)) out = -Inf;
    
    out
  }
  # Fit the model
  results = MLE( rt, st.f, mle.f, nRep = 10, 
                 unDef = -Inf, hessian = T, 
                 control = list( fnscale = -1, 
                                 maxit = 10000 ) )
  # Extract best fitting parameters
  sel = which( results$logLikSum == max( results$logLikSum ) )
  best.par = results$MLE[[ sel ]]
  # Convert parameters to mean and standard deviation
  est = best.par$par; est = tran.par( est )
  print( round( est, 3 ) )
  
  # Plot results
  if (plotYes) {
    val = seq( 0 , max( rt ), length = 1000 )
    lines( val, mdinvgauss( val, est[1],
                            est[2], 1, est[3], est[4], summation = F ),
           col = 'red', lwd = 2 )
    # The fit should be improved now
  }

  # Identify which observations are more likely under the uniform
  # distribution
  comp = mdinvgauss( rt, est[1], est[2], 1, est[3], est[4], sep = T )
  sel = which( comp[,2] > comp[,1] )
  # Trim out these responses from analysis
  dat.sel = dat.sel[ -sel, ]
  
}