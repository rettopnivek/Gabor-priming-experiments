#-----------------------------------------------#
# SDT model to identify non-performing subjects #
# Kevin Potter                                  #
# Updated 04/15/2016                            #
#-----------------------------------------------#

orig_dir = getwd()

###
### Load in useful functions
###

library(postexamine)
library(rstan)
library(loo)

# To run chains in parallel
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Indicates which code segments to run
runCode = c(
  T, # Null model
  T, # Performing model
  F  # Prior predictive check
)

# Create stan scripts in save them to a specific folder
setwd('Stan_scripts')

###
### Null model (d' is fixed to 0)
###

null_model_1 = 
"data {
  int No; // Number of observations
  int Nk;  // Number of covariates for kappa
  matrix[No,Nk] X_k; // Design matrix for kappa
  int Y[No]; // Array for number of times subject picked right
  int Nt[No]; // The total number of trials per observation
  matrix[Nk,2] Priors; // Matrix of parameters for prior distributions
}
parameters {
  vector[Nk] beta_k;
}
model {
  // Variable declaration
  vector[No] kappa;
  real theta[No];
  
  // Priors
  for (k in 1:Nk) beta_k[k] ~ normal( Priors[k,1], Priors[k,2] );
  
  // Likelihood
  kappa <- X_k*beta_k;
  
  for (no in 1:No) {
    theta[no] <- 1.0 - normal_cdf(kappa[no],0.0,1.0);
  }
  
  Y ~ binomial(Nt,theta);
}
generated quantities {
  // Variable declaration
  vector[No] kappa;
  real theta[No];
  vector[No] logLik;
  int sim1[No];
  int sim2[No];
  
  // Likelihood
  kappa <- X_k*beta_k;
  
  for (no in 1:No) {
    theta[no] <- 1.0 - normal_cdf(kappa[no],0.0,1.0);
    sim1[no] <- binomial_rng(Nt[no],theta[no]);
    sim2[no] <- binomial_rng(Nt[no],theta[no]);
    logLik[no] <- binomial_log(Y[no], Nt[no], theta[no]);
  }
}
"

# Write script file for stan
fname = file("Null_SDT_script.stan")
writeLines( null_model_1, con=fname )
close(fname)

###
### Performing model (d' and kappa are freely estimated)
###

# Generalized script for 2AFC SDT model (One subject)
modelScript = 
"data {
  int No; // Number of observations
  int Nd; // Number of covariates for d'
  int Nk; // Number of covariates for kappa
  matrix[No,Nd] X_d; // Design matrix for d'
  matrix[No,Nk] X_k; // Design matrix for kappa
  int Y[No]; // Array for number of times subject picked right
  int Nt[No]; // The total number of trials per observation
  matrix[Nd+Nk,2] Priors; // Matrix of parameters for prior distributions
}
parameters {
  vector[Nd] beta_d;
  vector[Nk] beta_k;
}
model {
  // Variable declaration
  vector[No] dprime;
  vector[No] kappa;
  real theta[No];
  
  // Priors
  for (d in 1:Nd) beta_d[d] ~ normal( Priors[d,1], Priors[d,2] );
  for (k in 1:Nk) beta_k[k] ~ normal( Priors[Nd+k,1], Priors[Nd+k,2] );
  
  // Likelihood
  dprime <- X_d*beta_d;
  kappa <- X_k*beta_k;
  
  for (no in 1:No) {
    theta[no] <- 1.0 - normal_cdf(kappa[no],dprime[no],1.0);
  }
  
  Y ~ binomial(Nt,theta);
}
generated quantities {
  // Variable declaration
  vector[No] dprime;
  vector[No] kappa;
  real theta[No];
  vector[No] logLik;
  int sim1[No];
  int sim2[No];
  
  // Likelihood
  dprime <- X_d*beta_d;
  kappa <- X_k*beta_k;
  
  for (no in 1:No) {
    theta[no] <- 1.0 - normal_cdf(kappa[no],dprime[no],1.0);
    sim1[no] <- binomial_rng(Nt[no],theta[no]);
    sim2[no] <- binomial_rng(Nt[no],theta[no]);
    logLik[no] <- binomial_log(Y[no], Nt[no], theta[no]);
  }
  
}
"

# Write script file for stan
fname = file("Perf_SDT_script.stan")
writeLines( modelScript, con=fname )
close(fname)

###
### Extract data to be fitted
###

# Subject indices
subj = sort( unique( allData$Subject ) )

# Select main experimental blocks
curData = allData[allData$BlockType == 2,]

# Initialize list to store data
binDat = c()

# Loop through all subjects
for (s in 1:N) {
  # s = 1 # Select a subject
  
  # Determine number of target identifications and trials
  # per condition
  sel = curData$Subject == subj[s]
  Y = aggregate( curData$Choice[sel], list(
    curData$PrimeDuration[sel],
    curData$Prime[sel],
    curData$Target[sel] ),
    sum )
  colnames(Y) = c('PrimeDuration','Prime','Target','Y')
  Nt = aggregate( rep(1,sum(sel)), list(
    curData$PrimeDuration[sel],
    curData$Prime[sel],
    curData$Target[sel] ),
    sum )$x
  
  binDat = c( binDat, list( list( Y = Y, Nt = Nt ) ) )
}
# Clean up workspace
rm( s, sel, Y, Nt, curData )

###
### Fit models using Stan
###

### Prior predictive check ###

if (runCode[3]) {
  
  mu_dp = .771
  sig_dp = .3
  mu_k = 0
  sig_k = .3
  
  S = 10000
  I = 24
  
  dp = rnorm(S,mu_dp,sig_dp)
  k = rnorm(S,mu_k,sig_k)
  
  theta_L = 1 - pnorm( k, -dp*.5, 1 )
  theta_R = 1 - pnorm( k, dp*.5, 1 )
  
  L = rbinom( S, I, theta_L )
  R = rbinom( S, I, theta_R )
  
  dL = table(L)/S
  dR = table(R)/S
  
  yl = c(0,ceiling( 10*max( c(dL,dR) ) )/10)
  
  # x11()
  layout( cbind(1,2) )
  plot( c(0,I), yl, type = 'n',
        bty = 'l', xlab = 'Freq. R (L correct)',
        ylab = 'Density', xaxt = 'n' )
  axis( 1, seq(0,I,length=6), round(seq(0,I,length=6)/I,2) )
  points( as.numeric( names(dL) ), dL, pch = 19 )
  
  plot( c(0,I), yl, type = 'n',
        bty = 'l', xlab = 'Freq. R (R correct)',
        ylab = 'Density', xaxt = 'n' )
  axis( 1, seq(0,I,length=6), round(seq(0,I,length=6)/I,2) )
  points( as.numeric( names(dR) ), dR, pch = 19 )
  
}

# List to store results for each subject
subjCheck = c()

# s = 1 # Select a subject
for (s in 1:N) {
  
  # Track progress
  cat( 'Subject', s, '\n' )
  
  ### Fit null model ###
  
  if ( runCode[1] ) {
    
    # Design matrix for kappa
    X_k = designCoding(c(1:10,1:10),type='Intercept')
    
    # Priors
    Priors = cbind(
      c( rep(0,ncol(X_k) ) ),
      rep( .3, ncol(X_k) )
    )
    
    # Extract data
    Y = binDat[[s]]$Y
    Nt = binDat[[s]]$Nt
    
    # Input for Stan
    stan_dat = list(
      No = nrow(Y),
      Nk = ncol(X_k),
      X_k = X_k,
      Y = Y$Y,
      Nt = Nt,
      Priors = Priors
    )
    
    burn = 500 # Burn-in
    niter = 2000 # Number of samples to approximate posterior
    
    startTime = Sys.time() # To assess run time
    
    # Fit model in Stan
    fit = stan(file = 'Null_SDT_script.stan', data = stan_dat, 
               warmup = burn, iter = burn+niter, 
               chains = 6)
    post = extract(fit) # Extract posterior samples
    
    # Total run time of code
    runTime = Sys.time() - startTime
    print( runTime); rm( startTime )
    
    # Credible intervals
    smth_kappa = apply( post$beta_k, 2, normModel )
    cred_int_null = rep( 1, ncol( post$beta_k ) )
    for (i in 1:ncol( post$beta_k ) ) 
      if ( smth_kappa[[i]]$Q[1]< 0.0 & smth_kappa[[i]]$Q[2] > 0.0 ) 
        cred_int_null[i] = 0
    
    # Posterior predictive check
    pred = apply( post$sim1, 2, quantile, prob=c(.25,.5,.75) )
    chk_null = Y$Y < pred[1,] | Y$Y > pred[3,] # Observed values outside 50% CI
    
    null_model = loo( post$logLik )
    
    yes_run = F
    if (yes_run) {
      # Posterior predictive check
      pred = apply( post$sim1, 2, quantile )
      x11()
      pred = apply( post$sim1, 2, quantile )
      x11()
      plot( c(1,20),c(0,24),type='n', main = 'Null',
            bty = 'l', xlab = 'Condition', ylab = 'Frequency' )
      segments( 1:20, pred[1,], 1:20, pred[5,] )
      segments( 1:20 - .2, pred[3,], 1:20+.2, pred[3,] )
      for (i in 1:20) {
        polygon( i + c(-.2,-.2,.2,.2),
                 pred[c(2,4,4,2),i],
                 col = 'white')
      }
      points( 1:20, Y$Y, pch = 19 )
    }
    
  }
  
  ### Fit performing model ###
  
  if ( runCode[2] ) {
    
    # Design matrix for d'
    X_d = designCoding(rep(1:10),type='Intercept')*-1
    X_d = rbind(X_d,designCoding(rep(1:10),type='Intercept'))
    # Design matrix for kappa
    X_k = designCoding(c(1:10,1:10),type='Intercept')
    
    # Priors
    Priors = cbind(
      c( rep(.771, ncol(X_d)), rep(0,ncol(X_k) ) ),
      rep( .3, ncol(X_d)+ncol(X_k) )
    )
    
    Y = binDat[[s]]$Y
    Nt = binDat[[s]]$Nt
    
    # Input for Stan
    stan_dat = list(
      No = nrow(Y),
      Nd = ncol(X_d),
      Nk = ncol(X_k),
      X_d = X_d,
      X_k = X_k,
      Y = Y$Y,
      Nt = Nt,
      Priors = Priors
    )
    
    burn = 500 # Burn-in
    niter = 2000 # Number of samples to approximate posterior
    
    startTime = Sys.time() # To assess run time
    
    # Fit model in Stan
    fit = stan(file = 'Perf_SDT_script.stan', data = stan_dat, 
               warmup = burn, iter = burn+niter, 
               chains = 6)
    post = extract(fit) # Extract posterior samples
    
    # Total run time of code
    runTime = Sys.time() - startTime
    print( runTime); rm( startTime )
    
    smth_dp = apply( post$beta_d, 2, normModel )
    smth_kappa = apply( post$beta_k, 2, normModel )
    cred_int = list( dp = rep( 1, ncol( post$beta_d ) ),
                     k = rep( 1, ncol( post$beta_k ) ) )
    for (i in 1:ncol( post$beta_d ) ) 
      if ( smth_dp[[i]]$Q[1]> 0.0 | smth_dp[[i]]$Q[2] < 0.0 ) cred_int[[1]][i] = 0
    for (i in 1:ncol( post$beta_k ) ) 
      if ( smth_kappa[[i]]$Q[1]< 0.0 & smth_kappa[[i]]$Q[2] > 0.0 ) cred_int[[2]][i] = 0
    
    # Posterior predictive check
    pred = apply( post$sim1, 2, quantile, prob=c(.25,.5,.75) )
    chk_perf = Y$Y < pred[1,] | Y$Y > pred[3,] # Observed values outside 50% CI
    
    perf_model = loo( post$logLik )
    
    
    yes_run = F
    if (yes_run) {
      # Posterior predictive check
      pred = apply( post$sim1, 2, quantile )
      x11()
      plot( c(1,20),c(0,24),type='n', main = 'Performing',
            bty = 'l', xlab = 'Condition', ylab = 'Frequency' )
      segments( 1:20, pred[1,], 1:20, pred[5,] )
      segments( 1:20 - .2, pred[3,], 1:20+.2, pred[3,] )
      for (i in 1:20) {
        polygon( i + c(-.2,-.2,.2,.2),
                 pred[c(2,4,4,2),i],
                 col = 'white')
      }
      points( 1:20, Y$Y, pch = 19 )
    }
    
  }
  
  ### Compare models
  
  if ( sum(runCode[1:2])==2 ) {
    res = 'No preference'
    # (-) Prefer null, (+) prefer performing
    cmp = compare( null_model, perf_model )
    conf_int = c( cmp[1] - 2*cmp[2], cmp[1] + 2*cmp[2] )
    if (conf_int[2] < 0.0) res = 'Null'
    if (conf_int[1] > 0.0) res = 'Perf'
    
    subjCheck = c( subjCheck,
                   list( list(
                     PPC = c( null = sum(chk_null), perf = sum(chk_perf) ),
                     LOO = list( comp = cmp, CI = conf_int ),
                     CI = list( Null = cred_int_null, Perf = cred_int ),
                     Results = res ) ) )
    
    # Clean up workspace
    rm( fit, post, res, cmp, conf_int, X_k, X_d )
    rm( Y, Nt, stan_dat, Priors, burn, niter, runTime )
    rm( smth_kappa, smth_dp, cred_int_null, cred_int, i )
  }
  
}


purge = numeric(N)
for (s in 1:N) {
  cat( "# of 95% CI overlapping with 0:", 
       sum( subjCheck[[s]]$CI$Perf$dp ),"\n" )
  if ( sum( subjCheck[[s]]$CI$Perf$dp )==10 ) purge[s] = 1
}

for (s in 1:N) {
  cmp = subjCheck[[s]]$LOO$comp
  cat( "LOO CI:", 
       cmp[1]-cmp[2]*2,cmp[1]+cmp[2]*2,"\n" )
  if (cmp[1]+cmp[2]*2 < 0 ) purge[s] = 1
}

newData = c()
for (s in 1:N) {
  if (purge[s]==0) {
    sel = allData$Subject == subj[s]
    newData = rbind(newData,allData[sel,])
  }
}
allData = newData

setwd(orig_dir)