#---------------------------#
# Sequential sampling model #
# using hierarchical BE     #
# Kevin Potter              #
# 05/29/2016                #
#---------------------------#

###
### Initial set-up
###

# Clear workspace
rm(list = ls())

# Lookup - 01
Pilot = T
source('F3_Setup.R')

# Load in useful functions
library(rstan)
library(loo)
library(seqmodels)
library(postexamine)

createYes = F
if (createYes) {
  # Create Stan script
  setwd('Stan_scripts')
  fileName = "Wald_race_stan_functions.txt"
  wr_f = readChar(fileName, file.info(fileName)$size)
  fileName = "WR_multi_subject.txt"
  ms = readChar(fileName, file.info(fileName)$size)

  model_script = paste( wr_f, ms, sep = "" )
  writeChar( model_script, "WR_MS.stan" )
  
  # Clean up workspace
  rm( wr_f, ms, model_script )
  setwd( orig_dir )
}

###
### Subject data and covariates
###

model_structure_create = function(type,cD,Priors) {
  # Purpose:
  # A function to create the necessary input to fit a hierarchical 
  # Bayesian response time model in Stan.
  # Arguments:
  # type   - The desired type of model structure and inputs to 
  #          generate, where...
  #          (1) Drift model
  #            - kappa
  #              Intercept only
  #            - xi
  #              Prime type (foil vs. target)
  #              Prime duration ( Assume D durations )
  #            - tau
  #              Intercept only
  #          (2) Threshold model
  #            - kappa
  #              Primed side vs. unprimed side
  #              Prime duration ( Assume D durations )
  #            - xi
  #              Prime type (foil vs. target)
  #            - tau
  #              Intercept only
  #          (3) Over-parameterized model
  #            - kappa
  #              Primed side vs. unprimed side
  #              Prime duration ( Assume D durations )
  #            - xi
  #              Prime type (foil vs. target)
  #              Prime duration ( Assume D durations )
  #            - tau
  #              Prime duration ( Assume D durations )
  # cD     - A data frame with the covariates and dependent variables
  # Priors - A matrix with the desired parameter values for the priors
  # Returns:
  # A list of the inputs
  
  # Number of observations by subject
  No = aggregate( rep(1,nrow(cD)),list(cD$Subject), sum )$x
  No_max = max( No ) # Largest number of observations
  Ns = length( No ) # Number of subjects
  if (Pilot) {
    allSubj = createIncrement( cD$Subject ) # Ensure subject indexing is incremental
  } else allSubj = cD$Subject
  subj = unique( allSubj ) # Subject identifiers
  
  # Select type of model to fit
  if ( type == 1) {
    
    # Extract number of prime durations
    D = length( unique( cD$PrimeDuration ) )
    
    # Coefficients
    # kappa       -> 1
    # xi target   -> D x 2
    # xi foil     -> D x 2
    # tau         -> 1
    # sigma       -> 1 (Fixed)
    # k = 2 + D*4
    
    # Coefficients
    # k (1)
    # xi_T_FP [ all D ] xi_T_TP [ all D ] ( 2:(1 + D*2) )
    # xi_F_FP [ all D ] xi_F_TP [ all D ] ( (1 + D*2):(1 + D*4) )
    # tau (1 + D*4)
    # sigma (Fixed to 1)
    
    # Define the number of thresholds, drift rates, and residual latencies
    Cf = c( 1, D*4, 1 )
    
    kn = 1
    drn = ( max(kn) + 1 ):( max(kn) + D*4 )
    rln = max( drn ) + 1 + Cf[3]
    
    # Create index for linear algebra function
    Clm = c( kn, drn, rln, kn + rln, drn + rln, rln + rln )
    Rws = c( rep( 1, 1 ),   # kappa (1)
             rep( 2, D*4 ), # xi (1)
             rep( 4, 1 ),   # tau (1)
             rep( 5, 1 ),   # kappa (0)
             rep( 6, D*4 ), # xi (0)
             rep( 8, 1 )    # tau (0)
    )
    # Create index for parameter selection
    parSel = c( 1, 2:(1+D*4), (1+D*4)+1, 1, 2:(1+D*4), (1+D*4)+1 )
    
    index = cbind( Rws, Clm )
    # Fixed values
    fixed = c(1,1)
    index = rbind( index,
                   c(3,rln-1), # sigma (1)
                   c(7,(rln-1)+rln) # sigma (0)
    )
    rm( Clm, Rws )
    # Dimensions
    index = rbind( index, c(8,length(parSel)) )
    
    # Fastest RTs by subject
    min_RT = aggregate( cD$RT, list( allSubj ), min )$x
    min_RT = array( min_RT, dim = c(Ns,1) )
    
    X = array( 0, dim = c( Ns, length( parSel ) + length( fixed ), No_max ) )
    Y = array( 0, dim = c( Ns, No_max, 2 ) )
    
    # Create a progress bar using a base R function
    pb = txtProgressBar( min = 1, max = Ns, style = 3 )
    
    for ( s in 1:Ns ) {
      
      # Extract data
      curSubj = cD[ allSubj == subj[s], ]
      
      Y[s, 1:No[s], ] = cbind( curSubj$RT, curSubj$Choice )
      
      # Extract covariates
      Dur = curSubj$PrimeDuration
      Pri = as.numeric( curSubj$Prime == curSubj$Target )
      Tar = curSubj$Target
      PriSide = as.numeric( curSubj$Prime )
      Int = rep( 1, nrow(curSubj) )
      
      # Create design matrix
      X_k = cbind( Int );
      cvrt = covCreate( cbind( Dur, Pri ) )
      Levels = aggregate(cvrt,list(Dur,Pri),unique)$x
      X_x = designCoding( cvrt, Levels = Levels, type = 'Intercept' )
      rm( cvrt )
      
      # Desired output
      # 8 x N matrix
      # Necessary input
      # 8 x row(X) -> parameter matrix
      # row(X) x N -> Design matrix
      
      # Design matrix
      X[ s, , 1:No[s] ] = 
        t( cbind( X_k, X_x*Tar, X_x*(1-Tar), # kappa/xi (1)
               Int, # sigma (1)
               Int, # tau (1)
               X_k, X_x*(1-Tar), X_x*Tar, # kappa/xi (0)
               Int, # sigma (0)
               Int  # tau (0)
        ) )
      
      if ( No[s] == No_max ) {
        
        # Create small design matrix for later plotting etc...
        cnd = aggregate( t(X[s,,]), list(
          Dur, PriSide, Pri, Tar ), unique )
        X_small = t( as.matrix( cnd[,-(1:4)] ) )
        curCnd = cnd[,1:4]
        colnames( curCnd ) = c('Dur','PriSide','Pri','Tar')
        
      }
      
      # Update the progress bar
      setTxtProgressBar(pb,s)
    }
    close(pb)
    
    # Return results
    return( list(
      Ns = Ns, 
      No = No, 
      No_max = No_max, 
      V = dim(X)[2], 
      K = length( parSel ),
      U = length( fixed ), 
      C = Cf,
      X = X, 
      fixed = fixed, 
      index = index, 
      parSel = parSel, 
      Y = Y, 
      min_RT = min_RT, 
      Priors = Priors, 
      X_small = X_small,
      curCnd = curCnd ) )
  }
  
  if ( type == 2) {
    
    # Extract number of prime durations
    D = length( unique( cD$PrimeDuration ) )
    
    # Coefficients
    # kappa primed   -> D
    # xi target      -> D
    # kappa unprimed -> D
    # xi foil        -> D
    # tau            -> 1
    # sigma          -> 1 (Fixed)
    # k = 1 + D*4
    
    # Coefficients
    # k_P [ all D ] k_UP [ all D ] ( 1:(D*2) )
    # xi_T [ all D ] ( (D*2 + 1 ):( D*3 ) )
    # xi_F [ all D ] ( ( D*3 + 1 ):( D*4 ) )
    # tau (1 + D*4)
    # sigma (Fixed to 1)
    
    # Define the number of thresholds, drift rates, and residual latencies
    Cf = c( D*2, D*2, 1 )
    
    kn = 1:( D*2 )
    drn = ( max(kn) + 1 ):( max(kn) + Cf[2] )
    rln = max( drn ) + 1 + Cf[3]
    
    # Create index for linear algebra function
    Clm = c( kn, drn, rln, kn + rln, drn + rln, rln + rln )
    Rws = c( rep( 1, D*2 ),  # kappa (1)
             rep( 2, D*2 ), # xi (1)
             rep( 4, 1 ), # tau (1)
             rep( 5, D*2 ), # kappa (0)
             rep( 6, D*2 ), # xi (0)
             rep( 8, 1 )  # tau (0)
    )
    # Create index for parameter selection
    parSel = c( 1:(D*4), D*4+1, 1:(D*4), D*4+1 )
    
    index = cbind( Rws, Clm )
    # Fixed values
    fixed = c(1,1)
    index = rbind( index,
                   c(3,rln-1), # sigma (1)
                   c(7,(rln-1)+rln) # sigma (0)
    )
    rm( Clm, Rws )
    # Dimensions
    index = rbind( index, c(8,length(parSel)) )
    
    # Fastest RTs by subject
    min_RT = aggregate( cD$RT, list( allSubj ), min )$x
    min_RT = array( min_RT, dim = c(Ns,1) )
    
    X = array( 0, dim = c( Ns, length( parSel ) + length( fixed ), No_max ) )
    Y = array( 0, dim = c( Ns, No_max, 2 ) )
    
    # Create a progress bar using a base R function
    pb = txtProgressBar( min = 1, max = Ns, style = 3 )
    
    for ( s in 1:Ns ) {
      
      # Extract data
      curSubj = cD[ allSubj == subj[s], ]
      
      Y[s, 1:No[s], ] = cbind( curSubj$RT, curSubj$Choice )
      
      # Extract covariates
      Dur = curSubj$PrimeDuration
      Pri = as.numeric( curSubj$Prime == curSubj$Target )
      Tar = curSubj$Target
      PriSide = as.numeric( curSubj$Prime )
      Int = rep( 1, nrow(curSubj) )
      
      # Create design matrix
      Levels = sort( unique( Dur ) )
      X_k = designCoding( Dur, Levels = Levels, type = 'Intercept' )
      X_x = designCoding( Dur, Levels = Levels, type = 'Intercept' )
      
      # Desired output
      # 8 x N matrix
      # Necessary input
      # 8 x row(X) -> parameter matrix
      # row(X) x N -> Design matrix
      
      # Design matrix
      X[ s, , 1:No[s] ] = 
        t( cbind( X_k*PriSide, X_k*(1-PriSide), X_x*Tar, X_x*(1-Tar), # kappa/xi (1)
                  Int, # sigma (1)
                  Int, # tau (1)
                  X_k*(1-PriSide), X_k*PriSide, X_x*(1-Tar), X_x*Tar, # kappa/xi (0)
                  Int, # sigma (0)
                  Int  # tau (0)
        ) )
      
      if ( No[s] == No_max ) {
        
        # Create small design matrix for later plotting etc...
        cnd = aggregate( t(X[s,,]), list(
          Dur, PriSide, Pri, Tar ), unique )
        X_small = t( as.matrix( cnd[,-(1:4)] ) )
        curCnd = cnd[,1:4]
        colnames( curCnd ) = c('Dur','PriSide','Pri','Tar')
        
      }
      
      # Update the progress bar
      setTxtProgressBar(pb,s)
    }
    close(pb)
    
    # Return results
    return( list(
      Ns = Ns, 
      No = No, 
      No_max = No_max, 
      V = dim(X)[2], 
      K = length( parSel ),
      U = length( fixed ), 
      C = Cf,
      X = X, 
      fixed = fixed, 
      index = index, 
      parSel = parSel, 
      Y = Y, 
      min_RT = min_RT, 
      Priors = Priors, 
      X_small = X_small,
      curCnd = curCnd ) )
  }
  
  if ( type == 3) {
    
    # Extract number of prime durations
    D = length( unique( cD$PrimeDuration ) )
    
    # Coefficients
    # kappa primed   -> D
    # kappa unprimed -> D
    # xi target      -> D x 2
    # xi foil        -> D x 2
    # tau            -> 1
    # sigma          -> 1 (Fixed)
    # k = 1 + D*2 + D*4
    
    # Coefficients
    # k_P [ all D ] k_UP [ all D ] ( 1:(D*2) )
    # xi_T_FP [ all D ] xi_T_TP [ all D ] ( (D*2 + 1):(D*4) )
    # xi_F_FP [ all D ] xi_F_TP [ all D ] ( (D*4 + 1):(D*6) )
    # tau (1 + D*6)
    # sigma (Fixed to 1)
    
    # Define the number of thresholds, drift rates, and residual latencies
    Cf = c( D*2, D*4, 1 )
    
    kn = 1:( D*2 )
    drn = ( max(kn) + 1 ):( max(kn) + Cf[2] )
    rln = max( drn ) + 1 + Cf[3]
    
    # Create index for linear algebra function
    Clm = c( kn, drn, rln, kn + rln, drn + rln, rln + rln )
    Rws = c( rep( 1, D*2 ), # kappa (1)
             rep( 2, D*4 ), # xi (1)
             rep( 4, 1 ),   # tau (1)
             rep( 5, D*2 ), # kappa (0)
             rep( 6, D*4 ), # xi (0)
             rep( 8, 1 )    # tau (0)
    )
    # Create index for parameter selection
    parSel = c( 1:(D*6), 1 + D*6, 1:(D*6), 1 + D*6 )
    
    index = cbind( Rws, Clm )
    # Fixed values
    fixed = c(1,1)
    index = rbind( index,
                   c(3,rln-1), # sigma (1)
                   c(7,(rln-1)+rln) # sigma (0)
    )
    rm( Clm, Rws )
    # Dimensions
    index = rbind( index, c(8,length(parSel)) )
    
    # Fastest RTs by subject
    min_RT = aggregate( cD$RT, list( allSubj ), min )$x
    min_RT = array( min_RT, dim = c(Ns,1) )
    
    X = array( 0, dim = c( Ns, length( parSel ) + length( fixed ), No_max ) )
    Y = array( 0, dim = c( Ns, No_max, 2 ) )
    
    # Create a progress bar using a base R function
    pb = txtProgressBar( min = 1, max = Ns, style = 3 )
    
    for ( s in 1:Ns ) {
      
      # Extract data
      curSubj = cD[ allSubj == subj[s], ]
      
      Y[s, 1:No[s], ] = cbind( curSubj$RT, curSubj$Choice )
      
      # Extract covariates
      Dur = curSubj$PrimeDuration
      Pri = as.numeric( curSubj$Prime == curSubj$Target )
      Tar = curSubj$Target
      PriSide = as.numeric( curSubj$Prime )
      Int = rep( 1, nrow(curSubj) )
      
      # Create design matrix
      Levels = sort( unique( Dur ) )
      X_k = designCoding( Dur, Levels = Levels, type = 'Intercept' )
      cvrt = covCreate( cbind( Dur, Pri ) )
      Levels = aggregate(cvrt,list(Dur,Pri),unique)$x
      X_x = designCoding( cvrt, Levels = Levels, type = 'Intercept' )
      rm( cvrt )
      
      # Desired output
      # 8 x N matrix
      # Necessary input
      # 8 x row(X) -> parameter matrix
      # row(X) x N -> Design matrix
      
      # Design matrix
      X[ s, , 1:No[s] ] = 
        t( cbind( X_k*PriSide, X_k*(1-PriSide), # kappa/xi (1)
                  X_x*Tar, X_x*(1-Tar), 
                  Int, # sigma (1)
                  Int, # tau (1)
                  X_k*(1-PriSide), X_k*PriSide, # kappa/xi (0)
                  X_x*(1-Tar), X_x*Tar, 
                  Int, # sigma (0)
                  Int  # tau (0)
        ) )
      
      if ( No[s] == No_max ) {
        
        # Create small design matrix for later plotting etc...
        cnd = aggregate( t(X[s,,]), list(
          Dur, PriSide, Pri, Tar ), unique )
        X_small = t( as.matrix( cnd[,-(1:4)] ) )
        curCnd = cnd[,1:4]
        colnames( curCnd ) = c('Dur','PriSide','Pri','Tar')
        
      }
      
      # Update the progress bar
      setTxtProgressBar(pb,s)
    }
    close(pb)
    
    # Return results
    return( list(
      Ns = Ns, 
      No = No, 
      No_max = No_max, 
      V = dim(X)[2], 
      K = length( parSel ),
      U = length( fixed ), 
      C = Cf,
      X = X, 
      fixed = fixed, 
      index = index, 
      parSel = parSel, 
      Y = Y, 
      min_RT = min_RT, 
      Priors = Priors, 
      X_small = X_small,
      curCnd = curCnd ) )
  }
  
}

###
### Model estimation in Stan
###

# To run chains in parallel
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Define version of model to fit
# type = 1
# type = 2
type = 3

# Define priors for hierarchical parameters
D = length( unique( cD$PrimeDuration ) )

if ( type == 1 ) {
  Priors = cbind(
    c( c(1), # Kappa
       rep( c( 2.725, 2.725 ), each=D ), # Xi 
       rep( c( 1.5, 1.5 ), each=D ), 
       7 ), # Theta (Prop. of tau)
    c( rep( .5, 1 ), # Kappa
       rep( .5, D*2 ), # Xi 
       rep( .5, D*2 ), 
       .5 ), # Theta (Prop. of tau)
    c( rep( 5, 1 ), # Kappa
       rep( 5, D*2 ), # Xi 
       rep( 5, D*2 ), 
       3 ), # Theta (Prop. of tau)
    c( rep( 8, 1 ), # Kappa
       rep( 8, D*2 ), # Xi 
       rep( 8, D*2 ), 
       .5 ) # Theta (Prop. of tau)
  )
}

if ( type == 2 ) {
  Priors = cbind(
    c( rep( 1, D*2 ), # Kappa
       rep( c( 2.725, 1.5 ), each=D ), # Xi 
       7 ), # Theta (Prop. of tau)
    c( rep( .5, D*2 ), # Kappa
       rep( .5, D*2 ), # Xi 
       .5 ), # Theta (Prop. of tau)
    c( rep( 5, D*2 ), # Kappa
       rep( 5, D*2 ), # Xi 
       3 ), # Theta (Prop. of tau)
    c( rep( 8, D*2 ), # Kappa
       rep( 8, D*2 ), # Xi 
       .5 ) # Theta (Prop. of tau)
  )
}

if ( type == 3 ) {
  Priors = cbind(
    c( rep( 1, D*2 ), # Kappa
       rep( c( 2.725, 2.725 ), each=D ), # Xi 
       rep( c( 1.5, 1.5 ), each=D ), 
       7 ), # Theta (Prop. of tau)
    c( rep( .5, D*2 ), # Kappa
       rep( .5, D*4 ), # Xi 
       .5 ), # Theta (Prop. of tau)
    c( rep( 5, D*2 ), # Kappa
       rep( 5, D*4 ), # Xi 
       3 ), # Theta (Prop. of tau)
    c( rep( 8, D*2 ), # Kappa
       rep( 8, D*4 ), # Xi 
       .5 ) # Theta (Prop. of tau)
  )
}

# Define folder location and file name to save output
folderName = "C:/Users/Kevin/Documents/Posteriors from Stan/Gabor_priming_2016_v2"
if (Pilot) {
  outName = paste("Pilot_Posterior_estimates_",type,".RData",sep="")
} else {
  outName = paste("Posterior_estimates_",type,".RData",sep="")
}

modelFit = F
if (modelFit) {
  
  # Extract data and covariates
  input = model_structure_create(type,cD,Priors)
  
  burn = 375 # Burn-in
  niter = 625 # Number of samples to approximate posterior
  chains = 8 # Number of chains to run
  
  setwd('Stan_scripts')
  
  if (Pilot) stan_seed = 30182 else stan_seed = 1384
  
  startTime = Sys.time() # To assess run-time
  fit = stan( file = 'WR_MS.stan', data = input, 
              warmup = burn, iter = burn+niter, 
              chains = chains,
              control = list( adapt_delta = .92 ),
              seed = stan_seed )
  
  post = extract(fit)
  # Report run time
  runTime = Sys.time() - startTime
  print( runTime )
  rm( startTime )
  
  # If desired, exclude certain chains
  
  # Extract rhat values
  rhat = summary(fit)$summary[,"Rhat"]
  rhat = rhat[1:(which(names(rhat)=='lp__')-1)]
  
  # Save posterior estimates
  setwd( folderName )
  save( post, rhat, input, file = outName )
  
} else {
  # Save posterior estimates
  setwd( folderName )
  load( outName )
}

setwd( orig_dir )