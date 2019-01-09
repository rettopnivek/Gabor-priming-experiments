#---------------------------#
# Sequential sampling model #
# using BE for one subject  #
# Kevin Potter              #
# 04/26/2016                #
#---------------------------#

###
### Initial set-up
###
# Lookup - 01
source('F3_Setup.R')

# Load in useful functions
library(rstan)
library(loo)
library(seqmodels)
library(utilityf)
library(postexamine)

# Create Stan script
setwd('Stan_scripts')
fileName = "Wald_race_stan_functions.txt"
wr_f = readChar(fileName, file.info(fileName)$size)
fileName = "WR_one_subject_ex.txt"
ms = readChar(fileName, file.info(fileName)$size)

model_script = paste( wr_f, ms, sep = "" )
writeChar( model_script, "WR_OS.stan" )

# Clean up workspace
rm( wr_f, ms, model_script )
setwd( orig_dir )

###
### Subject data and covariates
###

model_structure_create = function(s,type,curData) {
  # Purpose:
  # Forthcoming
  # Arguments:
  # Forthcoming
  # Returns:
  # Forthcoming
  
  # Select a subject's data
  subj = unique( curData$Subject ) # Subject identifiers
  
  # Extract data
  curSubj = curData[ curData$Subject == subj[s], ]
  
  # Select type of model to fit
  if ( type == 1) {
    # xi impacted by
    #   prime type (foil vs. target)
    #   prime duration
    # kappa impacted by
    #   side primed (left vs. right)
    #   binary prime duration ( < 100ms vs. >= 100 ms)
    # tau
    #   intercept only
    
    # Coefficients
    # kappaTarget -> 2
    # xiTarget    -> 10
    # kappaFoil   -> 2
    # xiFoil      -> 10
    # tau         -> 1
    # sigma       -> 1 (Fixed)
    # k = 25
    
    # Extract covariates
    Dur = curSubj$PrimeDuration
    DurBin = as.numeric( Dur < 100 )
    Pri = as.numeric( curSubj$Prime == curSubj$Target )
    Tar = curSubj$Target
    PriSide = Tar
    PriSide[ Pri == 0 ] = 1 - Tar[ Pri == 0 ]
    Int = rep(1,nrow(curSubj))
    
    # Create design matrix
    X_k = designCoding( DurBin, Levels=0:1, type = 'Intercept' )
    cvrt = cov_create( cbind( Dur, Pri ) )
    Levels = aggregate(cvrt,list(Dur,Pri),unique)$x
    X_x = designCoding( cvrt, Levels = Levels, type = 'Intercept' )
    rm( cvrt )
    
    # Desired output
    # 8 x N matrix
    # Necessary input
    # 8 x row(X) -> parameter matrix
    # row(X) x N -> Design matrix
    
    # Design matrix
    X = cbind( X_k*PriSide, X_k*(1-PriSide), X_x*Tar, X_x*(1-Tar), # kappa/xi (1)
               Int, # sigma (1)
               Int, # tau (1)
               X_k*(1-PriSide), X_k*PriSide, X_x*(1-Tar), X_x*Tar, # kappa/xi (0)
               Int, # sigma (0)
               Int  # tau (0)
               )
    X = t(X)
    
    # Create small design matrix for later plotting etc...
    cnd = aggregate( t(X), list(
      Dur, Pri, PriSide, Tar ), unique )
    X_small = t( as.matrix( cnd[,-(1:4)] ) )
    
    # Coefficients
    # k_sh_T k_l_T k_sh_F k_l_F (1:4)
    # xi_T_FP [ 7 - 563 ] xi_T_TP [7 - 563 ] (5:14)
    # xi_F_FP [ 7 - 563 ] xi_F_TP [7 - 563 ] (15:24)
    # tau (25)
    # sigma (Fixed to 1)
    
    # Create index for linear algebra function
    Clm = c( 1:4, 5:24, 26, 1:4+26, 5:24+26, 26+26 )
    Rws = c( rep(1,4), # kappa (1)
             rep(2, length( 5:24 ) ), # xi (1)
             4, # tau (1)
             rep(5,4), # kappa (0)
             rep(6, length( 5:24 ) ), # xi (0)
             8 # tau
            )
    # Create index for parameter selection
    parSel = c( 1:24, 25, 1:24, 25 )
    
    index = cbind( Rws, Clm )
    # Fixed values
    fixed = c(1,1)
    index = rbind( index,
                   c(3,25), # sigma (1)
                   c(7,25+26) # sigma (0)
                   )
    rm( Clm, Rws )
    # Dimensions
    index = rbind( index, c(8,length(parSel)) )
    
    # Return results
    return( list(
      X = X, 
      X_small = X_small, 
      index = index, 
      fixed = fixed, 
      parSel = parSel, 
      curSubj = curSubj ) )
  }
  
}

###
### Model estimation in Stan
###

# To run chains in parallel
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Extract data and covariates
input = model_structure_create(1,1,curData)

# Define set of priors
Priors = cbind(
  c( rep(.85,4), rep(2.0,10), rep(1.4,10), .1 ),
  c( rep(.3,4), rep(.5,10), rep(.5,10), .025 )
)

# Input for Stan
stanDat = list(
  N = nrow(input$curSubj),
  V = nrow(input$X),
  K = length(input$parSel),
  U = length(input$fixed), 
  C = c(4,20,1), 
  X = input$X,
  fixed = input$fixed,
  index = input$index,
  parSel = input$parSel,
  Y = cbind( input$curSubj$RT, input$curSubj$Choice ),
  min_RT = array( min(input$curSubj$RT), dim = 1 ),
  Priors = Priors
)

burn = 250 # Burn-in
niter = 1000 # Number of samples to approximate posterior
chains = 8 # Number of chains to run

setwd('Stan_scripts')

startTime = Sys.time() # To assess run-time
fit = stan(file = 'WR_OS.stan', data = stanDat, 
           warmup = burn, iter = burn+niter, 
           chains = chains)

post = extract(fit)
# Report run time
runTime = Sys.time() - startTime
print( runTime )
rm( startTime )

setwd( orig_dir )