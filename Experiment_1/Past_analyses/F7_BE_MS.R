#---------------------------#
# Sequential sampling model #
# using hierarchical BE     #
# Kevin Potter              #
# 04/28/2016                #
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

# Create the data to include and a set of hold-out data


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

###
### Subject data and covariates
###

model_structure_create = function(type,curData,Priors) {
  # Purpose:
  # Creates the input for Stan for a hierarchical Bayesian fit
  # Arguments:
  # type    - A numerical value indicating the type of model structure 
  #           to impose
  # curData - A data frame with the necessary data and covariates
  # Priors  - A matrix of parameter values for prior distributions
  # Returns:
  # A list to be inputted into the stan fitting function
  
  # Number of observations by subject
  No = aggregate( rep(1,nrow(curData)),list(curData$Subject), sum )$x
  No_max = max( No ) # Largest number of observations
  Ns = length( No ) # Number of subjects
  subj = unique( curData$Subject ) # Subject identifiers
  
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
    
    # Coefficients
    # k_sh_T k_l_T k_sh_F k_l_F (1:4)
    # xi_T_FP [ 7 - 563 ] xi_T_TP [7 - 563 ] (5:14)
    # xi_F_FP [ 7 - 563 ] xi_F_TP [7 - 563 ] (15:24)
    # tau (25)
    # sigma (Fixed to 1)
    
    # Define the number of thresholds, drift rates, and residual latencies
    Cf = c( 4, 20, 1 )
    
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
    
    # Fastest RTs by subject
    min_RT = aggregate( curData$RT, list( curData$Subject ), min )$x
    min_RT = array( min_RT, dim = c(Ns,1) )
    
    X = array( 0, dim = c( Ns, length( parSel ) + length( fixed ), No_max ) )
    Y = array( 0, dim = c( Ns, No_max, 2 ) )
    
    # Create a progress bar using a base R function
    pb = txtProgressBar( min = 1, max = Ns, style = 3 )
    
    for ( s in 1:Ns ) {
      
      # Extract data
      curSubj = curData[ curData$Subject == subj[s], ]
      
      Y[s, 1:No[s], ] = cbind( curSubj$RT, curSubj$Choice )
      
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
          Dur, Pri, PriSide, Tar ), unique )
        X_small = t( as.matrix( cnd[,-(1:4)] ) )
        
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
      X_small = X_small ) )
  }
  
}

###
### Model estimation in Stan
###

# To run chains in parallel
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Define priors for hierarchical parameters

Priors = cbind(
  c( c(.8,.8,.9,.9), # Kappa
     rep( c( 1.4, 2 ), each=5 ), # Xi 
     rep( c( 1.4, 1 ), each=5 ), 
     7 ), # Theta (Prop. of tau)
  c( rep( .5, 4 ), # Kappa
     rep( .5, 10 ), # Xi 
     rep( .5, 10 ), 
     .5 ), # Theta (Prop. of tau)
  c( rep(.5, 4 ), # Kappa
     rep( .5, 10 ), # Xi 
     rep( .5, 10 ), 
     3 ), # Theta (Prop. of tau)
  c( rep( .25, 4 ), # Kappa
     rep( .25, 10 ), # Xi 
     rep( .25, 10 ), 
     .5 ) # Theta (Prop. of tau)
)

# Extract data and covariates
input = model_structure_create(1,curData,Priors)

burn = 400 # Burn-in
niter = 1250 # Number of samples to approximate posterior
chains = 8 # Number of chains to run

setwd('Stan_scripts')

startTime = Sys.time() # To assess run-time
fit = stan( file = 'WR_MS.stan', data = input, 
            warmup = burn, iter = burn+niter, 
            chains = chains )

post = extract(fit)
# Report run time
runTime = Sys.time() - startTime
print( runTime )
rm( startTime )

### Quick plot of group level means ###

setwd( orig_dir )

# Threshold
pdf('Posterior_estimates_group_level.pdf',width=12,height=6)
pst = post$kappa_mu[trim,]
# x11( width = 12, height = 6 )
xl = c(.5,ncol(pst)+.5)
yl = lowerUpper( .2, as.vector(pst) )
blankPlot( xl, yl )
axis(1,1:4,c('Short primed','Long primed','Short unprimed','Long unprimed'),tick=F)
axis(2,seq(yl[1],yl[2],.2))
mtext('Thresholds',side=2,line=2)
for (i in 1:ncol(pst)) violinPlotNormal( pst[,i], pos = i )

# Drift rate (Target)
pst = post$xi_mu[trim,]
# x11( width = 12, height = 6 )
xl = c(.5,ncol(pst)+.5)
yl = lowerUpper( .2, as.vector(pst) )
blankPlot( xl, yl )
axis(1,1:20,paste( rep(sort(unique(curData$PrimeDuration)),4), 'ms' ),tick=F,
     cex.axis = .7 )
axis(3,c(2.5,7.5,12.5,17.5),rep( c('Foil primed','Target primed'), 2 ),tick=F)
legend('topleft','Target racers',bty='n')
legend('topright','Foil racers',bty='n')
abline(v=c(5.5,10.5,15.5))
axis(2,seq(yl[1],yl[2],.2))
mtext('Drift rates',side=2,line=2)
for (i in 1:ncol(pst)) violinPlotNormal( pst[,i], pos = i )
dev.off()