#-------------------------------------------#
# Useful functions for the current analysis #
# Kevin Potter                              #
# Updated 04/28/16                          #
#-------------------------------------------#

# Index
# Lookup - 01:  bplt
# Lookup - 02:  param_est
# Lookup - 03:  meta_subject
# Lookup - 04:  subj_ind_create
# Lookup - 05:  cov_create

# Lookup - 01
bplt = function(xa,qnt,wdth,
                lnColor = 'black', lnType = 1, 
                lnWidth = 1,...) {
  # Purpose:
  # A function to draw a custom boxplot onto a plot.
  # Arguments:
  # xa      - The midpoint on the x-axis to start the boxlplot
  # qnt     - A vector of 5 values where...
  #           [1] and [5] = the endpoints of the boxplot
  #           [2] and [4] = the start of the box
  #           [3] = the center line in the box
  # wdth    - the width on either side of the box from the midpoint
  # lnColor - the color of the lines making up the boxplot
  # lnType  - the type of line making up the boxplot
  # lnWidth - the width of the lines making up the boxplot
  # ...     - optional parameters for the 'polygon' function used to 
  #           fill in the boxplot
  
  # Draw the background for the boxplot
  xpos = c(xa-wdth,xa+wdth,xa+wdth,xa-wdth)
  ypos = c(qnt[2],qnt[2],qnt[4],qnt[4])
  
  polygon( xpos,ypos,
           border=NA, ... )
  
  # Draw the boxplot
  xpos = rbind(
    c(xa, xa-wdth, xa-wdth, xa+wdth, xa-wdth, xa, xa-wdth),
    c(xa, xa+wdth, xa-wdth, xa+wdth, xa+wdth, xa, xa+wdth) )
  
  ypos = rbind(
    c(qnt[1],qnt[2],qnt[2],qnt[2],qnt[4],qnt[4],qnt[3]),
    c(qnt[2],qnt[2],qnt[4],qnt[4],qnt[4],qnt[5],qnt[3]) )
  
  segments(xpos[1,],ypos[1,],xpos[2,],ypos[2,],
           lwd=lnWidth,lty=lnType,col=lnColor)
  
}

# Lookup - 02
param_est = function( X, coef, fixed, index, 
                      parSel ) {
  # Purpose:
  # Calculates the linear combination of a set of coefficients 
  # and a design matrix, allowing for the inclusion of fixed values.
  # Arguments:
  # X      - A V x N design matrix (V is the number of covariates, 
  #          and N is number of observations)
  # coef   - A vector of C coefficients
  # fixed  - A vector of F values for coefficients that are fixed
  # index  - A matrix that gives the position of the coefficients and fixed
  #          values in the the P x V parameter matrix. The first column of 
  #          the matrix gives the row index, the second column gives the 
  #          column index. The positions of coefficients come first, 
  #          followed by fixed values, and the first column of the final 
  #          row allows the value of P to be defined.
  # parSel - A vector of the indices for the mapping the coefficients in 
  #          the 'coef' vector to the second column of the 'index' matrix 
  #          (this allows coefficients to be set to be equal across 
  #          different conditions).
  # Returns:
  # A P x N matrix of the set of parameters per observation
  
  N <- ncol(X)
  V <- nrow(X)
  C <- length( parSel )
  L <- dim(index)[1]
  P <- index[ L, 1 ]
  finish <- L - 1 - C
  
  pm <- matrix( 0.0, P, V )
  
  for (i in 1:C ) {
    pm[ index[i,1], index[i,2] ] <- coef[ parSel[i] ]
  }
  
  if (finish > 0) {
    for (i in 1:finish) {
      pm[ index[i+C,1], 
          index[i+C,2] ] <- fixed[i]
    }
  }
  
  return( pm %*% X )
}

# Lookup - 03
meta_subject = function( rt, ch, sbj, 
                         lngth=1000,
                         prb=seq(0,1,.01) ) {
  # Purpose:
  # Generates simulated response times and choices drawn 
  # from the joint empirical CDFs created by aggregating over 
  # the CDFs for a set of subjects, creating data from a 'meta-subject'
  # Arguments:
  # rt    - A vector of response times across all subjects
  # ch    - A vector of choices (0 or 1s) across all subjects
  # sbj   - An index indicating which RTs and choices belong to 
  #         which subjects
  # lngth - The number of observations to simulate for the meta-subject
  # prb   - The points on the emprical CDF to aggregate over (using the 
  #         mean value)
  # Returns:
  # A matrix whose first column is for simulated response times and 
  # second column is for choices
  
  meanChoice = mean(ch)
  
  empiricalCDF1 = aggregate(rt[ch==1],list(sbj[ch==1]),
                            quantile,prob=prb )
  meanEmpiricalCDF1 = aggregate( empiricalCDF1$x, list( 
    rep( 1,nrow(empiricalCDF1) ) ),
    median )
  meanEmpiricalCDF1 = unlist(meanEmpiricalCDF1)[-1]
  
  empiricalCDF0 = aggregate(rt[ch==0],list(sbj[ch==0]),
                            quantile,prob=prb )
  meanEmpiricalCDF0 = aggregate( empiricalCDF0$x, list( 
    rep( 1,nrow(empiricalCDF0) ) ),
    median )
  meanEmpiricalCDF0 = unlist(meanEmpiricalCDF0)[-1]
  
  N_ch_1 = round( lngth*meanChoice )
  N_ch_0 = lngth-N_ch_1;
  
  sim = matrix(NA, lngth, 2)
  sim[,2] = c( rep(1,N_ch_1), rep(0,N_ch_0) )
  
  for (i in 1:N_ch_1) {
    val = runif( N_ch_1 )
    
    lowerLimit = which( prb < val[i] )
    if (length(lowerLimit)==0)
      lowerLimit = 1 else
        lowerLimit = max( lowerLimit );
    upperLimit = which( prb > val[i] )
    if (length(upperLimit)==0)
      upperLimit = length(prb) else
        upperLimit = min( upperLimit );
    sel = c(lowerLimit,upperLimit)
    
    lin_reg = as.numeric( 
      lm( prb[ sel ] ~ meanEmpiricalCDF1[ sel ] )$coef )
    
    sim[i,1] = (val[i]-lin_reg[1])/lin_reg[2]
    
  }
  
  for (i in 1:N_ch_0) {
    val = runif( N_ch_0 )
    
    lowerLimit = which( prb < val[i] )
    if (length(lowerLimit)==0)
      lowerLimit = 1 else
        lowerLimit = max( lowerLimit );
    upperLimit = which( prb > val[i] )
    if (length(upperLimit)==0)
      upperLimit = length(prb) else
        upperLimit = min( upperLimit );
    sel = c(lowerLimit,upperLimit)
    
    lin_reg = as.numeric( 
      lm( prb[ sel ] ~ meanEmpiricalCDF0[ sel ] )$coef )
    
    sim[i+N_ch_1,1] = (val[i]-lin_reg[1])/lin_reg[2]
    
  }
  
  return( sim )
}

# Lookup - 04
subj_ind_create = function(dat) {
  # Purpose:
  # Creates a new index for subjects that 
  # starts at 1 and increases in sequential order
  # Arguments:
  # dat - A data frame with a variable 'Subject'
  # Returns:
  # A vector with the new subject indices
  
  Orig_val = unique(dat$Subject)
  L = length(dat$Subject)
  S = length(Orig_val)
  Subjects = numeric(L)
  inc = 1
  for (s in Orig_val) {
    Subjects[ dat$Subject==s ] = inc
    inc = inc + 1
  }
  
  Subjects
}

# Lookup - 05
cov_create = function(mat) {
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

# Lookup - 06
violinPlotNormal = function( x, pos = 1, xWidth = .4, alpha = .95 ) {
  # Purpose:
  # Given a sample from a posterior, smoothes the sample by 
  # fitting a normal distribution and draws the density (reflected 
  # about the x-axis symmetrically) to a plot with a shaded credible 
  # interval
  # Arguments:
  # x      - the posterior sample
  # pos    - the position on the x-axis to center the density
  # xWidth - the maximum height of the density
  # alpha  - the width of the credible interval
  
  est = normModel(x,alpha=alpha)
  
  val = seq( min(x), max(x), length = 100 )
  
  xW = dnorm( val, est$param[1], est$param[2] )
  xW = xW/max(xW)
  xW = xW*xWidth
  
  sel = val > est$Q[1] & val < est$Q[2]
  polygon( pos - c( 0, xW[sel], 0 ), c( min(val[sel]), val[sel], max(val[sel]) ),
           col = 'grey', border = NA )
  polygon( pos + c( 0, xW[sel], 0 ), c( min(val[sel]), val[sel], max(val[sel]) ),
           col = 'grey', border = NA )
  
  lines( pos - xW, val )
  lines( pos + xW, val )
}
