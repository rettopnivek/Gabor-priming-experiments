#-----------------------------------#
# Useful functions for the analysis #
# Kevin Potter                      #
# Updated 05/25/16                  #
#-----------------------------------#

# Index
# Lookup - 01:  bplt
# Lookup - 02:  param_est
# Lookup - 03:  f_alpha_beta
# Lookup - 04:  g_alpha_beta
# Lookup - 05:  violinPlot

# Lookup - 01
bplt = function(xa,qnt,wdth,
                lnColor = 'black', lnType = 1, 
                lnWidth = 1,...) {
  # Purpose:
  # A function to draw a custom boxplot onto an already existing plot.
  # Arguments:
  # xa      - The position of the boxplot on the x-axis
  # qnt     - The positions on the x-axis of
  #             1) The lowest endpoint
  #             2) The lower edge of the box
  #             3) The midpoint
  #             4) The upper edge of the box
  #             5) The highest endpoint
  # wdth    - The width of the box
  # lnColor - The color of the lines making up the box
  # lnType  - The type of line making up the box
  # lnWidth - The width of the lines making up the box
  # ...     - Additional parameters for the 'polygon' function
  
  if ( length( qnt ) != 5 ) stop( '5 values required for variable qnt' )
  
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
  # A function to calculate the weighted sum of a design matrix  
  # and a set of coefficients to produce a matrix of parameters 
  # by observations. Fixed coefficient values can also be specified.
  # Arguments:
  # X      - A design matrix
  # coef   - A vector of C coefficients
  # fixed  - A vector of F values for the fixed coefficients
  # index  - A matrix with two columns, the first giving the 
  #          row positions for the free coefficients (followed by the 
  #          row positions for any fixed values), the second giving the 
  #          column positions for the free coefficients (followed by 
  #          the positions for the fixed values). The first column of 
  #          the final row of the matrix indicates the total number of 
  #          desired parameters so that the parameter matrix can be created.
  # parSel - A vector giving the indices mapping the values in the coefficient
  #          vector to the rows of the 'index' matrix, thereby allowing 
  #          different conditions to be constrained to have the same free 
  #          coefficient.
  # Details:
  # Given N observations and P desired parameters, the goal is to produce a 
  # P x N matrix given a set of V covariates, C coefficients, and F fixed values.
  # To do so, a P x V parameter matrix M is specified, and the 'index' matrix along 
  # with the 'parSel' vector are used to fill the positions of the P x V matrix.
  # Fixed values from the 'fixed' vector are additionally included in the matrix 
  # M. To produce the desired P x N output matrix, the P x V matrix M is multiplied 
  # by the V x N design matrix X.
  # Returns:
  # A P x N matrix giving the set of parameter values for each of the N observations.
  
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
f_alpha_beta = function(x,alpha,beta) {
  # Purpose:
  # A psychometric function, in which given a slope and cut-off 
  # parameter, calculates the probability of a correct choice 
  # for a specific stimulus intensity.
  # Arguments:
  # x     - A stimulus intensity level
  # alpha - The slope of the psychometric curve
  # beta  - The cut-off, the stimulus intensity at which 
  #         performance is at 75%
  # Returns:
  # The probability of making a correct choice at the given 
  # stimulus intensity.
  
  theta = .5 + .5/(1+exp(-alpha*(x+beta)))
  
  return( theta )
}

# Lookup - 04
g_alpha_beta = function(theta,alpha,beta) {
  # Purpose:
  # A psychometric function, in which given a slope and cut-off 
  # parameter, calculates the stimulus intensity associated with 
  # a specific probability of making a correct choice.
  # Arguments:
  # theta - The probability of making a correct choice
  # alpha - The slope of the psychometric curve
  # beta  - The cut-off, the stimulus intensity at which 
  #         performance is at 75%
  # Returns:
  # The stimulus intensity associated with the given probability 
  # of making a correct choice.
  
  x = log( .5/(theta-.5) - 1 )/(-alpha) - beta
  
  return( x )
}

# Lookup - 05
violinPlot = function( x, pos, crit = NULL, scaleH = .5, 
                       type = 'greater', ... ) {
  # Purpose:
  # Forthcoming
  # Arguments:
  # Forthcoming
  # Returns:
  # Forthcoming
  
  den = density( x )
  
  if ( length(crit) > 0 ) {
    if (type=='greater') {
      sel = den$x > crit
      PostProb = sum( x > crit )/length(x)
    }
    if (type=='less') {
      sel = den$x < crit
      PostProb = sum( x < crit )/length(x)
    }
  } else {
    sel = rep( T, length( den$x ) )
    PostProb = 1
  }
  
  den$y = den$y/max(den$y); den$y = den$y*scaleH;
  xa = c( -den$y[sel], rev(den$y[sel]) ) + pos
  ya = c( den$x[sel], rev(den$x[sel]) )
  polygon( xa, ya, ... )
  
  return( PostProb )
}
