#------------------------#
# Descriptive statistics #
# Kevin Potter           #
# Updated 06/08/16       #
#------------------------#

# Clear workspace
rm(list = ls())

Pilot = T
source( "F3_setup.R" )
if (Pilot) fName = "Pilot_descriptive_stats.pdf" else 
  fName = "Descriptive_stats.pdf"
# if (Pilot) fName = "Pilot_desc_stats.png" else 
#   fName = "Desc_stats.png"

# Load in library for Bayesian bootstrap
# install.packages("bayesboot")
library(bayesboot)

savePlots = T
if (savePlots) pdf( file = fName, width = 12, height = 6 )
# if (savePlots) png( file = fName, width = 960, height = 480 )

# Define function to draw bootstrapped standard errors for the 
# joint CDFs for response times
jntRT_AC_btstrp = function( jntRT, jntAC, ag_jntAC, 
                            prb, i, acVal, lim ) {
  
  nms = paste( prb*100,'%', sep = '' )
  for ( p in 1:length(prb) ) {
    
    sel1 = jntRT$Duration == ag_jntAC$Duration[i] & 
      jntRT$Prime == ag_jntAC$Prime[i] & 
      jntRT$Accuracy == acVal
    sel2 = jntAC$Duration == ag_jntAC$Duration[i] & 
      jntAC$Prime == ag_jntAC$Prime[i]
    if (acVal == 0) kp = jntAC$P != 1
    if (acVal == 1) kp = jntAC$P != 0
    
    if ( acVal == 0 ) curAC = ( 1 - jntAC$P[ kp & sel2 ] )*prb[p]
    if ( acVal == 1 ) curAC = jntAC$P[ kp & sel2 ]*prb[p]
    
    tmp = cbind( jntRT$Q[ sel1, nms[p]], curAC )
    btstrp = bayesboot( tmp, colMeans )
    
    xCI = quantile( btstrp$V1, c(.025,.975) )
    yCI = quantile( btstrp$curAC, c(.025,.975) )
    
    drawEllipse( diff(xCI)/2, diff(yCI)/2,
                 Xc = xCI[1] + diff( xCI )/2,
                 Yc = yCI[1] + diff( yCI )/2,
                 col = 'grey', border = NA )
  }
  
}

runCodeMain = c(
  F, # Practice
  F, # Adaptive trials
  T  # Main experiment
)

###
### Practice
###

if (runCodeMain[1]==T) {
  
  # Select only initial training trials
  sel = allData$Block== 0
  pD = allData[ sel, ]; rm( sel )
  
  # Plot of practice performance
  if (!savePlots ) x11(width=12,height=6)
  layout( cbind( 1, 2 ) )
  
  # Accuracy
  res = aggregate( pD$Accuracy, list( pD$Contrast, pD$Subject ), mean )
  colnames( res ) = c('C','S','P')
  res$C[ res$C == 1 ] = .999
  
  # Log-transform of contrast levels
  cL = log( sort( unique( res$C ) ) )
  
  blankPlot( c(0,4.8) )
  axis( 1, abs( rev( cL ) ), rev( round( sort( unique( res$C ) ), 3 ) ) )
  axis( 2, seq( 0, 1, .2 ) )
  mtext( 'Contrast of target', side = 1, line = 2 )
  mtext( 'P(Correct)', side = 2, line = 2.5 )
  
  for ( s in unique( res$S ) ) {
    lines( abs( rev( log( res$C[ res$S == s ] ) ) ), 
           rev( res$P[ res$S == s ] ), col = 'grey' )
  }
  abline( h = .5, lty = 2 )
  
  xb = aggregate( res$P, list( res$C ), mean )
  se = aggregate( res$P, list( res$C ), sem )
  points( abs( rev( log( xb[,1] ) ) ), rev(xb$x), pch = 19 )
  arrows( abs( rev( log( xb[,1] ) ) ), rev( xb$x - qt( .975, N-1 )*se$x ),
          abs( rev( log( xb[,1] ) ) ), rev( xb$x + qt( .975, N-1 )*se$x ),
          length = .1, angle = 90, code = 3 )
  legend( 'topright', '+/- 95% conf. int.', lty = 1, pch = 19, bty = 'n' )
  
  # Clean up workspace
  rm( s, res, xb, se, cL )
  
  # Median RT
  res = aggregate( pD$RT, list( pD$Contrast, pD$Subject ), median )
  colnames( res ) = c('C','S','P')
  res$C[ res$C == 1 ] = .999
  
  # Log-transform of contrast levels
  cL = log( sort( unique( res$C ) ) )
  
  yl = lowerUpper( .2, res$P )
  blankPlot( c(0,4.8), yl )
  axis( 1, abs( rev( cL ) ), rev( round( sort( unique( res$C ) ), 3 ) ) )
  axis( 2, seq( yl[1], yl[2], .2 ) )
  mtext( 'Contrast of target', side = 1, line = 2 )
  mtext( 'Median RT', side = 2, line = 2.5 )
  
  for ( s in unique( res$S ) ) {
    lines( abs( rev( log( res$C[ res$S == s ] ) ) ), 
           rev( res$P[ res$S == s ] ), col = 'grey' )
  }
  
  xb = aggregate( res$P, list( res$C ), mean )
  se = aggregate( res$P, list( res$C ), sem )
  points( abs( rev( log( xb[,1] ) ) ), rev(xb$x), pch = 19 )
  arrows( abs( rev( log( xb[,1] ) ) ), rev( xb$x - qt( .975, N-1 )*se$x ),
          abs( rev( log( xb[,1] ) ) ), rev( xb$x + qt( .975, N-1 )*se$x ),
          length = .1, angle = 90, code = 3 )
  legend( 'topright', paste('N =',N), bty = 'n' )
                         
  # Clean up workspace
  rm( s, res, xb, se, cL, yl )
  
  mtext( 'Initial practice block', side = 3, outer = T, line = -2 )
  
}

###
### Adaptive trials
###

if (runCodeMain[2]==T) {
  
  sbj = paste("S",unique( allData$Subject ), sep = "" )
  
  if (!savePlots ) x11( width = 12, height = 6 );
  layout( cbind( 1, 2, 3 ) )
  
  ### Plot psychometric curves ###
  
  # Contrast levels
  sel = allData$BlockType == 1
  x_lev = log( c( min( allData$Contrast[sel] ), max( allData$Contrast[sel] ) ) )
  x = seq( x_lev[1], x_lev[2], length = 100 )
  
  plot( x_lev, c(0,1), type = 'n', bty = 'l',
        xlab = "Contrast", xaxt = 'n', ylab = "P(Correct)",
        main = "Estimated psychometric curves" )
  
  allAlpha = numeric( N )
  allBeta = numeric( N )
  
  for ( s in 1:N ) {
    
    mat = matData[[ sbj[s] ]]
    
    alphaAdaptive = mat$alpha.grid[ 
      which( mat$alpha.prior == max(mat$alpha.prior) ), 1 ]
    betaAdaptive = mat$beta.grid[ 
      1, which( mat$beta.prior == max(mat$beta.prior) ) ]
    
    allAlpha[s] = alphaAdaptive
    allBeta[s] = betaAdaptive
    
    clr = rgb( .5, .5, .5, .5 )
    if (Pilot) if ( betaAdaptive > 1.5 ) clr = rgb( 1, 0, 0, .5 )
    
    lines( x, f_alpha_beta( x, alphaAdaptive, betaAdaptive ),
           col = clr )
    axis( 1, seq( x_lev[1], x_lev[2], 1 ),
          round( exp( seq( x_lev[1], x_lev[2], 1 ) ), 2 ) )
    abline( h = .7 )
    
    rm( mat, alphaAdaptive, betaAdaptive )
    
  }
  legend( "topleft", paste( "N =", N ), bty = "n" )
  
  ### Plot slope vs. cut-off ###
  
  xl = lowerUpper( .2, c( allAlpha, allBeta ) )
  plot( xl, xl, type = 'n', bty = 'l', xlab = 'Slope',
        ylab = '75% cut-off', main = 'Slope vs. Cut-off' )
  clr = rep( 'black', N )
  if (Pilot) clr[ allBeta > 1.5 ] = 'pink'
  points( allAlpha, allBeta, pch = 19, col = clr )
  
  ### Plot change in contrast levels over blocks ###
  
  res = aggregate( cD$Contrast, list( cD$Block, cD$Subject ), mean )
  colnames( res ) = c( 'Block','S','C' )
  
  yl = lowerUpper( .1, res$C )
  plot( c(1,6), yl, type = 'n', xaxt = 'n', bty = 'l',
        ylab = 'Contrast for target', xlab = 'Block',
        main = 'Change in contrast over blocks' )
  axis( 1, 1:6, unique( res$Block ) )
  sbjSel = unique( res$S )
  for ( n in 1:N ) {
    lines( 1:6, res$C[ res$S == sbjSel[n] ], col = clr[n] )
    points( 1:6, res$C[ res$S == sbjSel[n] ], pch = 19, col = clr[n] )
  }
  
}

###
### Main experiment
###

if (runCodeMain[3]==T) {
  
  ### Accuracy results ###
  
  runCode = c(F,T,F)
  
  pL = length( unique( cD$PrimeDuration ) )
  
  if (runCode[1]) {
    
    if (!savePlots ) x11(width = 12, height = 6 )
    layout( cbind(1,2) )
    
    # Accuracy
    ac = aggregate( cD$Accuracy, 
                    list(
                      cD$PrimeDuration,
                      cD$Target==cD$Prime,
                      cD$Subject
                    ), mean )
    colnames( ac ) = c("Duration","Prime","Subject","P")
    
    # Average over subjects
    ac_avg = aggregate( ac$P, list( ac$Duration, ac$Prime ),
                         mean )
    colnames( ac_avg ) = c('Duration','Prime','P')
    
    ### Target primed
    
    pL = length( unique( ac$Duration ) )
    
    plot( c(.5,pL+.5),
          lowerUpper( .1, ac$P ), type = 'n',
          xlab = 'Duration (ms)', ylab = 'Accuracy',
          xaxt = 'n', bty = 'n', main = 'Target primed')
    axis(1, 1:pL, unique(ac$Duration) )
    
    clr = rgb( .5, .5, .5, .4 )
    for (i in 1:N) {
      sel = ac$Prime == T & ac$Subject == unique(ac$Subject)[i]
      lines( 1:pL + seq(-.2,-.05,length=N)[i],
             ac$P[sel], col = clr, lwd = 2 )
    }
    
    for (i in 1:pL) {
      sel = ac$Prime == T & ac$Duration == unique(ac$Duration)[i]
      points( i + seq(-.2,-.05,length=N), ac$P[sel], 
              pch = 19, col = clr )
    }
    
    lines( 1:pL-.1, ac_avg$P[ ac_avg$Prime == T ],
           lwd = 3 )
    points(1:pL-.1, ac_avg$P[ ac_avg$Prime == T ], 
           pch = 21, bg = 'white', cex = 1.5 )
    
    ### Foil primed
    
    plot( c(.5,pL+.5),
          lowerUpper( .1, ac$P ), type = 'n',
          xlab = 'Duration (ms)', ylab = 'Accuracy',
          xaxt = 'n', bty = 'n', main = 'Foil primed')
    axis(1, 1:pL, unique(ac$Duration) )
    
    clr = rgb( .5, .5, .5, .4 )
    for (i in 1:N) {
      sel = ac$Prime == F & ac$Subject == unique(ac$Subject)[i]
      lines( 1:pL + seq(.05,.2,length=N)[i],
             ac$P[sel], col = clr, lwd = 2 )
    }
    
    for (i in 1:pL) {
      sel = ac$Prime == F & ac$Duration == unique(ac$Duration)[i]
      points( i + seq(.05,.2,length=N), ac$P[sel], 
              pch = 19, col = clr )
    }
    
    lines( 1:pL-.1, ac_avg$P[ ac_avg$Prime == F ],
           lwd = 3 )
    points(1:pL-.1, ac_avg$P[ ac_avg$Prime == F ], 
           pch = 21, bg = 'white', cex = 1.5 )
    
    
  }
  
  ### Response times ###
  
  if (runCode[2]) {
    
    if (!savePlots ) x11(width = 12, height = 6 )
    layout( cbind( 1, 2 ) )
    
    # RT
    rt = aggregate( cD$RT, 
                    list(
                      cD$PrimeDuration,
                      cD$Target==cD$Prime,
                      cD$Accuracy,
                      cD$Subject
                    ), median )
    colnames( rt ) = c("Duration","Prime","Accuracy","Subject","RT")
    
    cnd = c( T, F )
    
    inc = 1
    ttl = c('Target primed','Foil primed')
    for (k in 1:2) {
      
      plot( c(.5,pL+.5),
            lowerUpper( .1, rt$RT ), type = 'n',
            xlab = 'Duration (ms)', ylab = 'Median RT (s)',
            xaxt = 'n', bty = 'n', main = ttl[k])
      axis(1, 1:pL, unique(rt$Duration) )
      
      shft = c(-.125,.125)
      clr = c('grey','white')
      if (k == 2) legend( 'topright',c('Error','Correct'),
                         fill = clr, bty = 'n' )
      for (m in 0:1) {
        
        for (i in 1:pL) {
          sel = rt$Prime == cnd[k] & rt$Accuracy == m & 
            rt$Duration == unique(rt$Duration)[i]
          qnt = quantile( rt$RT[sel], prob = c(.025,.25,.5,.75,.975) )
          bplt( i+shft[m+1], qnt, .1, col = clr[m+1])
        }
      }
    }
  }
  
  ###
  ### Joint CDF plots of RT
  ###
  
  if (runCode[3]) {
    
    # Extract RT quantiles
    jntRT = aggregate( cD$RT, list(
      cD$PrimeDuration,
      cD$Prime==cD$Target,
      cD$Accuracy,
      cD$Subject ),
      quantile, prob = c(0,seq(.1,.9,.2),1) )
    colnames(jntRT)=c('Duration','Prime','Accuracy','Subject','Q')
    
    ag_jntRT = aggregate( jntRT$Q, list(
      jntRT$Duration,
      jntRT$Prime,
      jntRT$Accuracy ),
      mean )
    colnames( ag_jntRT ) = c('Duration','Prime','Accuracy',
                             'Q0','Q10','Q30','Q50','Q70','Q90','Q100')
    
    # Extract accuracy
    jntAC = aggregate( cD$Accuracy, list(
      cD$PrimeDuration,
      cD$Prime==cD$Target,
      cD$Subject ),
      mean )
    colnames(jntAC)=c('Duration','Prime','Subject','P')
    
    ag_jntAC = aggregate( jntAC$P, list(
      jntAC$Duration,
      jntAC$Prime ),
      mean )
    colnames( ag_jntAC ) = c('Duration','Prime','P')
    
    # Create joint CDF plots
    
    if (!savePlots) x11( width = 12, height = 6 )
    # Determine number of priming durations and adjust plot accordingly
    layout( rbind( 1:pL, 1:pL + pL ) )
    
    ttl = cbind(
      paste( jntAC$Duration, 'ms' ),
      rep( c('Foil','Target'), each = pL ) )
    
    drawCDF = function(qnt,prb,ac, pts = c(19,15), 
                       clr = c('black','black'), ... ) {
      
      points( qnt[2,], prb*ac, pch = pts[1], col = clr[1], ... )
      points( qnt[1,], prb*(1-ac), pch = pts[2], col = clr[2], ... )
      
    }
    
    # Create a progress bar using a base R function
    pb = txtProgressBar( min = 1, max = pL*2, style = 3 )
    
    # Extract limits for x-axis
    xl = lowerUpper( .5, as.numeric( unlist( ag_jntRT[,-(1:3)] ) ) )
    
    for (i in 1:pL) {
      
      if (i == 1 ) par( mar=c(2,1,2,5) ) else par( mar=c(2,2,2,5) )
      plot( c(0,xl[2]), c(0,1), type = 'n',
            bty = 'n', xlab = ' ', ylab = ' ',
            main = paste( 'Primed for', ttl[i,1] ),
            yaxt = 'n' )
      axis(4,seq(0,1,.2))
      
      sel = ag_jntRT$Duration == ag_jntAC$Duration[i] & 
        ag_jntRT$Prime == ag_jntAC$Prime[i]
      
      prb = c(0, seq(.1,.9,.2), 1 )
      
      # Draw standard errors
      jntRT_AC_btstrp( jntRT, jntAC, ag_jntAC, prb[-1], i, 0, 
                       c(0,xl[2],0,1) )
      jntRT_AC_btstrp( jntRT, jntAC, ag_jntAC, prb[-1], i, 1, 
                       c(0,xl[2],0,1) )
      
      segments( ag_jntRT[sel,-(1:3)][,length(prb)],
                c( 1 - ag_jntAC$P[i], ag_jntAC$P[i] ),
                rep(1.5,2), c( 1 - ag_jntAC$P[i], ag_jntAC$P[i] ),
                col = 'grey' )
      
      drawCDF( ag_jntRT[sel,-(1:3)], prb, ag_jntAC$P[i], cex = 1.5 )
      
      if (i==1) legend('topleft',c('Response to target','Response to foil'),
                       pch = c(19,15),bty='n')
      
      if (i==pL) legend('topleft','Foil primed',bty='n')
      
      # Update the progress bar
      setTxtProgressBar(pb,i)
    }
    
    for (i in (1:pL)+pL ) {
      
      if (i == (pL+1)) par( mar=c(3.5,1,.5,5) ) else par( mar=c(3.5,2,.5,5) )
      plot( c(0,xl[2]), c(0,1), type = 'n',
            bty = 'n', xlab = ' ', ylab = ' ',
            yaxt = 'n' )
      axis(4,seq(0,1,.2))
      
      sel = ag_jntRT$Duration == ag_jntAC$Duration[i] & 
        ag_jntRT$Prime == ag_jntAC$Prime[i]
      
      prb = c(0, seq(.1,.9,.2), 1 )
      # Draw standard errors
      jntRT_AC_btstrp( jntRT, jntAC, ag_jntAC, prb[-1], i, 0, 
                       c(0,xl[2],0,1) )
      jntRT_AC_btstrp( jntRT, jntAC, ag_jntAC, prb[-1], i, 1, 
                       c(0,xl[2],0,1) )
      
      segments( ag_jntRT[sel,-(1:3)][,length(prb)],
                c( 1 - ag_jntAC$P[i], ag_jntAC$P[i] ),
                rep(1.5,2), c( 1 - ag_jntAC$P[i], ag_jntAC$P[i] ),
                col = 'grey' )
      
      drawCDF( ag_jntRT[sel,-(1:3)], prb, ag_jntAC$P[i], cex = 1.5 )
      
      if (i==(pL+pL)) legend('topleft','Target primed',bty='n')
      
      # Update the progress bar
      setTxtProgressBar(pb,i)
    }
    close(pb)
    
    mtext( 'RT (ms)', side = 1, outer = T, cex = 1, line = -1 )
    mtext( 'Joint CDF', side = 4, outer = T, cex = 1, line = -2 )
    
  }
  
}

if (savePlots) dev.off()

setwd( orig_dir )