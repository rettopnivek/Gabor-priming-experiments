#------------------------------------#
# Examination of posterior estimates #
# Kevin Potter                       #
# Updated 05/29/2016                 #
#------------------------------------#

###
### Initial setup
###
# Lookup - 01

# Load in package for simulating from a truncated normal distribution
library( msm )

savePlot = T # Indicates whether to save plot as a pdf
runCode = c( T, T, T, F )


if (savePlot) {
  if ( Pilot ) fname = 'WR_H_Pilot' else fname = 'WR_H'
  fname = paste( fname, '_', type, '_results.pdf', sep = '' )
  
  pdf( file = fname, width = 12, height = 6 )
}

###
### Convergence checks
###
# Lookup - 02

if ( runCode[1] ) {
  
  if (!savePlot) x11(width = 12, height = 6 )
  tmp = hist( rhat, plot = F, breaks = 30 )
  
  xl = lowerUpper( .05, tmp$mids )
  yl = c(0, max( tmp$density ) )
  xl[2] = max( 1.15, xl[2] )
  plot( xl, yl, type = 'n', bty = 'l', yaxt = 'n',
        ylab = 'Density', xlab = expression( hat(R) ),
        main = 'Convergence check' )
  abline( v = 1.1, lty = 2 )
  segments( tmp$mids, rep( 0, length( tmp$mids ) ),
            tmp$mids, tmp$density, col = 'grey', lwd = 1.5 )
  
}

###
### Plots of posterior distributions
###
# Lookup - 03

if ( runCode[2] ) {
  
  if (type == 1) {
    
    ### Posterior samples for threshold and residual latency ###
    layout( cbind(1,2) )
    
    yl = lowerUpper( .2, as.vector( post$kappa_mu ) )
    clr = rgb( .5, .5, .5, .6 )
    if (!savePlot) x11(width=12,height=6)
    plot( c(0,1), yl, type = 'n', bty = 'l', xaxt = 'n',
          ylab = 'Posterior samples', xlab = ' ', yaxt = 'n',
          main = 'Threshold' )
    violinPlot( post$kappa_mu[,1], .5, scaleH = .4, border = NA, col = clr[1] )
    axis( 2, seq( yl[1], yl[2], .2 ) )
    
    # Convert posterior into the mean of the beta distribution
    tmp = post$theta_alpha[,1]/(post$theta_alpha[,1] + post$theta_beta[,1])
    yl = lowerUpper( .2, as.vector(tmp) )
    if (!savePlot) x11(width=12,height=6)
    plot( c(0,1), yl, type = 'n', bty = 'l', xaxt = 'n',
          ylab = 'Time (ms)', xlab = ' ', yaxt = 'n',
          main = 'Residual latency' )
    violinPlot( tmp, .5, scaleH = .25, border = NA, col = 'grey' )
    axis( 2, seq( yl[1], yl[2], .1 ), round( seq( yl[1], yl[2], .1 )*colMeans( input$min_RT ), 2 ) )
    
    ### Posterior samples for drift rates ###
    
    layout( 1 )
    D = length( unique( cD$PrimeDuration ) )
    nC = D*2
    xl = c(1 - .5, nC + .5 )
    yl = lowerUpper( .2, as.vector( post$xi_mu ) )
    clr = c(
      rgb( 1, 1, 1, 0 ),
      rgb( .5, .5, .5, .6 ) )
    lt = 2
    if (!savePlot) x11(width=12,height=6)
    plot( xl, yl, type = 'n', bty = 'l', xaxt = 'n',
          ylab = 'Posterior samples', xlab = 'Prime duration' )
    axis( 1, 1:nC, paste( rep( sort( unique( cD$PrimeDuration ) ), 2 ), 'ms' ),
          tick = F )
    abline( v = D+.5 )
    axis( 3, c(D/2,D/2+D),c('Foil primed','Target primed'),tick=F)
    # Set indices for target/foil priming with drift rates
    T_FP = 1:D;       T_TP = 1:D + D;
    F_FP = 1:D + D*2; F_TP = 1:D + D*3
    tar_xi = c( T_FP, T_TP ); foil_xi = c( F_FP, F_TP )
    
    for (i in 1:nC) {
      
      violinPlot( post$xi_mu[,tar_xi[i]], i, scaleH = .4, 
                  col = clr[1], lty = 2 )
      violinPlot( post$xi_mu[,foil_xi[i]], i, scaleH = .4,  border = NA, 
                  col = clr[2] )
      
    }
    legend('topright',c('Target racer','Foil racer'),fill=clr,bty='n')
    mtext('Drift rates',side=3,line=2)
    
  }
  
  if (type == 2) {
    
    D = length( unique( cD$PrimeDuration ) )
    PD = sort( unique( cD$PrimeDuration ) )
    
    ### Posterior samples for threshold ###
    
    xl = c( .5, D + .5 )
    yl = lowerUpper( .2, as.vector( post$kappa_mu ) )
    clr = c( 
      rgb( 1, 1, 1, 0 ),
      rgb( .5, .5, .5, .6 ) )
    if (!savePlot) x11(width=12,height=6)
    plot( xl, yl, type = 'n', bty = 'l', xaxt = 'n',
          ylab = 'Density', xlab = 'Prime Duration' )
    axis( 1, 1:D, paste( PD, 'ms' ), tick = 'F' )
    
    for (i in 1:D) {
      
      violinPlot( post$kappa_mu[,i], i, scaleH = .4, 
                  col = clr[1], lty = 2 )
      violinPlot( post$kappa_mu[,i+D], i, scaleH = .4, 
                  border = NA, col = clr[2] )
      
    }
    
    legend('topleft',c('Primed choice','Unprimed choice'), fill = clr,
           bty = 'n' )
    mtext('Thresholds',side=3,line=2)
    
    
    ### Posterior samples for drift rates ###
    
    xl = c( .5, D + .5 )
    yl = lowerUpper( .2, as.vector( post$xi_mu ) )
    clr = c( 
      rgb( 1, 1, 1, 0 ),
      rgb( .5, .5, .5, .6 ) )
    if (!savePlot) x11(width=12,height=6)
    plot( xl, yl, type = 'n', bty = 'l', xaxt = 'n',
          ylab = 'Density', xlab = 'Prime Duration' )
    axis( 1, 1:D, paste( PD, 'ms' ), tick = 'F' )
    
    for (i in 1:D) {
      
      violinPlot( post$xi_mu[,i], i, scaleH = .4, 
                  col = clr[1], lty = 2 )
      violinPlot( post$xi_mu[,i+D], i, scaleH = .4, 
                  border = NA, col = clr[2] )
      
    }
    
    legend('topleft',c('Target','Foil'), fill = clr,
           bty = 'n' )
    mtext('Drift rates',side=3,line=2)
    
    ### Posterior samples for proportion governing residual latency ###
    
    # Convert posterior into the mean of the beta distribution
    tmp = post$theta_alpha[,1]/(post$theta_alpha[,1] + post$theta_beta[,1])
    yl = lowerUpper( .2, as.vector(tmp) )
    if (!savePlot) x11(width=12,height=6)
    plot( c(0,1), yl, type = 'n', bty = 'l', xaxt = 'n',
          ylab = 'Time (ms)', xlab = ' ', yaxt = 'n' )
    violinPlot( tmp, .5, scaleH = .25, border = NA, col = 'grey' )
    mtext('Residual latency',side=3,line=2)
    axis( 2, seq( yl[1], yl[2], .1 ), round( seq( yl[1], yl[2], .1 )*colMeans( input$min_RT ), 2 ) )
    
  }
  
  if (type == 3) {
    
    D = length( unique( cD$PrimeDuration ) )
    PD = sort( unique( cD$PrimeDuration ) )
    
    ### Posterior samples for threshold ###
    
    xl = c( .5, D + .5 )
    yl = lowerUpper( .2, as.vector( post$kappa_mu ) )
    clr = c( 
      rgb( 1, 1, 1, 0 ),
      rgb( .5, .5, .5, .6 ) )
    if (!savePlot) x11(width=12,height=6)
    plot( xl, yl, type = 'n', bty = 'l', xaxt = 'n',
          ylab = 'Density', xlab = 'Prime Duration' )
    axis( 1, 1:D, paste( PD, 'ms' ), tick = 'F' )
    
    for (i in 1:D) {
      
      violinPlot( post$kappa_mu[,i], i, scaleH = .4, 
                  col = clr[1], lty = 2 )
      violinPlot( post$kappa_mu[,i+D], i, scaleH = .4, 
                  border = NA, col = clr[2] )
      
    }
    
    legend('topleft',c('Primed choice','Unprimed choice'), fill = clr,
           bty = 'n' )
    mtext('Thresholds',side=3,line=2)
    
    ### Posterior samples for drift rates ###
    
    layout( 1 )
    D = length( unique( cD$PrimeDuration ) )
    nC = D*2
    xl = c(1 - .5, nC + .5 )
    yl = lowerUpper( .2, as.vector( post$xi_mu ) )
    clr = c( 
      rgb( 1, 1, 1, 0 ),
      rgb( .5, .5, .5, .6 ) )
    if (!savePlot) x11(width=12,height=6)
    plot( xl, yl, type = 'n', bty = 'l', xaxt = 'n',
          ylab = 'Posterior samples', xlab = 'Prime duration' )
    axis( 1, 1:nC, paste( rep( sort( unique( cD$PrimeDuration ) ), 2 ), 'ms' ),
          tick = F )
    abline( v = D+.5 )
    axis( 3, c(D/2,D/2+D),c('Foil primed','Target primed'),tick=F)
    # Set indices for target/foil priming with drift rates
    T_FP = 1:D;       T_TP = 1:D + D;
    F_FP = 1:D + D*2; F_TP = 1:D + D*3
    tar_xi = c( T_FP, T_TP ); foil_xi = c( F_FP, F_TP )
    
    for (i in 1:nC) {
      
      violinPlot( post$xi_mu[,tar_xi[i]], i, scaleH = .4, 
                  col = clr[1], lty = 2 )
      violinPlot( post$xi_mu[,foil_xi[i]], i, scaleH = .4, 
                  border = NA, col = clr[2] )
      
    }
    legend('topright',c('Target racer','Foil racer'),fill=clr,bty='n')
    mtext('Drift rates',side=3,line=2)
    
    ### Posterior samples for proportion governing residual latency ###
    
    # Convert posterior into the mean of the beta distribution
    tmp = post$theta_alpha[,1]/(post$theta_alpha[,1] + post$theta_beta[,1])
    yl = lowerUpper( .2, as.vector(tmp) )
    if (!savePlot) x11(width=12,height=6)
    plot( c(0,1), yl, type = 'n', bty = 'l', xaxt = 'n',
          ylab = 'Time (ms)', xlab = ' ', yaxt = 'n' )
    violinPlot( tmp, .5, scaleH = .25, border = NA, col = 'grey' )
    mtext('Residual latency',side=3,line=2)
    axis( 2, seq( yl[1], yl[2], .1 ), round( seq( yl[1], yl[2], .1 )*colMeans( input$min_RT ), 2 ) )
    
  }
  
}

###
### Posterior retrodictive checks
###
# Lookup - 04

if ( runCode[3] ) {
  
  pD = sort( unique( cD$PrimeDuration ) )
  pL = length( pD )
  avg_min_RT = colMeans(input$min_RT) # Average fastest response time
  
  # Extract posterior simulations
  kappa_s = post$kappa_mu
  xi_s = post$xi_mu
  tau_s = avg_min_RT*( post$theta_alpha/(post$theta_alpha+post$theta_beta) )
  
  # Set indices for target/foil priming with thresholds/drift rates
  cnt = rep( 1, nrow( cD ) )
  cvrt_str = aggregate( cnt, list(
    cD$PrimeDuration, cD$Prime, cD$Target, cD$Choice ),
    sum )
  colnames( cvrt_str ) = c( 'Duration', 'Prime', 'Target', 'Choice', 'Count' )
  rm( cnt )
  
  if (type == 1) {
    
    k1_index = rep( 1, nrow( cvrt_str ) )
    k0_index = rep( 1, nrow( cvrt_str ) )
    
    x1_index = rep( 1:pL, nrow(cvrt_str)/pL )
    x1_index[ (cvrt_str$Prime == cvrt_str$Target) == 1 ] = (pL+1):(pL*2)
    x1_index[ cvrt_str$Target == 0 ] = x1_index[ cvrt_str$Target == 0 ] + pL*2
    x0_index = rep( 1:pL, nrow(cvrt_str)/pL )
    x0_index[ (cvrt_str$Prime == cvrt_str$Target) == 1 ] = (pL+1):(pL*2)
    x0_index[ cvrt_str$Target == 1 ] = x1_index[ cvrt_str$Target == 1 ] + pL*2
    
  }
  if (type == 2) {
    
    k1_index = rep( 1:pL, nrow(cvrt_str)/pL )
    k1_index[ cvrt_str$Prime == 0 ] = (pL+1):(pL*2)
    k0_index = k1_index;
    k0_index[ cvrt_str$Prime == 0 ] = k1_index[ cvrt_str$Prime == 1 ]
    k0_index[ cvrt_str$Prime == 1 ] = k1_index[ cvrt_str$Prime == 0 ]
    
    x1_index = rep( 1:pL, nrow(cvrt_str)/pL )
    x1_index[ cvrt_str$Target == 0 ] = (pL+1):(pL*2)
    x0_index = x1_index;
    x0_index[ cvrt_str$Target == 0 ] = x1_index[ cvrt_str$Target == 1 ]
    x0_index[ cvrt_str$Target == 1 ] = x1_index[ cvrt_str$Target == 0 ]
    
  }
  if (type == 3) {
    
    k1_index = rep( 1:pL, nrow(cvrt_str)/pL )
    k1_index[ cvrt_str$Prime == 0 ] = (pL+1):(pL*2)
    k0_index = k1_index;
    k0_index[ cvrt_str$Prime == 0 ] = k1_index[ cvrt_str$Prime == 1 ]
    k0_index[ cvrt_str$Prime == 1 ] = k1_index[ cvrt_str$Prime == 0 ]
    
    x1_index = rep( 1:pL, nrow(cvrt_str)/pL )
    x1_index[ (cvrt_str$Prime == cvrt_str$Target) == 1 ] = (pL+1):(pL*2)
    x1_index[ cvrt_str$Target == 0 ] = x1_index[ cvrt_str$Target == 0 ] + pL*2
    x0_index = rep( 1:pL, nrow(cvrt_str)/pL )
    x0_index[ (cvrt_str$Prime == cvrt_str$Target) == 1 ] = (pL+1):(pL*2)
    x0_index[ cvrt_str$Target == 1 ] = x1_index[ cvrt_str$Target == 1 ] + pL*2
    
  }
  
  # Store in array
  cnd = array( NA, dim = c( nrow( kappa_s ), 9, nrow( cvrt_str ) ) )
  cnd[,3,] = 1; cnd[,7,] = 1
  for ( s in 1:nrow( kappa_s ) ) {
    cnd[s,1,] = kappa_s[ s, k1_index ]
    cnd[s,5,] = kappa_s[ s, k0_index ]
    cnd[s,2,] = xi_s[ s, x1_index ]
    cnd[s,6,] = xi_s[ s, x0_index ]
    cnd[s,4,] = tau_s[ s ]
    cnd[s,8,] = tau_s[ s ]
    cnd[s,9,] = cvrt_str$Choice
  }
  
  ts = seq( avg_min_RT, 1.5, length = 20 )
  # Function to generate joint CDF curve
  pred_generate = function(x,ts) {
    
    ts = c( ts, Inf )
    out = pwaldrace( ts,x[9],x[1],x[2],x[4],x[5],x[6],x[8] )
    
    return( out )
  }
  
  tmp = round( apply( cnd[1,,], 2, pred_generate, ts = ts ), 3 )
  out = array( NA, dim = c( dim(cnd)[1], dim(tmp)[1], dim(tmp)[2]/2 ) )
  
  # Create a progress bar using a base R function
  pb = txtProgressBar( min = 0, max = nrow( kappa_s ), style = 3 )
  for ( s in 1:nrow( kappa_s ) ) {
    
    tmp2 = apply( cnd[s,,], 2, pred_generate, ts = ts )
    sel = cvrt_str$Choice == 1
    tmp2 = tmp2[,sel]
    
    ord = 1:pL
    ord = c( ord + pL, ord, ord + pL*2, ord + pL*3 )
    
    out[s,,] = tmp2[,ord]
    
    # Update the progress bar
    setTxtProgressBar(pb,s)
  }
  close(pb)
  
  time_points = array( NA, dim = c( 3, 20, dim(out)[3] ) )
  for ( r in 1:20 ) {
    time_points[,r,] = apply( out[,r,], 2, quantile, prob=c(.025,.5,.975) )
  }
  # ord = order( input$curCnd[,'Tar'], partial = input$curCnd[,'Pri'])
  ord = 1:20
  
  ### Plot observed data ###
  
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
  
  pL = length( unique( cD$PrimeDuration ) )
  if (!savePlot) x11( width = 12, height = 6 )
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
    
    if (i == pL ) par( mar=c(2,.5,2,4.5) ) else par( mar=c(2,2,2,3) )
    plot( c(0,xl[2]), c(0,1), type = 'n',
          bty = 'n', xlab = ' ', ylab = ' ',
          main = paste( 'Primed for', ttl[i,1] ),
          yaxt = 'n' )
    axis(4,seq(0,1,.2))
    
    sel = ag_jntRT$Duration == ag_jntAC$Duration[i] & 
      ag_jntRT$Prime == ag_jntAC$Prime[i]
    
    prb = c(0, seq(.1,.9,.2), 1 )
    
    polygon( c( ts, rev( ts ) ), c( time_points[1,,ord[i]], rev( time_points[3,,ord[i]] ) ),
             border = NA, col = rgb( .5, 0, .5, .3 ) )
    lines( ts, time_points[2,,ord[i]], lty = 2, col = rgb( .5, 0, .5, 1 ) )
    polygon( c( ts, rev( ts ) ), c( time_points[1,,ord[i+pL*2]], rev( time_points[3,,ord[i+pL*2]] ) ),
             border = NA, col = rgb( 0, .5, .5, .3 ) )
    lines( ts, time_points[2,,ord[i+pL*2]], lty = 2, col = rgb( 0, .5, .5, 1 ) )
    
    drawCDF( ag_jntRT[sel,-(1:3)], prb, ag_jntAC$P[i], cex = 1.5 )
    
    if (i==1) legend('topleft',c('Response to target','Response to foil'),
                     pch = c(19,15),bty='n')
    
    if (i==pL) legend('topleft','Foil primed',bty='n')
    
    # Update the progress bar
    setTxtProgressBar(pb,i)
  }
  
  for (i in (1:pL)+pL ) {
    
    if (i == (pL+pL)) par( mar=c(3.5,.5,.5,4.5) ) else par( mar=c(3.5,2,.5,3) )
    plot( c(0,xl[2]), c(0,1), type = 'n',
          bty = 'n', xlab = ' ', ylab = ' ',
          yaxt = 'n' )
    axis(4,seq(0,1,.2))
    
    sel = ag_jntRT$Duration == ag_jntAC$Duration[i] & 
      ag_jntRT$Prime == ag_jntAC$Prime[i]
    
    prb = c(0, seq(.1,.9,.2), 1 )
    
    polygon( c( ts, rev( ts ) ), c( time_points[1,,ord[i]], rev( time_points[3,,ord[i]] ) ),
             border = NA, col = rgb( .5, 0, .5, .3 ) )
    lines( ts, time_points[2,,ord[i]], lty = 2, col = rgb( .5, 0, .5, 1 ) )
    polygon( c( ts, rev( ts ) ), c( time_points[1,,ord[i+pL*2]], rev( time_points[3,,ord[i+pL*2]] ) ),
             border = NA, col = rgb( 0, .5, .5, .3 ) )
    lines( ts, time_points[2,,ord[i+pL*2]], lty = 2, col = rgb( 0, .5, .5, 1 ) )
    
    if (i==(pL+1)) legend( 'topleft',c('Predictions for avg. target',
                                       'Predictions for avg. foil'),
                          fill = c( rgb( 0, .5, .5, 1 ), rgb( .5, 0, .5, 1 ) ),
                          bty = 'n' )
    
    drawCDF( ag_jntRT[sel,-(1:3)], prb, ag_jntAC$P[i], cex = 1.5 )
    
    if (i==(pL+pL)) legend('topleft','Target primed',bty='n')
    
    # Update the progress bar
    setTxtProgressBar(pb,i)
  }
  close(pb)
  
  mtext( 'RT (ms)', side = 1, outer = T, cex = 1, line = -1 )
  mtext( 'Joint CDF', side = 4, outer = T, cex = 1, line = -2 )
  
  
}

if (savePlot) dev.off()

###
### Model comparison
###

if ( runCode[4] ) {
  
  setwd( orig_dir )
  
  if (savePlot) pdf('Model_comparisons.pdf',width=12,height=6)
  
  # Define folder location and file name to save output
  folderName = "C:/Users/Kevin/Documents/Posteriors from Stan/Gabor_priming_2016_v2"
  
  # Load in posteriors from first model
  type = 1
  
  if (Pilot) {
    outName = paste("Pilot_Posterior_estimates_",type,".RData",sep="")
  } else {
    outName = paste("Posterior_estimates_",type,".RData",sep="")
  }
  
  # Load posterior estimates
  setwd( folderName )
  load( outName )
  
  model_1_waic = waic( post$log_lik )
  
  # Load in posteriors from second model
  type = 2
  
  if (Pilot) {
    outName = paste("Pilot_Posterior_estimates_",type,".RData",sep="")
  } else {
    outName = paste("Posterior_estimates_",type,".RData",sep="")
  }
  
  # Load posterior estimates
  setwd( folderName )
  load( outName )
  
  model_2_waic = waic( post$log_lik )
  
  # Load in posteriors from third model
  type = 3
  
  if (Pilot) {
    outName = paste("Pilot_Posterior_estimates_",type,".RData",sep="")
  } else {
    outName = paste("Posterior_estimates_",type,".RData",sep="")
  }
  
  # Load posterior estimates
  setwd( folderName )
  load( outName )
  
  model_3_waic = waic( post$log_lik )
  
  # Clean up workspace
  rm( post )
  
  val = c( model_1_waic$waic, model_2_waic$waic, model_3_waic$waic )
  names(val) = c('Drift','Threshold','Both')
  diff_val = val - min( val )
  w = exp( -.5*diff_val )
  w = w/sum(w)
  
  if (!savePlot) x11( width = 12, height = 6 )
  layout( cbind( 1, 2, 3 ) )
  
  yl = lowerUpper( 100, val )
  xl = c( .5, length( val )+.5 )
  plot( xl, yl, type = 'n', bty = 'l', xaxt = 'n', 
        main = ' ', xlab = 'Model',
        ylab = 'WAIC', cex.lab = 1.3 )
  axis( 1, 1:length(val), names(val), tick = F, cex.axis = 1.3 )
  points( 1:length(val), val, pch = 15 )
  
  legend( 'bottomright', 'Less misfit', bty = 'n', cex = 1.3 )
  legend( 'topright', 'Greater misfit', bty = 'n', cex = 1.3 )
  
  yl = c(0,1)
  xl = c( .5, length( val )+.5 )
  plot( xl, yl, type = 'n', bty = 'l', xaxt = 'n', 
        main = ' ', xlab = 'Model',
        ylab = 'Relative Akaike weights', cex.lab = 1.3 )
  axis( 1, 1:length(val), names(val), tick = F, cex.axis = 1.3 )
  points( 1:length(val), w, pch = 15 )
  
  par( mar = c(3, 5, 3, 5 ) )
  d_v_t = compare( model_1_waic, model_2_waic )
  d_v_s = compare( model_1_waic, model_3_waic )
  t_v_s = compare( model_2_waic, model_3_waic )
  
  val = c( d_v_t[1], d_v_s[1], t_v_s[1] )
  SE = c( d_v_t[2], d_v_s[2], t_v_s[2] )
  CI = rbind( val - 1.96*SE, val + 1.96*SE )
  yl = lowerUpper( 10, as.vector( CI ) )
  xl = c( .5, length(val)+.5 )
  
  plot( 1:length(val), val, pch = 15, 
        ylim = yl, xlim = xl,
        main = ' ', bty = 'n', xaxt = 'n',
        ylab = 'WAIC difference', xlab = ' ',
        cex.lab = 1.3 )
  abline( h = 0, lty = 2 )
  axis(1,1:length(val),c( 'Drift','Drift','Both'),
       tick = F, cex.axis = 1.3 )
  axis(3,1:length(val),c( 'Threshold','Both','Both'),
       tick = F, cex.axis = 1.3 )
  axis(4, 0, '-I------   Direction of preference   ------I-', 
       tick = F, cex.axis = 1.3 )
  segments( 1:length(val), CI[1,], 1:length(val), CI[2,] )
  
  setwd( orig_dir )
  
  if (savePlot) dev.off()
  
}