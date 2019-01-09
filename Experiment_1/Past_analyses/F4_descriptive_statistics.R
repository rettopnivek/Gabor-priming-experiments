#------------------------#
# Descriptive statistics #
# Kevin Potter           #
# Updated 04/07/16       #
#------------------------#

###
### Practice
###

###
### Adaptive trials
###

###
### Main experiment
###

pdf('Descriptive_statistics.pdf',width=12,height=6)

# Extract data relevant to main experiment
mainExp = allData[ allData$BlockType == 2, ]

### Accuracy results ###

runCode = c(T,T,T)

if (runCode[1]) {
  
  # x11(width = 12, height = 6 )
  layout( cbind(1,2) )
  
  # Accuracy
  ac = aggregate( mainExp$Accuracy, 
                  list(
                    mainExp$PrimeDuration,
                    mainExp$Target==mainExp$Prime,
                    mainExp$Subject
                  ), mean )
  colnames( ac ) = c("Duration","Prime","Subject","P")
  
  ### Target primed
  
  plot( c(.5,5.5),
        lowerUpper( .1, ac$P ), type = 'n',
        xlab = 'Duration (ms)', ylab = 'Accuracy',
        xaxt = 'n', bty = 'n', main = 'Target primed')
  axis(1, 1:5,
       unique(ac$Duration) )
  
  for (i in 1:N) {
    sel = ac$Prime == T & ac$Subject == unique(ac$Subject)[i]
    lines( 1:5 + seq(-.2,-.05,length=N)[i],
           ac$P[sel], col = rgb( 0,1,0,.4), lwd = 2 )
  }
  
  for (i in 1:5) {
    sel = ac$Prime == T & ac$Duration == unique(ac$Duration)[i]
    points( i + seq(-.2,-.05,length=N), ac$P[sel], pch = 21, bg = 'green' )
  }
  
  ### Foil primed
  
  plot( c(.5,5.5),
        lowerUpper( .1, ac$P ), type = 'n',
        xlab = 'Duration (ms)', ylab = 'Accuracy',
        xaxt = 'n', bty = 'n', main = 'Foil primed')
  axis(1, 1:5,
       unique(ac$Duration) )
  
  for (i in 1:N) {
    sel = ac$Prime == F & ac$Subject == unique(ac$Subject)[i]
    lines( 1:5 + seq(.05,.2,length=N)[i],
           ac$P[sel], col = rgb( 1,0,0,.4), lwd = 2 )
  }
  
  for (i in 1:5) {
    sel = ac$Prime == F & ac$Duration == unique(ac$Duration)[i]
    points( i + seq(.05,.2,length=N), ac$P[sel], pch = 21, bg = 'red' )
  }
  
}

### Response times ###

if (runCode[2]) {
  
  # x11(width = 12, height = 6 )
  layout( cbind( 1, 2 ) )
  
  # RT
  rt = aggregate( mainExp$RT, 
                  list(
                    mainExp$PrimeDuration,
                    mainExp$Target==mainExp$Prime,
                    mainExp$Accuracy,
                    mainExp$Subject
                  ), mean )
  colnames( rt ) = c("Duration","Prime","Accuracy","Subject","RT")
  
  cnd = c( T, F )
  
  inc = 1
  ttl = c('Target primed','Foil primed')
  for (k in 1:2) {
    
    plot( c(.5,5.5),
          lowerUpper( .1, rt$RT ), type = 'n',
          xlab = 'Duration (ms)', ylab = 'RT (s)',
          xaxt = 'n', bty = 'n', main = ttl[k])
    axis(1, 1:5,
         unique(rt$Duration) )
    
    shft = c(-.125,.125)
    clr = c('grey','white')
    for (m in 0:1) {
      
      for (i in 1:5) {
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
  jntRT = aggregate( curData$RT, list(
    curData$PrimeDuration,
    curData$Prime==curData$Target,
    curData$Accuracy,
    curData$Subject ),
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
  jntAC = aggregate( curData$Accuracy, list(
    curData$PrimeDuration,
    curData$Prime==curData$Target,
    curData$Subject ),
    mean )
  colnames(jntAC)=c('Duration','Prime','Subject','P')
  
  ag_jntAC = aggregate( jntAC$P, list(
    jntAC$Duration,
    jntAC$Prime ),
    mean )
  colnames( ag_jntAC ) = c('Duration','Prime','P')
  
  # Create joint CDF plots
  
  # x11( width = 12, height = 6 )
  layout( rbind( 1:5, 6:10 ) )
  
  ttl = cbind(
    paste( jntAC$Duration, 'ms' ),
    rep( c('Foil','Target'), each = 5 ) )
  
  drawCDF = function(qnt,prb,ac, pts = c(19,15), 
                     clr = c('black','black'), ... ) {
    
    points( qnt[2,], prb*ac, pch = pts[1], col = clr[1], ... )
    points( qnt[1,], prb*(1-ac), pch = pts[2], col = clr[2], ... )
    
  }
  
  for (i in 1:5) {
    
    if (i == 1) par( mar=c(2,5,2,1) ) else par( mar=c(2,2,2,1) )
    plot( c(0,1.5), c(0,1), type = 'n',
          bty = 'n', xlab = ' ', ylab = ' ',
          main = paste( 'Primed for', ttl[i,1] ) )
    
    sel = ag_jntRT$Duration == ag_jntAC$Duration[i] & 
      ag_jntRT$Prime == ag_jntAC$Prime[i]
    
    prb = c(0, seq(.1,.9,.2), 1 )
    drawCDF( ag_jntRT[sel,-(1:3)], prb, ag_jntAC$P[i], cex = 1.5 )
    
    if (i==1) legend('topleft',c('Response to target','Response to foil'),
                     pch = c(19,15),bty='n')
    
    if (i==5) legend('topright','Foil primed',bty='n')
  }
  
  for (i in 6:10) {
    
    if (i == 6) par( mar=c(3.5,5,.5,1) ) else par( mar=c(3.5,2,.5,1) )
    plot( c(0,1.5), c(0,1), type = 'n',
          bty = 'n', xlab = ' ', ylab = ' ' )
    
    sel = ag_jntRT$Duration == ag_jntAC$Duration[i] & 
      ag_jntRT$Prime == ag_jntAC$Prime[i]
    
    prb = c(0, seq(.1,.9,.2), 1 )
    drawCDF( ag_jntRT[sel,-(1:3)], prb, ag_jntAC$P[i], cex = 1.5 )
    
    if (i==10) legend('topright','Target primed',bty='n')
  }
  
  mtext( 'RT (ms)', side = 1, outer = T, cex = 1, line = -1 )
  mtext( 'Joint CDF', side = 2, outer = T, cex = 1, line = -2 )
  
}

dev.off()