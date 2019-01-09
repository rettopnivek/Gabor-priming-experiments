#--------------------------------------#
# Exploration of psychophysics variant #
# of Wald race model                   #
#--------------------------------------#

library( seqmodels )

f_alpha_beta = function( targetContrast, alpha, beta ) {
  
  x = log( targetContrast );
  
  theta = 1.0 / ( 1.0 + exp( -alpha * ( x + beta ) ) );
  theta[ theta < .5 ] = .5;
  
  return( theta )
}

kappa = .8
tau = .3

foilContrastDefault = .05;
targetRatios = seq( 0, 5, length = 1000 );
targetContrasts = foilContrastDefault * targetRatios;

alpha = 3; beta = 2.9

x1 = 5 * f_alpha_beta( targetContrasts, alpha, beta )
x0 = 5 * ( 1 - f_alpha_beta( targetContrasts, alpha, beta ) )

p_y_equals_1 = pwaldrace( Inf, 1, kappa, x1, tau, kappa, x0, tau )

x11()
plot( targetRatios, p_y_equals_1, type = 'l',
      ylim = c( 0, 1 ), bty = 'l', xlab = 'Target to foil ratio',
      ylab = 'P( Y = 1 )' )
abline( h = .8, lty = 2 )

abline( v = targetRatios[ min( which( p_y_equals_1 >= .8 ) ) ] )