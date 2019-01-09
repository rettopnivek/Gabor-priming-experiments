function [ out ] = log_likelihood_f( alpha, beta, y, x )
% Purpose:
% Calculates the likelihood for a simple psychophysical model using a 
% version of the logistic function in which the slope and the cut-off for 
% accuracy f .75 can vary (adjusted for chance performance for binary 
% choices).
% Arguments:
% alpha - the slope
% beta  - the negative of the cut-off point for .75 accuracy
% y     - the observed accuracy, either 0 or 1
% x     - the sensory level, a value between -infinity and infinity
% Notes:
% The code is vectors, so vectors of matching lengths can be passed in as
% parameters
% Returns:
% A vector (or scalar when appropriate) of likelihoods

  out = y .* f_alpha_beta( x, alpha, beta ) + ...
      ( 1 - y ) .* ( 1 - f_alpha_beta( x, alpha, beta ) );
  out = log( out );

end