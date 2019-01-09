function [ x ] = g_alpha_beta( theta, alpha, beta )
% Purpose:
% Calculates the inverse of a version of the logistic function in
% which the slope and the cut-off for accuracy of .75 can vary (adjusted 
% for chance performance for binary choices).
% Arguments:
% theta - a probability
% alpha - the slope
% beta  - the negative of the cut-off point for .75 accuracy
% Returns:
% A vector (or scalar when appropriate) of values bounded between -infinity
% and infinity

x = log( .5 ./ (theta - .5) - 1 ) ./ (-alpha) - beta;
  
end

