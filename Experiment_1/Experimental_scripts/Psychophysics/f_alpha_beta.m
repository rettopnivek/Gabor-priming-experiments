function [ theta ] = f_alpha_beta( x, alpha, beta )
% Purpose:
% Calculates a probability using a version of the logistic function in
% which the slope and the cut-off for accuracy of .75 can vary (adjusted 
% for chance performance for binary choices).
% Arguments:
% x     - a value between -infinity and infinity
% alpha - the slope
% beta  - the negative of the cut-off point for .75 accuracy
% Returns:
% A vector (or scalar when appropriate) of probabilities

theta = .5 + .5 ./ ( 1 + exp( -alpha .* ( x + beta ) ) );

end

