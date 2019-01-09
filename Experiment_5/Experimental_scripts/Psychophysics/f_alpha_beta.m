function [ theta ] = f_alpha_beta( x, alpha, beta )
% Purpose:
%   Calculates a probability using a version of the logistic function in
%   which the slope and the cut-off can vary (truncated to not fall 
%   below chance performance).
% Arguments:
%   x     - A value between -infinity and infinity
%   alpha - The slope
%   beta  - The negative of the cut-off point for .5 accuracy
% Returns:
%   A vector (or scalar when appropriate) of probabilities

theta = 1 ./ ( 1 + exp( -alpha .* ( x + beta ) ) );
theta( theta < .5 ) = .5;

end

