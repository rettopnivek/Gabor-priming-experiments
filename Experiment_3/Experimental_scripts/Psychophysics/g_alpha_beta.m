function [ x ] = g_alpha_beta( theta, alpha, beta )
% Purpose:
%   Calculates the inverse of a version of the logistic function in
%   which the slope and the cut-off can vary (truncated to not fall 
%   below chance performance).
% Arguments:
%   theta - A probability
%   alpha - The slope
%   beta  - The negative of the cut-off point for .5 accuracy
% Returns:
%   A vector (or scalar when appropriate) of values bounded between 
%   -infinity and infinity.

x = -log( (1 ./ theta) - 1 ) ./ alpha - beta;
x( theta < .5 ) = -beta;

end

