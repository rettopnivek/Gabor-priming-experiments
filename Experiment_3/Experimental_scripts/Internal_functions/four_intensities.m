function [ cb ] = four_intensities( PP, TC, FC )
% Purpose:
%   Determines the set of four intensities given the maximum peak 
%   and a desired target and foil contrast (using Michelson contrast).
% Arguments:
%   PP - The value of the maximum peak
%   TC - The desired target contrast
%   FC - The desired foil contrast
% Returns:
%   A vector with four values, the intensities for the maximum 
%   trough (TT), the minimum trough (TP), minimum peak (PT), and 
%   maximum peak (PP).
    
PT = PP*(1-FC)/(1+FC);
TP = PP*(1-TC)/(1+TC);
TT = TP*(1-FC)/(1+FC);

cb = [ TT TP PT PP ];

end

