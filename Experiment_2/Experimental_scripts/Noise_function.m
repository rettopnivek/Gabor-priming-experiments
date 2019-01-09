function [ output ] = Noise_function( imSize, sigma, show, prp )
% Purpose:
%   Function to generate a noise patch with a gaussian window.
% Arguments:
%  imSize  - the size in pixels of the final (square) image
%  sigma   - Standard deviation for the gaussian filter (larger values
%            lead to more of the gabor patch being exposed).
%  show    - Set to 1 to generate a figure displaying the output.

% Create the noisy background
background = unifrnd(-ones(imSize,imSize),ones(imSize,imSize));

X = 1:imSize;           % X is a vector from 1 to imageSize
X0 = (X / imSize) - .5; % Rescale X -> -.5 to .5

% Create a grid for grating
[ Xm, Ym ] = meshgrid(X0, X0);

s = sigma / imSize; % Gaussian width as fraction of imageSize
gauss = exp( -(((Xm.^2)+(Ym.^2)) ./ (2* s^2)) ); % formula for 2D Gaussian

% Apply a gaussian window to the noisy background
output = background.*gauss;

% If desired, displays the patch
if show==1
    colormap gray(256); % use gray colormap (0: black, 1: white)
    imagesc( output, [-1 1] );
end

if prp==1
    output = (1 + output)/2; % Converts to proportion
end

end

