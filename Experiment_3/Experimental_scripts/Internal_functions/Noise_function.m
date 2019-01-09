function [ intensity ] = Noise_function( imSize, R, show, prp )
% Purpose:
%   Function to generate a noise patch within a circle or ring.
% Arguments:
%  imSize  - The size in pixels of the final (square) image
%  R       - A pair of radii for the inner and outer circle of the annulus
%  sigma   - Standard deviation for the gaussian filter (larger values
%            lead to more of the gabor patch being exposed)
%  show    - Set to 1 to generate a figure displaying the output
% Returns:
%   An intensity matrix that can be turned into a texture.

% Create the noisy background
intensity = unifrnd(-ones(imSize,imSize),ones(imSize,imSize));

% x-axis coordinates (centered at 0)
X = linspace( -.5, .5, imSize );
X = reshape( repmat( X, imSize, 1 ), 1, imSize*imSize );
% y-axis coordinates (centered at 0)
Y = linspace( -.5, .5, imSize );
Y = repmat( Y, 1, imSize );

if size( R, 2 ) == 1
    X_circ_out = sqrt( R(1)^2 - Y.^2 );
    
    sel = X > 0 & X > X_circ_out;
    intensity(sel) = 0;
    
    sel = X < 0 & X < -X_circ_out;
    intensity(sel) = 0;
else
    X_circ_out = sqrt( R(1)^2 - Y.^2 );
    X_circ_in = sqrt( R(2)^2 - Y.^2 );
    
    sel = ( X < X_circ_in | X > X_circ_out ) & X > 0;
    intensity(sel) = 0; % Set to gray
    
    sel = ( X > -X_circ_in | X < -X_circ_out ) & X < 0;
    intensity(sel) = 0; % Set to gray
    
end

% If desired, displays the patch
if show==1
    colormap gray(256); % use gray colormap (0: black, 1: white)
    imagesc( intensity, [-1 1] );
end

if prp==1
    intensity = (1 + intensity)/2; % Converts to proportion
end

end

