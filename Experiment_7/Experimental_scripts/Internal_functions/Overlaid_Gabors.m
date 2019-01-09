function [ intensity ] = Overlaid_Gabors( imSize, PP, TC, FC, thetaT, lambda, R, show )
% Purpose:
%   Function to generate a set of overlaid step-function gabor patches.
% Arguments:
%   imSize  - The size in pixels of the final (square) image
%   PP     - The height of the maximum peak of the set of 4 intensities
%            (Based on illuminance values, 0 - 134)
%   TC     - The desired Michelson contrast level of the target (must be
%            be equal or higher than that of the foil)
%   FC     - The desired Michelson contrast level of the foil
%   thetaT - The orientation in degrees (0 = straight, + rotated
%            clockwise, rotated counter-clockwise). Second grating is
%            90 degrees counter-clockwise from the first
%   lambda - The frequency of the sine wave, determining the number of
%            stripes present for each grating
%   R      - Radius for circle within which gabor patches should be shown
%   show   - Set to 1 to generate a figure displaying the output
% Returns:
%   An intensity matrix that can be turned into a texture.

% x-axis coordinates (centered at 0)
X = linspace( -.5, .5, imSize );
Xc = unifrnd(-.1,.1); % Recenter x-values
X = X + Xc;
X = repmat( X, imSize, 1 );
X = reshape( X, 1, imSize*imSize );
% y-axis coordinates (centered at 0)
Y = linspace( -.5, .5, imSize );
Y = repmat( Y, 1, imSize );

% Angle of target and foil
thetaF = thetaT - 90;
% Convert to radians
thetaT = DegreesToRadians(thetaT);
thetaF = DegreesToRadians(thetaF);

% Determine constants
MN = size( X, 2 ); % Total number of coordinates
beta_ij_T = f_Theta(lambda,thetaT,0,X,Y) - .5;
beta_ij_F = f_Theta(lambda,thetaF,0,X,Y) - .5;

% Create a default grid of intensities that are easily separable
A_F = .4; A_T = .6;
intensity_T = A_T*beta_ij_T + .5; % Intensity for target
intensity_F = A_F*beta_ij_F + .5; % Intensity for foil
default_intensity = .5*intensity_T + .5*intensity_F; % Mixture of two
cb_old = sort( unique( default_intensity ) );

% Determine the new intensity values using Michelson contrast
cb = four_intensities( PP, TC, FC );
% Convert to grayscale values
cb = Illuminance_to_grayscale( cb, 134.025, 2.000 );

% Replace the old intensities with the new ones
intensity = default_intensity;
for i = 1:4
    intensity( default_intensity == cb_old(i) ) = cb(i);
end

% Determine whether to show patch within ring or within the inner circle
if size(R,2) == 1
    Y_keep = sqrt( R^2 - (X-Xc).^2 );
    sel = ( Y > Y_keep ) | ( Y < -Y_keep );
    
    intensity(sel) = .5; % Set to gray
    % intensity(sel) = unifrnd(0,1,1,sum(sel)); % Set to uniform white noise
elseif size(R,2) == 2
    Y_keep1 = sqrt( R(2)^2 - (X-Xc).^2 );
    Y_keep2 = sqrt( R(1)^2 - (X-Xc).^2 );
    
    sel = ( Y < Y_keep1 | Y > Y_keep2 ) & Y > 0;
    intensity(sel) = .5; % Set to gray
    % intensity(sel) = unifrnd(0,1,1,sum(sel)); % Set to uniform white noise
    
    sel = ( Y > -Y_keep1 | Y < -Y_keep2 ) & Y < 0;
    intensity(sel) = .5; % Set to gray
    % intensity(sel) = unifrnd(0,1,1,sum(sel)); % Set to uniform white noise
end

intensity = vec2mat( intensity, imSize ); % Convert to matrix

if show == 1
    colormap gray(256); % use gray colormap (0: black, 1: white)
    imagesc( intensity, [0 1] );
end

end