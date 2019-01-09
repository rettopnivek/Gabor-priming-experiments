function [ intensity ] = f_Theta( lambda, theta, phi, x_i, y_j )
% Purpose:
%   Calculates a sinusoidal plane wave and converts it to a step-function.
% Arguments:
%   lambda - The frequency, the number of oscillations (or bands) within a 
%            single unit
%   theta  - The angle of the bands (in radians)
%   phi    - The phase (in radians), indicating at what point the cycle 
%            starts
%   x_i    - The value of the x-axis coordinate(s)
%   y_j    - The value of the y-axis coordinate(s)
% Returns:
%   The grayscale intensity values (bounded between 0 and 1).

intensity = sin( 2*pi*lambda*( x_i*cos(theta) + y_j*sin(theta) + phi ) );
% Center at .5 and bound between 0 and 1
intensity = .5*intensity + .5;
% Convert to step function
intensity = round( intensity );

end

