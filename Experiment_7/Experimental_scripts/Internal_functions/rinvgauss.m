function [ t ] = rinvgauss( kappa, xi )
% Purpose:
%   Generate a random draw from the inverse gaussian distribution.
% Arguments:
%   kappa - The threshold towards which evidence accumulates
%   xi    - The average rate at which evidence accumulates
% Returns:
%   A single random deviate

% Convert parameters into a mean and scale
mu = kappa/xi;
lambda = kappa^2;

% Generate a random draw
v = normrnd(0.0,1.0);
z = unifrnd(0.0,1.0);
y = v^2;
p1 = mu + (mu^2)*y/(2*lambda);
p2 = mu/(2*lambda);
p3 = sqrt( 4*mu*lambda*y + (mu*y)^2 );
x = p1+p2*p3;
if z <= mu/(mu+x)
    t = x;
else
    t = (mu^2)/x;
end;

end

