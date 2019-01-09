function [ posterior, alpha_posterior, beta_posterior ] = ...
    grid_approx( y, x, alpha_val, beta_val, alpha_prior, beta_prior )
% Purpose:
%   A function for estimation of the joint and marginal posteriors of the
%   parameters alpha and beta given a set of priors using grid 
%   approximation.
% Arguments:
%   y           - The observed accuracy ( y = { 0, 1 } )
%   x           - A sensory level
%   alpha_val   - A sequence of the possible values for alpha
%   beta_val    - A sequence of the possible values for beta
%   alpha_prior - The associated prior density for each possible
%                 value of alpha
%   beta.prior  - The associated prior density for each possible
%                 value of beta
% Returns:
% 1) the joint posterior for alpha and beta
% 2) the marginal posterior for alpha
% 3) the marginal posterior for beta
	
% Create a matrix to store the discrete approximation to the joint
% posterior
nA = size( alpha_val, 2 );
nB = size( beta_val, 2 );
posterior = zeros( nA, nB );

% Calculate the log of the posterior
for i = 1:nB
    posterior(:,i) = log_likelihood_f( alpha_val, beta_val(1,i), ...
        y, x ) + log( alpha_prior ) + log( beta_prior(i) );
end

% Normalize the posterior
normalized_posterior = exp( posterior );
normalized_posterior = normalized_posterior ./ ...
    sum( sum( normalized_posterior ) );

% Determine the marginal posterior distributions for alpha and beta
alpha_posterior = sum( normalized_posterior, 2 )';
beta_posterior = sum( normalized_posterior, 1 );

end