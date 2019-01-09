function [ utility, new_x  ] = utility_f( y, x_lev, alpha_val, ...
    beta_val, alpha_prior, beta_prior )
% Purpose:
% A function to determine the next sensory level that provides the most 
% information (i.e. parameter estimates with low variance) to use in a 
% subseqeunt experimental trial.
% Arguments:
% y           - the observed accuracy ( y = { 0, 1 } )
% x_lev       - a sequence of the possible sensory levels
% alpha.val   - A sequence of the possible values for alpha
% beta.val    - A sequence of the possible values for beta
% alpha.prior - the (updated) associated prior density for each 
%               possible value of alpha
% beta.prior  - the (updated) associated prior density for each 
%               possible value of beta
% Notes:
% Use the 'grid.approx' function to determine the updated priors 
% needed for this function (i.e. set the priors equal to the 
% marginal posteriors).
% Returns:
% 1) the utility values associated with each sensory level
% 2) the sensory level with the lowest average variance for
%    the parameter estimates

xN = size( x_lev, 2 );

% Design optimization
U = zeros( xN, 2 );

% Loop through the discretized sensory levels using the newly updated 
% priors
for xn = 1:xN
    % Determine the resulting posterior given the new priors for each 
    % sensory level
    [ ~, alpha_slice, beta_slice ] = grid_approx( y, ...
        x_lev(1,xn), alpha_val, beta_val, alpha_prior, beta_prior );
    
    % The expected values
	expected_alpha = sum( alpha_val .* alpha_slice );
	expected_beta = sum( beta_val .* beta_slice );
    
    % The variances
	var_alpha = sum( (alpha_val .^ 2) .* alpha_slice ) - ...
        expected_alpha .^ 2;
	var_beta = sum( (beta_val .^ 2) .* beta_slice ) - ...
        expected_beta .^ 2;
    
    % Calculate the ratio of E(x)/Var(x) to standardize
    U(xn,1) = expected_alpha/var_alpha;
    U(xn,2) = expected_beta/var_beta;
end

% Determine the utility (average of the ratios across parameters)
utility = sum( U, 2)/2;

% Pick the sensory level with the highest corresponding average
new_x = max( x_lev( utility == max( utility ) ) );

end
