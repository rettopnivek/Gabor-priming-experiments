%%% Adaptive design example %%%

% True parameters
tp = [ rand(1,1)*4.5 + .5, rand(1,1)*4 ];

% Grid precision
alpha_prec = 50;
beta_prec = 50;

% Create a discretized range of sensory levels
x_lev = linspace( -7, 0, 50 );

% Vector of parameter values
boundaries = [ 0, 7, 0, 7 ];

% Parameter grid
alpha_val = linspace( boundaries(1), boundaries(2), alpha_prec );
beta_val = linspace( boundaries(1), boundaries(2), beta_prec );

% Priors
alpha_prior = normpdf( alpha_val, 3, 3 ) ./ ...
    ( normcdf( 7, 3, 3 ) - normcdf( 0, 3, 3 ) );
beta_prior = normpdf( beta_val, 3.5, 3 ) / ...
    ( normcdf( 7, 3.5, 3 ) - normcdf( 0, 3.5, 3 ) );

% Save the original starting priors
alpha_start_prior = alpha_prior;
beta_start_prior = beta_prior;

% Number of trials
nTrials = 80;

% Vectors to hold simulated data
accuracy = zeros( 1, nTrials );
sensory_levels = zeros( 1, nTrials );

%%% Trial 1 %%%

% Define a starting sensory level
x = x_lev( round(size(x_lev,2)/2) );

% Simulate an initial observation using the true parameters
y = binornd(1, f_alpha_beta( x, tp(1), tp(2) ) );

% Store the results
accuracy(1) = y; sensory_levels(1) = x;

%%% Subsequent trials %%%

for trl = 2:nTrials
    
    % Determine the posterior using a grid approximation method
	[ posterior, alpha_post, beta_post ] = grid_approx( y, x, ...
        alpha_val, beta_val, alpha_prior, beta_prior );
    
    % Update the priors using the new marginalized posteriors for 
    % alpha/beta
	alpha_prior = alpha_post;
	beta_prior = beta_post;
    
    % Design optimization
	[ ~, new_x ] = utility_f( y, x_lev, alpha_val, beta_val, ...
        alpha_prior, beta_prior );
    
    % Get the new sensory level to use in the next trial
	x = new_x;
	
	% Simulate the next observation
    y = binornd(1, f_alpha_beta( x, tp(1), tp(2) ) );
    
    % Store the results
	accuracy(trl) = y; sensory_levels(trl) = x;
    
end

%%% Plot the results %%%

alpha_grid = meshgrid( alpha_val )';
beta_grid = meshgrid( beta_val );
vec_post = posterior(:);
[ ~, I ] = max( vec_post );

% Indices for parameter matrices
[aI, bI ] = ind2sub( size(posterior), I );

% Contour plot of posterior
contourf(beta_grid,alpha_grid,posterior);
hold on;

% Maximum
plot( beta_grid(aI,bI), alpha_grid(aI,bI), ...
    'r.','MarkerSize',20 );

% True parameters
hold on;
plot( tp(2), tp(1) , 'b.','MarkerSize', 20 );