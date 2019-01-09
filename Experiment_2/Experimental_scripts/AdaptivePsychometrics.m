%-----------------------------------------------%
% Script to run adaptive block of gabor priming %
% for psychometric curve determination          %
%-----------------------------------------------%

% Add path for functions to fit basic psychophysics model
orig_dir = cd('Psychophysics');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

% Randomly shuffle adaptiveOrder
adaptiveOrd = randperm( size( adaptiveCnd, 1 ) );

% Define current block type
blckType = 1;

% Set number of trials to loop over
loop = size( adaptiveCnd, 1 );
% For later blocks
if blck > 2
    loop = trialsAdaptiveLoop;
end

% Matrix to record results
% 1) RT, 2) Choice, 3) Accuracy 4) Target, 
% 5) Prime 6) Prime duration 7) Contrast 8) Block
% 9) Rotation angle 10 - 13) Stimulus timing 14) Type
results = zeros( loop, 14);

for trl = 1:loop
    
    % Set contrast difference
    cntrstPrp = exp(x);
    cntrstLure = .5*cntrstPrp;
    
    % Create noise patch
    noiseWin = Noise_function(imSize,gaussWinNoise,0,1);
    noiseWinTex = Screen('MakeTexture', window, noiseWin);
    
    % Set up variables for each experimental trial
    primeTimeCur = adaptiveCnd( adaptiveOrd(trl), 1 );
    fixTime = totalTime - primeTimeCur - targetTime - ...
        postMaskTime;
    FT = [ fixTime, primeTimeCur, targetTime, postMaskTime ];
    
    % Randomly select what the prime and target will be
    currentPrime = adaptiveCnd( adaptiveOrd(trl), 3 );
    currentTarget = adaptiveCnd( adaptiveOrd(trl), 2 );
    
    if ( currentTarget == 1 )
        cntrst = .5 + cntrstLure;
    else
        cntrst = .5 - cntrstLure;
    end
    
    % Create the gabor patch
    rot = datasample( rotRange, 1 );
    gabor = Overlaid_Gabors(imSize,stripes,amp,cntrst,rot,gaussWin,0,1,stepYes);
    gaborTex = Screen('MakeTexture', window, gabor);
    
    PrimeYes = 1;
    % Run script
    Gabor_priming_trial
    
    % Save responses
    Accuracy = currentTarget == resp;
    y = Accuracy;
    results(trl,:) = [ RT resp Accuracy currentTarget currentPrime ...
        primeTimeCur cntrstPrp blck rot time_check blckType ];
    
    % Color the screen grey
    Screen('FillRect', window, [ .5 .5 .5 ]);
    
    % Color the screen grey
    Screen('FillRect', window, [ .5 .5 .5 ]);
    
    if yesFeedback==1
        % Report feedback
        if ( Accuracy == 1 )
            DrawFormattedText(window, 'Correct!', 'center','center', [ 0 0 0 ] );
        else
            DrawFormattedText(window, 'Wrong!', 'center','center', [ 0 0 0 ] );
        end
    end
    
    % Flip to the screen
    Screen('Flip', window);
    WaitSecs(.5);
    
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
    
end

% Store practice results
allResults = [ allResults; results ];

alpha_grid = meshgrid( alpha_val )';
beta_grid = meshgrid( beta_val );
vec_post = posterior(:);
[ ~, I ] = max( vec_post );

% Indices for parameter matrices
[aI, bI ] = ind2sub( size(posterior), I );

%{
% Contour plot of posterior
contourf(beta_grid,alpha_grid,posterior);
hold on;

% Maximum
plot( beta_grid(aI,bI), alpha_grid(aI,bI), ...
    'r.','MarkerSize',20 );
%}

% Determine contrast level that should produce 70% correct
x = g_alpha_beta( crt, alpha_grid(aI,bI), beta_grid(aI,bI) );
cntrstPrp = exp( x );

% Provide feeback at the end of the block
Block_feedback
