%-----------------%
% Adaptive trials %
%-----------------%

% Parameter grid
alpha_val = linspace( boundaries(1), boundaries(2), alpha_prec );
beta_val = linspace( boundaries(1), boundaries(2), beta_prec );

% Priors
if adaptiveInc == 1
    priors = origPriors;
    alpha_prior = normpdf( alpha_val, priors(1), priors(2) ) ./ ...
        ( normcdf( boundaries(2), priors(1), priors(2) ) - ...
        normcdf( boundaries(1), priors(1), priors(2) ) );
    beta_prior = normpdf( beta_val, priors(3), priors(4) ) ./ ...
        ( normcdf( boundaries(4), priors(3), priors(4) ) - ...
        normcdf( boundaries(3), priors(3), priors(4) ) );
    % Initialize contrast difference
    x = x_lev( round(size(x_lev,2)/2) );
end

% Define prime durations
primeTimes = [ 50 400 ];
% Extract foil contrast levels
foilContrasts = foilContrastDefault;
% Define target/foil ratios
targetRatios = 2;
% Set prime type
primeTypes = 0;
% Set number of trials and blocks
if adaptiveInc == 1
    nTrials = 20;
else
    nTrials = 8;
end
% Set values for correct answers
correctAnswers = [ 0 1 ];

% Generate conditions
Generate_conditions

% Randomize the presentation order
TrialOrder = randperm( size( AllCnd, 1 ) );

%%% Adaptive trials %%%

% Determine number of trials
totalTrials = TotalCnd*nTrials;

% Create matrix to store current results
results = zeros(totalTrials,11);

for trl = 1:totalTrials
    
    sel = TrialOrder(trl);
    
    % Column 1: Foil contrasts
    % Column 2: Target/foil ratios
    % Column 3: Target contrasts
    % Column 4: Prime durations
    % Column 5: Prime type
    % Column 6: Correct answer
    
    currentFoil = AllCnd(sel,1);
    currentTarget = exp( x );
    primeTimeCur = AllCnd(sel,4);
    correctAnswer = AllCnd(sel,6);
    
    % Determine prime type
    currentType = AllCnd(sel,5);
    if currentType == 0
        NeitherPrime = 1;
        currentPrime = correctAnswer;
    elseif currentType == 1
        NeitherPrime = 0;
        currentPrime = correctAnswer;
    elseif currentType == 2
        NeitherPrime = 0;
        currentPrime = 1 - correctAnswer;
    end
    
    % Determine timing for stimuli
    placeTime = placeholderTime - primeTimeCur;
    fixTime = totalTime - placeTime - primeTimeCur - targetTime - ...
        targetMaskTime;
    FT = [ fixTime, placeTime, primeTimeCur, targetTime, targetMaskTime ];
    
    % Based on target value, set rotation angle
    rot = datasample( theta, 1 );
    thetaT = rot;
    if correctAnswer == 0
        thetaT = thetaT - 90;
    end
    
    % Create the gabor patch
    gabor = Overlaid_Gabors(imSize,PP,currentTarget,currentFoil,thetaT,stripes,stimWin,0);
    % Create possible prime
    if currentType == 0
        gaborPrime = Overlaid_Gabors(imSize,PP,1,0,90,stripes,choiceR,0);
    elseif currentType == 1
        gaborPrime = Overlaid_Gabors(imSize,PP,1,0,thetaT,stripes,choiceR,0);
    elseif currentType == 2
        if correctAnswer == 1
            gaborPrime = Overlaid_Gabors(imSize,PP,1,0,thetaT-90,stripes,choiceR,0);
        else
            gaborPrime = Overlaid_Gabors(imSize,PP,1,0,rot,stripes,choiceR,0);
        end
    end
    static_display = 1;
    
    % Run script
    Gabor_priming_trial
    
    % Save responses
    Choice = resp;
    ResponseTime = RT;
    Accuracy = correctAnswer == resp;
    PrimeType = 0;
    if NeitherPrime == 0
        if currentPrime == correctAnswer
            PrimeType = 2;
        else
            PrimeType = 1;
        end
    end
    
    results(trl,:) = [ RT resp Accuracy correctAnswer ...
        currentType primeTimeCur currentFoil currentTarget PP rot blckType ];
    
    if ( Accuracy == 1 )
        DrawFormattedText(window, 'Correct!', 'center','center', [ 0 0 0 ] );
    else
        DrawFormattedText(window, 'Wrong!', 'center','center', [ 0 0 0 ] );
    end
    
    % Flip to the screen
    Screen('Flip', window);
    WaitSecs(feedbackTime);
    
    % Close textures
    Screen('Close');
    
    % Determine the posterior using a grid approximation method
    [ posterior, alpha_post, beta_post ] = grid_approx( Accuracy, x, ...
        alpha_val, beta_val, alpha_prior, beta_prior );
    
    % Update the priors using the new marginalized posteriors for
    % alpha/beta
    alpha_prior = alpha_post;
    beta_prior = beta_post;
    
    % Design optimization
    [ ~, new_x ] = utility_f( Accuracy, x_lev, alpha_val, beta_val, ...
        alpha_prior, beta_prior );
    
    % Get the new sensory level to use in the next trial
    x = new_x;
    
end

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

% Determine contrast level that should produce 75% correct
x = g_alpha_beta( .75, alpha_grid(aI,bI), beta_grid(aI,bI) );
targetThreshold = exp( x );
