%------------------%
% Staircase trials %
%------------------%

% Define parameters
crit = .75; % Desired level of accuracy
N_width = 10; % Width of interval for moving average
N_int = 8; % Number of intervals to assess
interval = zeros( 1, N_width );
cnt = 1; % Initialize count variable
inc = 1;

% Define window within which no changes should occur
stable_win = binoinv( [.25,.75], N_width, crit )/N_width;

% Size of change to log of target contrast during each interval
x_change = linspace( .5, .1, N_width );

% Define prime durations
primeTimes = [ 50 400 ];
% Extract foil contrast levels
foilContrasts = foilContrastDefault;
% Define target/foil ratios
targetRatios = 2;
% Set prime type
primeTypes = 0;
% Set values for correct answers
correctAnswers = [ 0 1 ];

% Initial starting value for log of target contrast
x_cur = log( targetRatios * foilContrasts );

%%% Adaptive trials %%%

% Determine number of trials
totalTrials = N_int * N_width;

% Create matrix to store current results
results = zeros(totalTrials,11);

for trl = 1:totalTrials
    
    currentFoil = foilContrastDefault;
    currentTarget = exp( x_cur );
    primeTimeCur = datasample( primeTimes, 1 );
    correctAnswer = datasample( correctAnswers, 1 );
    
    % Determine prime type
    currentType = primeTypes;
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
    fixTime = totalTime - placeTime - primeTimeCur - targetTime;
    FT = [ fixTime, placeTime, primeTimeCur, targetTime ];
    
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
    
    % Apply staircase algorithm
    [ est_ac, x_cur, cnt, interval ] = staircaseAlgorithm( Accuracy, ...
        x_cur, x_change(inc), interval, stable_win, cnt );
    if cnt == 1
        inc = inc + 1;
    end
    
end

allResults = [ allResults; results ];