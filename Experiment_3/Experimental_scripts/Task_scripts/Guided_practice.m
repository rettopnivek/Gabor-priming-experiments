%-----------------%
% Practice trials %
%-----------------%

% Define prime durations
primeTimes = 100;
% Extract foil contrast levels
foilContrasts = foilContrastDefault;
% Define target/foil ratios
targetRatios = 5;
% Set prime type
primeTypes = 0;
% Set number of trials and blocks
nTrials = 10;
% Set values for correct answers
correctAnswers = [ 0 1 ];

%%% Training set %%%

% Determine number of trials
totalTrials = 5; % Maximum number of possible trials

% Create matrix to store current results
results = zeros(totalTrials,11);

% Guided practice introduction

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

instruct = [ ...
    'You will now complete 5 examples, guided by the experimenter.\n' ...
    'In these examples, the grid will be shown very slowly, to\n' ...
    'to help you understand the task.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, pakSpace );

for trl = 1:totalTrials
        
    currentFoil = foilContrastDefault;
    currentTarget = foilContrastDefault * targetRatios;
    primeTimeCur = primeTimes;
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
    guidedPractice = 400;
    placeTime = placeholderTime - primeTimeCur;
    fixTime = totalTime - placeTime - primeTimeCur - guidedPractice - ...
        targetMaskTime;
    FT = [ fixTime, placeTime, primeTimeCur, guidedPractice, targetMaskTime ];
    
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
    
end