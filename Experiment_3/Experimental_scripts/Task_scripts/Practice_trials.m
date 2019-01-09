%-----------------%
% Practice trials %
%-----------------%

% Define prime durations
primeTimes = 100;
% Extract foil contrast levels
foilContrasts = foilContrastDefault;
% Define target/foil ratios
targetRatios = [ 2 1.5 1.25 1.125 ];
% Set prime type
primeTypes = 0;
% Set number of trials and blocks
nTrials = 10;
% Set values for correct answers
correctAnswers = [ 0 1 ];

% Generate conditions
Generate_conditions

% For each contrast level, randomize the presentation order
TrialOrder = zeros( 1, size( AllCnd, 1 ) );
tmp = 1:size(AllCnd,1);
tmp2 = unique( AllCnd(:,2) );
for i = 1:size( targetRatios, 2 )
    tmp3 = tmp( AllCnd(:,2) == tmp2( ( size(tmp2,1) + 1 ) - i ) );
    TrialOrder( ( 1:(nTrials*2) ) + (nTrials*2)*(i-1) ) = ...
        tmp3( randperm( nTrials*2 ) );
end

clear tmp tmp2 tmp3 i;

%%% Practice trials %%%

% Determine number of trials
totalTrials = TotalCnd*nTrials;

% Create matrix to store current results
results = zeros(totalTrials,11);

% Practice block introduction

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

instruct = [ 'As additional practice, you will now complete a sequence \n' ...
    'of trials that get progressively harder.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, pakSpace );

for trl = 1:totalTrials
    
    sel = TrialOrder(trl);
    
    % Column 1: Foil contrasts
    % Column 2: Target/foil ratios
    % Column 3: Target contrasts
    % Column 4: Prime durations
    % Column 5: Prime type
    % Column 6: Correct answer
    
    currentFoil = AllCnd(sel,1);
    currentTarget = AllCnd(sel,3);
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
end

allResults = [ allResults; results ];