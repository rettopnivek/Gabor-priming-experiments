%----------------------%
% Psychophysics trials %
%----------------------%

% Define prime durations
primeTimes = [ 50 400 ];
% Extract foil contrast levels
foilContrasts = foilContrastDefault;
% Define target/foil ratios
targetRatios = [ 1 1.14 1.18 1.23 1.28 1.34 1.42 1.51 1.64 2.54 ];
% Set prime type
primeTypes = 0;
% Set number of trials and blocks
nTrials = 9;
% Set values for correct answers
correctAnswers = [ 0 1 ];

if blck == 2
    % Generate conditions
    Generate_conditions
    
    % Rename the condition matrix
    PsychoCnd = AllCnd;
    
    % Randomize the presentation order
    PsychoTrialOrder = randperm( size( PsychoCnd, 1 ) );
    
    % Create an increment
    PsychoInc = 1;
end

%%% Pschophysics trials %%%

totalTrials = 60;

results = zeros(totalTrials,11);

for trl = 1:totalTrials
        
    sel = PsychoTrialOrder(PsychoInc);
    
    % Column 1: Foil contrasts
    % Column 2: Target/foil ratios
    % Column 3: Target contrasts
    % Column 4: Prime durations
    % Column 5: Prime type
    % Column 6: Correct answer
    
    currentFoil = PsychoCnd(sel,1);
    currentTarget = PsychoCnd(sel,3);
    primeTimeCur = PsychoCnd(sel,4);
    correctAnswer = PsychoCnd(sel,6);
    
    currentType = PsychoCnd(sel,5);
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
    
    % Increment trial
    PsychoInc = PsychoInc + 1;
end

allResults = [ allResults; results ];
