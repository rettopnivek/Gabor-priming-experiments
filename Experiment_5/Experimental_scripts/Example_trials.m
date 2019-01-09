%-------------------------------------------------%
% An set of example trials for debugging purposes %
%-------------------------------------------------%

% Skip Psychtoolbox synch tests
Screen('Preference', 'SkipSyncTests', 1);
% Set debug status
debug = 0;

% Add path for additional functions
orig_dir = cd('Internal_functions');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

% Add path for trial scripts
orig_dir = cd('Stimuli');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

% Add path for psychophysics functions
orig_dir = cd('Psychophysics');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

% Add path for psychophysics functions
orig_dir = cd('Task_scripts');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

% Dialog boxes to set trial options
boxSize = [ 300, 150 ]; % Set the width and height of the dialog box
prompt = {...
    'Prime types (1 = Neither primed, 2 = target/foil, 3 = all)' ...
    'Prime durations (in ms)' ...
    'Foil contrasts (Between 0-1)' ...
    'Ratio of target/foil contrast (1 or greater)' ...
    'Number of trials per condition' ...
    'Reload? 0 = no, 1 = yes' ...
    'Save? 0 = no, 1 = yes' ...
    };
dlg_title = 'Options';
num_lines = 1;
exampleOptions = inputdlg(prompt,dlg_title,num_lines);
% This line prevents Matlab from freezing following the dialog box
drawnow; pause(0.05);

% Extract options for reloading and saving
reloadYes = str2num( exampleOptions{6} );
saveYes = str2num( exampleOptions{7} );

% Extract prime durations
primeTimes = str2num( exampleOptions{2} );
% Extract foil contrast levels
foilContrasts = str2num( exampleOptions{3} );
% Extract target/foil ratios
targetRatios = str2num( exampleOptions{4} );
% Set prime type
if exampleOptions{1} == '1'
    primeTypes = [ 0 ];
elseif exampleOptions{1} == '2'
    primeTypes = [ 1 2 ];
elseif exampleOptions{1} == '3'
    primeTypes = [ 0 1 2 ];
end
% Set number of trials and blocks
nTrials = str2num( exampleOptions{5} );
% Set values for correct answers
correctAnswers = [ 0 1 ];

% Set prime version (1 = dots, 2 = Gabor patch)
PrimeVer = 2;

% Run script to initialize stimuli/responses
Stimulus_initialization

% Generate conditions
Generate_conditions

% Randomize the presentation order
TrialOrder = randperm( size( AllCnd, 1 ) );

%%% Example trials %%%

% Load in previous results or create new empty vector to store results
if reloadYes == 1
    cd('Example_results');
    load('Example_results.mat');
    cd(orig_dir);
else
    allResults = [];
end

totalTrials = TotalCnd*nTrials;

% Determine size of a block of trials
if totalTrials > 50
    
    blockSize = 0;
    chks = [ 60 64 65 70 75 80 ];
    for i = 1:size(chks,2)
        
        tst = round(totalTrials/chks(i)) ~= totalTrials/chks(i);
        if tst == 0
            blockSize = chks(i);
        end
    end
    
else
    blockSize = totalTrials;
end

results = zeros(totalTrials,10);

nBlocks = totalTrials/blockSize; % Total number of blocks
trk = blockSize+1;
blck = 1;
for trl = 1:totalTrials
    
    if trk > blockSize
        
        % Color the screen grey
        Screen('FillRect', window, [ .5 .5 .5 ]);
        
        % Block progress
        string = ' ';
        blockProgress( window, center, blck, nBlocks, string, [ 150 .5 pakSpace  lnSpace 300 ] );
        trk = 1;
        blck = blck + 1;
    else
        trk = trk + 1;
    end
    
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
    
    placeTime = placeholderTime - primeTimeCur;
    fixTime = totalTime - placeTime - primeTimeCur - targetTime;
    FT = [ fixTime, placeTime, primeTimeCur, targetTime];
    
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
        currentType primeTimeCur currentFoil currentTarget PP rot ];
    
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

if saveYes == 1
    cd('Example_results');
    % Save a csv file
    fid = fopen('Example_results.csv', 'wt'); % Open for writing
    fprintf(fid, 'RT,Choice,Accuracy,Target,PrimeType,PrimeDuration,FoilContrast,TargetContrast,PP,Angle\n');
    for i=1:size(allResults,1)
        fprintf(fid, '%d,', allResults(i,1:(size(allResults,2)-1)));
        fprintf(fid, '%d', allResults(i,size(allResults,2)));
        fprintf(fid, '\n');
    end
    fclose(fid);
    
    % Save a matlab file
    save('Example_results.mat','allResults');
    
    % Return to original directory
    cd(orig_dir);
end

% Indicate that the experiment is finished
instruct = [ 'Example trials are done! Press any button to end the task.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, pakSpace );

% Clear the screen
clear Screen;
sca;
