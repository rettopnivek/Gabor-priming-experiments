%-------------------------------------------------%
% An set of example trials for debugging purposes %
%-------------------------------------------------%

% Skip Psychtoolbox synch tests
Screen('Preference', 'SkipSyncTests', 1);

% Add necessary folder paths
Folder_paths

% Dialog boxes to set trial options
boxSize = [ 300, 150 ]; % Set the width and height of the dialog box
prompt = {...
    % Line 1
    sprintf( ...
    [ 'Debug status, prime types, and trial number' ...
    '\n(0 - 2, 1 - 3, 1 onwards)' ] ) ...
    % Line 2
    'Foil contrasts (Between 0-1)' ...
    % Line 3
    'Ratio of target/foil contrast (1 or greater)' ...
    % Line 4
    'Prime durations (up to 267 ms)' ...
    % Line 5
    'Reload, and/or save? 0 = no, 1 = yes' ...
    };
dlg_title = 'Options';
num_lines = 1;
exampleOptions = inputdlg(prompt,dlg_title,num_lines);
% This line prevents Matlab from freezing following the dialog box
drawnow; pause(0.05);

% Extract options for reloading and saving
tmp = str2num( exampleOptions{5} );
reloadYes = tmp(1); saveYes = tmp(2);

% Settings for experiment type
tmp = str2num( exampleOptions{1} );

% Extract debug status, where...
% 0 = User inputs
% 1 = Rapid simulated responses
% 2 = Simulated responses
debug = tmp(1);
if debug == 2
    robotParam( :, 6 ) = 1;
end

% Extract the type of primes to include, where...
% 1 = Only neither primed
% 2 = Only target/foil primed
% 3 = All prime types
% Set prime type
if tmp(2) == 1
    primeTypes = [ 1 ];
elseif tmp(2) == 2
    primeTypes = [ 2 3 ];
elseif tmp(2) == 3
    primeTypes = [ 1 2 3 ];
end

% Set number of trials and blocks
nTrials = tmp(3);

% Extract foil contrast levels
foilContrasts = str2num( exampleOptions{2} );
% Extract target/foil ratios
targetRatios = str2num( exampleOptions{3} );
% Extract prime durations
examplePrimeTimes = str2num( exampleOptions{4} );

% Create names for output files
csvOutputFile = 'Example_results.csv';
matOutputFile = 'Example_results.mat';
% Subject ID number
SubjNum = 666;

% Run script to initialize stimuli/responses
Stimulus_initialization

% Define prime times
primeTimes = examplePrimeTimes;

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

% Determine total number of trials
totalTrials = size( AllCnd, 1 );

% Determine size of a block of trials
if totalTrials > 50
    
    blockSize = 0;
    chks = [ 54 60 64 65 70 75 80 ];
    for i = 1:size(chks,2)
        
        tst = round(totalTrials/chks(i)) ~= totalTrials/chks(i);
        if tst == 0
            blockSize = chks(i);
        end
    end
    
else
    blockSize = totalTrials;
end

% Initialize matrix to store results
results = zeros(totalTrials,nCol_results);

nBlocks = totalTrials/blockSize; % Total number of blocks
trk = blockSize+1;

% Loop over trials
for trl = 1:totalTrials
    
    % At start of block, display
    % progress bar
    if trk > blockSize
        
        % Color the screen grey
        Background
        
        % Block progress
        string = ' ';
        blockProgress( window, center, currentBlock, nBlocks, ...
            string, [ 150 .5 pakSpace  lnSpace 300 ] );
        
        % Wait for a keystroke on the keyboard:
        Post_instruction
        
        trk = 1;
        currentBlock = currentBlock + 1;
    else
        trk = trk + 1;
    end
    
    % Define block type
    currentBlockType = 5;
    
    % Set trial index
    currentTrial = trl;
    
    % Extract relevant conditions,
    % run priming trial, and
    % save results
    Embedded_trial_in_loop
    
end


% Store results into final trial
allResults = [ allResults; results ];

if saveYes == 1
    % Navigate to folder for example results
    cd('Example_results');
    
    % Save data as .csv and .mat
    Save_results
    
    % Return to original directory
    cd(orig_dir);
end

% Indicate that the experiment is finished
instruct = [ 'Example trials are done! Press any button to end the task.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, pakSpace );

% Wait for a keystroke on the keyboard:
Post_instruction

% Clear the screen
clear Screen;
sca;
