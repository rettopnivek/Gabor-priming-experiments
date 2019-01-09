%------------------------%
% Main experiment script %
%------------------------%

%{
Purpose:
Matlab and Psychtoolbox code for running variants of a priming task using
overlaid stripes based on Gabor patches.

Requirements:
Matlab R2015a
Psychtoolbox

Outputs:
A .csv file with the stimuli and response information
A .mat file with the starting and ending time, and the stimuli ordering
info
A text file with demographics information

Notes:
- To prematurely end the experiment, press alt + Tab and close the 'PTB'
  window.
- Thanks is extended to Will Hopper for help with the code in Matlab
- Use the key shortcut Win+P to change from dual to single monitor (Win is
  the key with the Windows logo).
%}

% Add necessary folder paths
Folder_paths

%%% Experiment set up %%%

% Record when the experiment begins
startTime = clock;

%%% Specify subject ID %%%
Subject_ID_input

%%% Demographics %%%
if sessionNumber == 1
    Demographics
end

%%% Set debug options %%%
if debug > 0
    % Skip Psychtoolbox synch tests
    Screen('Preference', 'SkipSyncTests', 1);
end

% Run script to initialize stimuli/responses
Stimulus_initialization

% If specified, add delay to responses
if debug == 2
    robotParam( :, 6 ) = 1;
end

% Initialize empty matrix to store all results
allResults = [];

%%% Instructions %%%
Task_instructions

%%% Experimental trials %%%

% Run guided practice and training trials
Guided_practice
Training_trials

% Run practice trials
Practice_trials

% Staircase approach for calibration trials
Staircase_trials

% Loop over blocks
nBlocks = 10;
for nb = 1:nBlocks
    
        % Color the screen grey
        Background
        
        % Block progress
        string = ' ';
        blockProgress( window, center, nb, nBlocks, ...
            string, [ 150 .5 pakSpace  lnSpace 300 ] );
        
        % Wait for a keystroke on the keyboard:
        Post_instruction

    % Run main study
    Priming_trials
    
end

%%% End of the experiment %%%

% Record end of experiment
endTime = clock;

% Navigate to the directory for subject data
cd('Subjects');

    % Save data as .csv and .mat
    Save_results

% Color the screen grey
Background

instruct = [ ...
    'Congratulations, you have finished the study! \n' ...
    'Thanks again for your participation! \n' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, ...
    lnSpace, InstructionTime, pakSpace );

% Clear the screen
clear Screen;
sca;

% Return to original directory
cd('..');

% Run script to add to experiment log
Experiment_log
