%-------------------------%
% Example of instructions %
%-------------------------%

% Skip Psychtoolbox synch tests
Screen('Preference', 'SkipSyncTests', 1);

% Add necessary folder paths
Folder_paths

% Specify experiment type
sessionNumber = 2;

% Set debug options
debug = 0;

% Run script to initialize stimuli/responses
Stimulus_initialization

% Adjust simulation parameters
if debug == 2
    robotParam( :, 6 ) = 1;
end

% Display the instructions
Task_instructions

% Indicate that the experiment is finished
instruct = 'Example instructions are done! Press any button to end the task.';
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, pakSpace );

% Wait for a keystroke on the keyboard:
KbStrokeWait;

% Clear the screen
clear Screen;
sca;