%-------------------------%
% Example of instructions %
%-------------------------%

% Skip Psychtoolbox synch tests
Screen('Preference', 'SkipSyncTests', 1);
% Set debug status
debug = 0;
PrimeVer = 1;
StudyType = 1;

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

% Add path for task scripts
orig_dir = cd('Task_scripts');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

% Run script to initialize stimuli/responses
Stimulus_initialization

% Display the instructions
Task_instructions

% Indicate that the experiment is finished
instruct = 'Example instructions are done! Press any button to end the task.';
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, pakSpace );

% Clear the screen
clear Screen;
sca;