%-------------------%
% Stimuli templates %
%-------------------%

% Skip Psychtoolbox synch tests
Screen('Preference', 'SkipSyncTests', 1);

% Add necessary folder paths
Folder_paths

% Specify experiment type
experiment_ver = 3;

% Set debug options
debug = 2;

% Run script to initialize stimuli/responses
Stimulus_initialization

% Adjust robot parameters
if debug == 2
    robotParam( :, 6 ) = 1;
end

%--------------------------%
% Stimulus characteristics %
%--------------------------%

currentFoil = .2;
currentTarget = .5;
targetRotation = datasample( [ 0 1 ], 1 );
currentType = datasample( [ 1 2 3 ], 1 );
onscreenChoices = datasample( 1:2, 1 );
rowIndex = 1;
primeTimeCur = datasample( mainPrimeTimes, 1 );
currentBlockType = 5;

if currentType == 1
    primeRotation = 2;
elseif currentType == 3
    primeRotation = targetRotation;
else
    primeRotation = 1 - targetRotation;
end

if experiment_ver == 2
    correctAnswer = targetRotation;
else
    if onscreenChoices == 1
        % Same
        correctAnswer = 0;
    elseif onscreenChoices == 2
        % Different
        correctAnswer = 1;
    end
end

% Initialize matrix to store results
results = zeros( 1, nCol_results );
trl = 1;

%----------%
% Fixation %
%----------%

FixationDisplay

% Flip to display
Screen('Flip', window);

% Wait for a keystroke on the keyboard:
Post_instruction

%-------------%
% Placeholder %
%-------------%

static_display = 1;

PlaceholderDisplay

% Flip to display
Screen('Flip', window);

% Wait for a keystroke on the keyboard:
Post_instruction

%-------%
% Prime %
%-------%

Target_prime_creation

PrimeDisplay

% Flip to display
Screen('Flip', window);

% Wait for a keystroke on the keyboard:
Post_instruction

%--------%
% Target %
%--------%

TargetDisplay

% Flip to display
Screen('Flip', window);

% Wait for a keystroke on the keyboard:
Post_instruction

%------%
% Mask %
%------%

MaskDisplay

% Flip to display
Screen('Flip', window);

% Wait for a keystroke on the keyboard:
Post_instruction

%--------%
% Choice %
%--------%

ChoiceDisplay

%----------%
% Feedback %
%----------%

Accuracy = resp == correctAnswer;
FeedbackDisplay

%-------------------------%
% Example of single trial %
%-------------------------%

% Introduction

% Color the screen grey
Background

instruct = 'Example of a single trial (in sequence):';
[nx,ny,bbox] = displayInstruct( window, instruct, center, ...
    lnSpace, InstructionTime, ...
    pakSpace );

% Wait for a keystroke on the keyboard:
Post_instruction

% Specify stimulus timing

% Duration of fixation
fixTime = totalTime - preTargetTime - targetTime - maskTime;
% Duration of placeholder
placeTime = preTargetTime - primeTimeCur;
% Create vector of durations
FT = [ ...
    fixTime ...
    placeTime ...
    primeTimeCur ...
    targetTime ...
    maskTime ...
    ];

% Create textures for prime/target stimuli
Target_prime_creation

% Run script for generating single trial
Gabor_priming_trial

% Log data
Data_logger

%--------------%
% Progress bar %
%--------------%

blockProgress( window, center, 1, 10, '1', ...
    [ 150 .5 pakSpace  lnSpace 300 ] )

% Wait for a keystroke on the keyboard:
Post_instruction

%--------------------%
% End example script %
%--------------------%

% Color the screen grey
Background

instruct = 'Press any button to end the task.';
[nx,ny,bbox] = displayInstruct( window, instruct, center, ...
    lnSpace, InstructionTime, ...
    pakSpace );

% Wait for a keystroke on the keyboard:
KbStrokeWait;

% Clear the screen
clear Screen;
sca;