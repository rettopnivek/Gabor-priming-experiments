%--------------------------------------%
% Stimulus and response initialization %
%--------------------------------------%

rng shuffle; % Random seed for rng

% Initialize Psychtoolbox
PTB_initial

%{
% Check if framerate is 60 Hz
if (ifi > .017)
    % Indicate that the monitor is set to the wrong refresh rate
    warning = 'Please press shift+2 and set monitor frame rate to at least 60 Hz';
    DrawFormattedText(window, warning,'center','center',[ 0 0 0 ] );
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    % Quit Matlab
    quit force;
end
%}

% Check if framerate is 120 Hz
if (ifi > .0086 )
    % Indicate that the monitor is set to the wrong refresh rate
    warning = 'Please press shift+2 and set monitor frame rate to at least 120 Hz';
    DrawFormattedText(window, warning,'center','center',[ 0 0 0 ] );
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    % Quit Matlab
    quit force;
end

% Hide the mouse curser
HideCursor();

%%% General settings %%%

% Color values
color_black = [ 0, 0, 0 ];
color_white = [ 1, 1, 1 ];
color_grey = [ .5, .5, .5 ];
color_light_grey = [ .75, .75, .75 ];
color_red = [ 1, 0, 0 ];
color_blue = [ 0, 0, 1 ];

% Set the font and text size
Screen('TextFont',window,'Monospace');
TxtSz = 20;
Screen('TextSize',window,TxtSz);

% Set distance from center for the 'Press any key' phrase
pakSpace = 250;
% Set line spacing for instructions
lnSpace = 2.5;
% Set interval before 'continue' option appears
InstructionTime = 2;

% Number of columns for matrix to store results
nCol_results = 24;

%%% Set options for making responses %%%

% Set the interval before a time-out response is recorded (in seconds)
timeout = 4.995;

% Set keys for responses
keys = 'jk';
% Define labels for key assignments
if experiment_ver == 2
    keyAssignments = { 'Left', 'Right' };
else
    keyAssignments = { 'Same', 'Different' };
end
keyOptions = getKeyAssignments(keys,1,300);
% Variables to pass into 'keyboardResponses'
keys2AFC = keyOptions(1:2);
assignedValues = [ 0 1 ];
% Load in parameters for simulating responses
Robot_parameters
% Set variables for correct answers
correctAnswers = [ 0 1 ];

%%% Initialize variables for stimulus presentation %%%

%%% Annulus

% Set the dimensions for the annulus
ovalRadius1 = 24;
ovalSize1 = [ center(1) - ovalRadius1, center(2) - ovalRadius1, ...
    center(1) + ovalRadius1, center(2) + ovalRadius1 ];
ovalRadius2 = ovalRadius1 + 26;
ovalSize2 = [ center(1) - ovalRadius2, center(2) - ovalRadius2, ...
    center(1) + ovalRadius2, center(2) + ovalRadius2 ];

%%% Fixation

% Set the dimensions and color of the fixation dot
dotSizePix = 10;
dotColor = color_black;

%%% Prime

% Set the dimensions for the ovals of the clock prime/mask
RadiusPrp = .8;
nPoints = 15; % Number of ovals to draw for mask

% Define prime durations
if experiment_ver == 2
    mainPrimeTimes = [ 8 34 134 ];
    % mainPrimeTimes = [ 17 34 134 ]; % 60 Hz
else
    mainPrimeTimes = [ 34 134 ];
end

%%% Target

% Set image size for gabor patch\noise
imSize = 100;
% Set number of stripes in gabor patch ( 5 = ~3 visible stripes )
stripesTarget = 5;
stripes = 2;
% Set radius for circle containing gabor patches
stimWin = .24;
% Set angle of gratings
theta = [ 35 40 45 50 55 ];
% Set illuminance for maximum peak of the 4 intensities in the patch
PP = 76;
% Set value for foil contrast
foilContrastDefault = .05;
% Set rotation values for target, where...
% 0 = left, 1 = right
targetRotations = [ 0 1 ];

%%% Choice

% Set width of line for choice comparison
choiceWdth = [.2,.125];
% Set radius of circle containing choice line
choiceR = [.5,.24];

% For third experiment, create same/different
% onscreen alternatives
if experiment_ver == 3
    
    % Set patches above and below each other in the center
    vShift = 15;
    topPos = [ center(1) - imSize/2, ...
        center(2) - imSize - vShift, ...
        center(1) + imSize/2, ...
        center(2) - vShift ];
    botPos = [ center(1) - imSize/2, ...
        center(2) + vShift, ...
        center(1) + imSize/2, ...
        center(2) + imSize + vShift ];
    
    % Set vertical position for text
    vChoicePos = center(2) + imSize + vShift + 25;
    
    % Define onscreen choice alternatives
    allOnscreenChoices = [ 1 2 ];
    
else
    
    % Single onscreen choice (None)
    allOnscreenChoices = 3;
end

%%% Stimulus timing

if experiment_ver == 2
    % Total time of stimuli displays (ms)
    totalTime = 800;
    % Duration of placeholder/prime displays (ms)
    preTargetTime = 400;
    % Duration of target flash (ms)
    targetTime = 84;
    % Post-target mask
    maskTime = 0;
    % Duration of feedback (seconds)
    feedbackTime = .4;
end

if experiment_ver == 3
    % Total time of stimuli displays (ms)
    totalTime = 800;
    % Duration of placeholder/prime displays (ms)
    preTargetTime = 317;
    % Duration of target flash (ms)
    targetTime = 84;
    % Post-target mask
    maskTime = 301;
    % Duration of feedback (seconds)
    feedbackTime = .4;
end

if debug == 1
    % Total time of stimuli displays (ms)
    totalTime = 0;
    % Duration of placeholder/prime displays (ms)
    preTargetTime = 0;
    % Duration of target flash (ms)
    targetTime = 0;
    % Post-target mask
    maskTime = 0;
    % Duration of feedback (seconds)
    feedbackTime = 0;
end

%%% Misc. variables

% Initialize variable to track all trial numbers
currentTrialTracker = 1;
% Initialize variable to track all blocks
currentBlock = 1;
% Variable to track the type of block, where...
% -2 = Guided practice
% -1 = Exclusion practice (10 correct in a row)
%  0 = Final Training
%  1 = Calibration trials (Staircase method)
%  2 = Main study (Priming trials)
blockTypes = [ -2 -1 0 1 2 ];

%%% Labels for conditions

primeTypeLabels = { 'Neither', 'Foil primed', 'Target primed' };
rotationLabels = { 'Left', 'Right', 'Vertical' };
onscreenAltLabels = { 'Same', 'Different', 'None' };
correctAnswerLabels = keyAssignments;
blockTypeLabels = { 'Guided practice', 'Exclusion practice', ...
                    'Final training', 'Staircase calibration', ...
                    'Main study' };

% Initialize variables for flipping every frame
Background
vbl = Screen('Flip', window);
waitframes = 1;