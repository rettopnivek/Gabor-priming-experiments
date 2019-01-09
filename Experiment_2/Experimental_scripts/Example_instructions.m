%---------------------------------------------%
% Example instructions for debugging purposes %
%---------------------------------------------%

% Set debug mode
debug = 0;

rng shuffle; % Random seed for rng

% Initialize Psychtoolbox
PTB_initial

% Hide the mouse curser
HideCursor();

%%% Initialize variables for stimulus presentation %%%

% Set delay for instructions
instructionWaitTime = .2;

% Set the font and text size
Screen('TextFont',window,'Arial Black');
Screen('TextSize',window,20);

% Set the dimensions and color of the fixation dot
dotSizePix = 10;
dotColor = [0 0 0];

% Set the dimensions for the annulus
ovalRadius1 = 35;
ovalSize1 = [ center(1) - ovalRadius1, center(2) - ovalRadius1, ...
    center(1) + ovalRadius1, center(2) + ovalRadius1 ];

ovalRadius2 = ovalRadius1 + 15;
ovalSize2 = [ center(1) - ovalRadius2, center(2) - ovalRadius2, ...
    center(1) + ovalRadius2, center(2) + ovalRadius2 ];

% Set the dimensions for the ovals of the clock prime/mask
RadiusPrp = .6;
nPoints = 30; % Number of ovals to draw for mask

% Set the dimensions for the ovals of the clock prime/mask
RadiusPrp = .6;
nPoints = 15; % Number of ovals to draw for mask

% Set image size for gabor patch\noise
imSize = 100;
% Set contrast for target
cntrstTarget = .4;
% Set number of stripes in gabor patch ( 15 = ~4 stripes, 20 - 25 = 
% 2 stripes
stripes = 15;
% Set size of gaussian window (Larger values lead to less of a gaussian
% mask)
gaussWin = 15;
% Set size of gaussian window for noise
gaussWinNoise = 20;
% Set the criterion for the adaptive trials
crt = .7;
% Set the interval before a time-out response is recorded (in seconds)
timeout = 4.995;
% Indicate if a sinusoidal or step function should be used
stepYes = 1;
% Set the amplitude of the gabor patch
amp = 1;
% Range of rotations
rotRange = [ 35 40 45 50 55 ];

% Define the stimulus timing
totalTime = 1500;
primeTimes = [ 50 100 150 ];
targetTime = 100;
postMaskTime = 100;

%%% Initialize variables for responses %%%

% Set keys for responses
keys = 'jk';
keyOptions = getKeyAssignments(keys,1,300);

% Define the number of blocks and trials to run
nPBtask = 96;
trialSetAdaptive = 36;
nLoops = 3; % Number of times to repeat the adaptive/experiment structure
nA = 1; % Number of times the adaptive design is included
nE = 2; % Number of times the experimental blocks are included
nBlockTrials = 6*3*4;

%%% Define study version and present instructions %%%

% Display the instructions
Task_instructions

% Clear the screen
clear Screen;
% clear all;
sca;