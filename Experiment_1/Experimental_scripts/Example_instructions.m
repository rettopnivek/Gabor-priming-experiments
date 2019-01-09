%---------------------------------------------%
% Example instructions for debugging purposes %
%---------------------------------------------%

rng shuffle; % Random seed for rng

% Initialize Psychtoolbox
PTB_initial

% Hide the mouse curser
HideCursor();

%%% Initialize variables for stimulus presentation %%%

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

% Set image size for gabor patch\noise
imSize = 100;
% Set contrast for target
cntrstTarget = .4;
% Set number of stripes in gabor patch ( 15 = ~4 stripes, 20 - 25 = 
% 2 stripes
stripes = 15;
% Set gaussian window
gaussWin = 15;
% Set gaussian window for noise
gaussWinNoise = 20;

% Define the stimulus timing
totalTime = 1500;
primeTimes = [ 50 400 ];
targetTime = 100;
postMaskTime = 200;

%%% Initialize variables for responses %%%

% Set keys for responses
keys = 'jk';
keyOptions = getKeyAssignments(keys,1,300);

% Define the number of blocks and trials to run
trialSetPractice = 4;
trialSetAdaptive = 20;
trialSetPsychometric = 4;
stepSetPsychometric = 20;
trialTotalPsychometric = 80;
trialSetPriming = 5;
nBlocksPriming = 4;

%%% Define study version and present instructions %%%

studyType = 1; % Psychometric version
% studyType = 2; % Priming version

% Display the instructions
Task_instructions

% Clear the screen
clear Screen;
% clear all;
sca;