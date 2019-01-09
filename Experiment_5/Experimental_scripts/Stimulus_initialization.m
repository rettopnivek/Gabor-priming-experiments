%--------------------------------------%
% Stimulus and response initialization %
%--------------------------------------%

rng shuffle; % Random seed for rng

% Initialize Psychtoolbox
PTB_initial

% Check if framerate is 60 Hz
if (ifi > .017)
    % Indicate that the monitor is set to the wrong refresh rate
    warning = 'Please press shift+2 and set monitor frame rate to at least 60 Hz';
    DrawFormattedText(window, warning,'center','center',[ 0 0 0 ] );
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    return
end

% Hide the mouse curser
HideCursor();

%%% Initialize variables for stimulus presentation %%%

%%% General settings

% Set the font and text size
Screen('TextFont',window,'Monospace');
TxtSz = 20;
Screen('TextSize',window,TxtSz);

% Set the dimensions for the annulus
ovalRadius1 = 35;
ovalSize1 = [ center(1) - ovalRadius1, center(2) - ovalRadius1, ...
    center(1) + ovalRadius1, center(2) + ovalRadius1 ];
ovalRadius2 = ovalRadius1 + 15;
ovalSize2 = [ center(1) - ovalRadius2, center(2) - ovalRadius2, ...
    center(1) + ovalRadius2, center(2) + ovalRadius2 ];

% Set distance from center for the 'Press any key' phrase
pakSpace = 250;
% Set line spacing for instructions
lnSpace = 1.5;

%%% Fixation

% Set the dimensions and color of the fixation dot
dotSizePix = 10;
dotColor = [0 0 0];

%%% Prime

% Set the dimensions for the ovals of the clock prime/mask
RadiusPrp = .6;
nPoints = 20; % Number of ovals to draw for mask

% Set the type of prime mask to use
maskString = '#';
primeMask = 2;

%%% Target

% Set image size for gabor patch\noise
imSize = 100;
% Set number of stripes in gabor patch ( 5 = ~3 visible stripes )
stripes = 5;
% Set radius for circle containing gabor patches
stimWin = .35;
% Set angle of gratings
theta = [ 35 40 45 50 55 ];
% Set illuminance for maximum peak of the 4 intensities in the patch
PP = 76;
% Set value for foil contrast
foilContrastDefault = .05;

%%% Choice

% Set width of line for choice comparison
choiceWdth = [.2,.125];
% Set radius of circle containing choice line
choiceR = [.5,.35];

% Set the interval before a time-out response is recorded (in seconds)
timeout = 4.995;

% Set keys for responses
keys = 'jk';
keyOptions = getKeyAssignments(keys,1,300);
optionalString = ' ';

%%% Stimulus timing

% Define the stimulus timing
totalTime = 1500; % In milliseconds
placeholderTime = 600;
targetTime = 84;
feedbackTime = .5; % In seconds
InstructionTime = 2;

%%% Variables for adaptive trials

% Grid precision
alpha_prec = 50;
beta_prec = 50;

% Create a discretized range of sensory levels
x_lev = linspace( log( foilContrastDefault ), log(1), 50 );

% Vector of parameter values
boundaries = [ 0, 15, 0, 10 ];

% Starting priors for adaptive trials

% Priors
% alpha - N(5.59,1.08)
% beta - N(2.99,.054)
origPriors = [ 5.59 1.08 2.99 .5 ];