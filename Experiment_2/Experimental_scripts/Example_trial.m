%-----------------------------------------%
% An example trial for debugging purposes %
%-----------------------------------------%

debug = 0;
robotPar = [ 1.5, 2.5, 1, 1 ];

rng shuffle; % Random seed for rng

% Initialize Psychtoolbox
PTB_initial

% Hide the mouse curser
HideCursor();

%%% Initialize variables for stimulus presentation %%%

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

% Empty vector to store results
allResults = [];

%%% Example trial %%%

% Set contrast difference
cntrstPrp = .25;
cntrstLure = .5*cntrstPrp;

% Create noise patch
noiseWin = Noise_function(imSize,gaussWinNoise,0,1);
noiseWinTex = Screen('MakeTexture', window, noiseWin);

% Set up variables for each experimental trial
primeTimeCur = datasample( primeTimes, 1 );
fixTime = totalTime - primeTimeCur - targetTime - ...
    postMaskTime;
FT = [ fixTime, primeTimeCur, targetTime, postMaskTime ];

% Randomly select what the prime and target will be
currentPrime = datasample( [ 0 1 ], 1 );
currentTarget = datasample( [ 0 1 ], 1 );

if ( currentTarget == 1 )
    cntrst = .5 + cntrstLure;
else
    cntrst = .5 - cntrstLure;
end

% Create the gabor patch
rot = datasample(rotRange,1);
gabor = Overlaid_Gabors(imSize,stripes,amp,cntrst,rot,gaussWin,0,1,stepYes);
gaborTex = Screen('MakeTexture', window, gabor);

PrimeYes = 1;
% Run script
Gabor_priming_trial

% Save responses
Choice = resp;
ResponseTime = RT;
Accuracy = currentTarget == resp;

if ( Accuracy == 1 )
    DrawFormattedText(window, 'Correct!', 'center','center', [ 0 0 0 ] );
else
    DrawFormattedText(window, 'Wrong!', 'center','center', [ 0 0 0 ] );
end

% Flip to the screen
Screen('Flip', window);

% Now we have drawn to the screen we wait for a keyboard button press (any
% key) to terminate the demo.
KbStrokeWait;

% Clear the screen
clear Screen;
sca;