%-----------------------------------------%
% An example trial for debugging purposes %
%-----------------------------------------%

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
nPoints = 15; % Number of ovals to draw for mask

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
% Set the interval before a time-out response is recorded (in seconds)
timeout = 4.995;

% Define the stimulus timing
totalTime = 1500;
primeTimes = [ 100 400 ];
targetTime = 100;
postMaskTime = 200;

%%% Initialize variables for responses %%%

% Set keys for responses
keys = 'jk';
keyOptions = getKeyAssignments(keys,1,300);

%%% Example trial %%%

% Set contrast difference
cntrstPrp = 1;
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
rot = rand*80;
gabor = Overlaid_Gabors(imSize,stripes,1,cntrst,rot,gaussWin,0,1);
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