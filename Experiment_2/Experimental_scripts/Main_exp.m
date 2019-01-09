%--------------------------------%
% Gabor priming task (version 2) %
% Kevin Potter                   %
% Updated 04/23/2016             %
%--------------------------------%

%{
Purpose:
Matlab and Psychtoolbox code for running a priming task using overlaid
Gabor patches.

Requirements:
Matlab R2013a or Matlab R2015a
Psychtoolbox

Outputs:
A .csv file with the stimuli and response information
A .mat file with the starting and ending time, and the stimuli ordering
info
A text file with demographics information

Notes:
- To prematurely end the experiment, press shift + 2.
- Thanks is extended to Will Hopper and Angela Nelson Lowe for help with
  the code in Matlab
- Run the Test1a.m file first on certain machines to make sure Psychtoolbox
  is working.
- Use the key shortcut Win+P to change from dual to single monitor (Win is
  the key with the Windows logo).

Index:
Lookup - 01:  Pre-experiment setup
Lookup - 02:  Initialize variables for stimulus presentation
Lookup - 03:  Initialize variables for responses
Lookup - 04:  Structure of practice block
Lookup - 05:  Structure of adaptive blocks
Lookup - 06:  Structure of experimental blocks
Lookup - 07:  Run through the practice and experimental blocks
Lookup - 08:  End of experiment
%}

%%% Pre-experiment setup %%%
% Lookup - 01

% Set debug state
debug = 0;
robotPar = [ 2, 3, 1, 1 ];

% Record when the experiment begins
startTime = clock;

% Determine output filenames
if debug == 0
    Subject_ID_input
else
    csvOutputFile{1} = 'Subject_100.csv';
    matOutputFile{1} = 'Subject_100.mat';
end;

% Convert the ID number from a string to a number for later indexing
for i = 9:12
    if ( csvOutputFile{1}(i)=='.' )
        SubjNum = str2num( csvOutputFile{1}(9:(i-1)) );
    end
end;

if debug == 0
    % Determine demographics info based on NIH standards
    Demographics
    % Rename the demographics file
    movefile('Demographics.txt',demographicsFile{1});
    % Move the file to the subjects folder
    movefile(demographicsFile{1},'Subjects');
end;

% Read in the RNG seeds
fileID = fopen('RNG_seeds.txt','r');
RNG_seeds = fscanf(fileID,'%d' );
fclose(fileID);
rng( RNG_seeds(SubjNum) ); % Set seed based on current subject
% rng shuffle; % Random seed for rng

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
% Lookup - 02

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
nPoints = 15; % Number of ovals to draw for mask

% Set image size for gabor patch\noise
imSize = 100;
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
% Lookup - 03

% Set keys for responses
keys = 'jk';
keyOptions = getKeyAssignments(keys,1,300);

% Create empty variable to store all results
allResults = [];

%%%
%%% Structure of practice block %%%
%%%
% Lookup - 04

% Define the number trials and contrast steps for the practice block
trialSetPractice = 4;
stepSetPractice = 6;
nPBtask = trialSetPractice*4*stepSetPractice; % For task instructions

%%% Trials for practice block

% The length of the prime (though it's not shown) for the practice block
practicePrimeTime = 100;
% Define a set of contrast differences to test
cntrstSpace = exp( linspace( log(1), log(.01), stepSetPractice ) );
% Vector of trial numbers
nTrials = ones(1, length(cntrstSpace)*4 )*trialSetPractice;
% Condition variables
cntrstCnd = sort( repmat( cntrstSpace, 1, 4*trialSetPractice ), 'descend');
durCnd = repmat( practicePrimeTime, 1, trialSetPractice*stepSetPractice*4 );
tarCnd = repmat( [ 0, 0, 1, 1 ], 1, trialSetPractice*stepSetPractice );
priCnd = repmat( [ 0, 1, 0, 1 ], 1, trialSetPractice*stepSetPractice );
% Define matrix with variables for conditions
practiceCnd = [ cntrstCnd; durCnd; tarCnd; priCnd ];
practiceCnd = practiceCnd';

%%%
%%% Structure of adaptive blocks %%%
%%%
% Lookup - 05

% Structure
% Prime duration: 50    50    50    50
% Target:         Left  Left  Right Right
% Prime:          Left  Right Left  Right

% Prime duration: 100   100   100   100
% Target:         Left  Left  Right Right
% Prime:          Left  Right Left  Right

% Prime duration: 150   150   150   150
% Target:         Left  Left  Right Right
% Prime:          Left  Right Left  Right

% Set the number of trials for each condition with the adaptive design
trialSetAdaptive = 7;
trialsAdaptiveLoop = 36; % Number of adaptive trials for the subsequent loops

% Condition variables
durCnd = sort( repmat( primeTimes, 1, 4*trialSetAdaptive ) );
tarCnd = repmat( [ 0, 0, 1, 1 ], 1, size( primeTimes, 2 )*trialSetAdaptive );
priCnd = repmat( [ 0, 1, 0, 1 ], 1, size( primeTimes, 2 )*trialSetAdaptive );
% Define matrix with variables for conditions
adaptiveCnd = [ durCnd; tarCnd; priCnd ];
adaptiveCnd = adaptiveCnd';

%%% Initialize variables for measurement model estimation

% Grid precision
alpha_prec = 50;
beta_prec = 50;
% Create a discretized range of sensory levels
x_lev = linspace( log(.005), log(1), 50 );
% Vector of parameter values
boundaries = [ 0, 7, 0, 7 ];
% Parameter grid
alpha_val = linspace( boundaries(1), boundaries(2), alpha_prec );
beta_val = linspace( boundaries(1), boundaries(2), beta_prec );
% Priors
priors = [ 1.5, .5, 2.5, .5 ];
alpha_prior = normpdf( alpha_val, priors(1), priors(2) ) ./ ...
    ( normcdf( 7, priors(1), priors(2) ) - ...
    normcdf( 0, priors(1), priors(2) ) );
beta_prior = normpdf( beta_val, priors(3), priors(4) ) ./ ...
    ( normcdf( 7, priors(3), priors(4) ) - ...
    normcdf( 0, priors(3), priors(4) ) );
% Initialize contrast difference
x = x_lev( round(size(x_lev,2)/2) );

%%%
%%% Structure of experimental blocks %%%
%%%
% Lookup - 06

% Setup for experimental blocks
nLoops = 3; % Number of times to repeat the adaptive/experiment structure
nA = 1; % Number of times the adaptive design is included
nE = 2; % Number of times the experimental blocks are included
nBlocks = nLoops*(nA+nE); % Total number of blocks
yesFeedback = 1; % Feedback will be given

% Structure
% Prime duration: 50    50    50    50
% Target:         Left  Left  Right Right
% Prime:          Left  Right Left  Right

% Prime duration: 100   100   100   100
% Target:         Left  Left  Right Right
% Prime:          Left  Right Left  Right

% Prime duration: 150   150   150   150
% Target:         Left  Left  Right Right
% Prime:          Left  Right Left  Right

% Set the number of trials for each condition for the priming blocks
trialSetPriming = 6;
nBlockTrials = trialSetPriming*size(primeTimes,2)*4;

% Vector of trial numbers
nTrials = ones(1, 4*size(primeTimes,2))*trialSetPriming;
% Condition variables
durCnd = sort( repmat( primeTimes, 1, 4*trialSetPriming ) );
tarCnd = repmat( [ 0, 0, 1, 1 ], 1, size( primeTimes, 2 )*trialSetPriming );
priCnd = repmat( [ 0, 1, 0, 1 ], 1, size( primeTimes, 2 )*trialSetPriming );
% Define matrix with variables for conditions
primeCnd = [ durCnd; tarCnd; priCnd ];
primeCnd = primeCnd';

%%% Run through the practice and experimental blocks %%%
% Lookup - 07

%%% Task instructions and blocks %%%

Task_instructions

% Initialize blcks
blck = 0;

% Initial practice block
PracticeBlock
blck = blck + 1;

%%% Main blocks %%%
% Lookup - 06

for bl = 1:nLoops
    
    % Color the screen grey
    Screen('FillRect', window, [ .5 .5 .5 ]);
    
    % Introduction to loop
    if blck == 1
        string = 'You are starting the experimental blocks. Good luck!';
    elseif blck >= round( (nBlocks - 1)/2 )
        string = 'Feel free to pause for a bit if you are tired.';
    else
        string = ' ';
    end
    
    instruct = num2str( ones(2,1) ); % Create a cell string array
    instruct = cellstr(instruct);
    instruct{1} = [ 'Block ' num2str(blck) ];
    instruct{2} = string;
    instruct{3} = ' ';
    instruct{4} = 'Press any key to begin';
    lenIns = length(instruct);
    
    % Display the instructions
    displayInstruct % Call separate Matlab script
    
    % Loop through adaptive blocks
    for na = 1:nA
        % Use a simple measurement model to determine contrast level
        AdaptivePsychometrics
        blck = blck + 1;
    end
    
    % Loop through experimental blocks
    for ne = 1:nE
        
        if ( blck == round( (nBlocks - 1)/2 ) ) || (  blck == round( 3*(nBlocks - 1)/4 ) )
            string = 'Feel free to pause for a bit if you are tired.';
        else
            string = ' ';
        end
        
        instruct = num2str( ones(2,1) ); % Create a cell string array
        instruct = cellstr(instruct);
        instruct{1} = [ 'Block ' num2str(blck) ];
        instruct{2} = string;
        instruct{3} = ' ';
        instruct{4} = 'Press any key to begin';
        lenIns = length(instruct);
        
        % Display the instructions
        displayInstruct % Call separate Matlab script
        
        % Script for priming blocks
        PrimingBlocks
        blck = blck + 1;
        
    end
    
end

%%% End of the experiment %%%
% Lookup - 08

% Record end of experiment
endTime = clock;

% Save results to file
orig_dir = cd('Subjects');
fid = fopen(csvOutputFile{1}, 'wt'); % Open for writing
fprintf(fid, 'Subject,RT,Choice,Accuracy,Target,Prime,PrimeDuration,Contrast,Block,Angle,FixTime,PrimeTime,TargetTime,MaskTime,BlockType\n');
for i=1:size(allResults,1)
   fprintf(fid, '%d,', SubjNum );
   fprintf(fid, '%d,', allResults(i,1:(size(allResults,2)-1)));
   fprintf(fid, '%d', allResults(i,size(allResults,2)));
   fprintf(fid, '\n');
end
fclose(fid);

% Record Matlab output as well
save(matOutputFile{1},'allResults','startTime','endTime','SubjNum', ...
    'posterior','alpha_prior','beta_prior','alpha_grid','beta_grid');

% Return to the original directory
cd(orig_dir);

% Indicate that the experiment is finished
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
instruct{1} = 'Congratulations, you have finished the study. Please ';
instruct{2} = 'let the experimenter know that you are done and pick up ';
instruct{3} = 'your debriefing form. We appreciate your participation!';
instruct{4} = ' ';
instruct{5} = ' ';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Terminate experiment
KbStrokeWait;

% Clear the screen
clear Screen;
sca;

% Run script to add to experiment log
Experiment_log