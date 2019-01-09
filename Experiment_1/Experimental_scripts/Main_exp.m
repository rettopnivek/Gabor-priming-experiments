%--------------------%
% Gabor priming task %
% Kevin Potter       %
% Updated 03/15/2016 %
%--------------------%

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
Lookup - 04:  Set up trials and blocks
Lookup - 05:  Create stimulus variables for trials and blocks
Lookup - 06:  Main blocks
Lookup - 07:  End of experiment
%}

%%% Pre-experiment setup %%%
% Lookup - 01

% Record when the experiment begins
startTime = clock;

% Define the study type
% studyType = 1; % Psychometric version
% studyType = 2; % Priming version no feedback
% studyType = 3; % Priming version with feedback
% studyType = 4; % Priming version with null/both prime
Select_version

% Determine output filenames
Subject_ID_input

% Convert the ID number from a string to a number for later indexing
SubjNum = [];
for i = 9:11
    if ( isempty(str2num(csvOutputFile{1}(i)))==0 )
        if ( real( str2num(csvOutputFile{1}(i) ) ) > 0 )
            SubjNum = [ SubjNum csvOutputFile{1}(i) ];
        end;
    end;
end;
SubjNum = str2num( SubjNum );

% Read in the RNG seeds
fileID = fopen('RNG_seeds.txt','r');
RNG_seeds = fscanf(fileID,'%d' );
fclose(fileID);
rng( RNG_seeds(SubjNum) ); % Set seed based on current subject
% rng shuffle; % Random seed for rng

% Determine demographics info based on NIH standards
Demographics
% Rename the demographics file
movefile('Demographics.txt',demographicsFile{1});
% Move the file to the subjects folder
movefile(demographicsFile{1},'Subjects');

% Initialize Psychtoolbox
PTB_initial

% Check if framerate is 144 Hz
if (ifi < .006) || (ifi > .008)
    % Indicate that the monitor is set to the wrong refresh
    % rate
    warning = 'Please press shift+2 and set monitor frame rate to 144 Hz';
    DrawFormattedText(window, warning,'center','center',[ 0 0 0 ] );
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    return
end

% Hide the mouse curser
HideCursor();

%%% Initialize variables for stimulus presentation %%%
% Lookup - 02

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
% Set the criterion for the adaptive trials
crt = .67;
% Set the interval before a time-out response is recorded (in seconds)
timeout = 4.995;
% Range of rotations
rotRange = [ 35 40 45 50 55 ];

% Define the stimulus timing
totalTime = 1500;
% If the framerate is 1 frame every 7 ms (i.e. 144 Hz) we want 
% [ 1 3 9 27 81 ] frames
primeTimes = [ 7 21 63 188 563 ];
targetTime = 100;
postMaskTime = 100;

%%% Initialize variables for responses %%%
% Lookup - 03

% Set keys for responses
keys = 'jk';
keyOptions = getKeyAssignments(keys,1,300);

% Create empty variable to store all results
allResults = [];

%%% Set up trials and blocks %%%
% Lookup - 04

% Define the number trials and contrast steps for the practice block
trialSetPractice = 4;
stepSetPractice = 6;
nPBtask = trialSetPractice*4*stepSetPractice; % For task instructions

% Setup for experimental blocks
nLoops = 3; % Number of times to repeat the adaptive/experiment structure
nA = 1; % Number of times the adaptive design is included
nE = 2; % Number of times the experimental blocks are included
nBlocks = nLoops*(nA+nE); % Total number of blocks
yesFeedback = 0; % No feedback will be given
if studyType == 3
    yesFeedback = 1; % Unless it is the third study version
    studyType = 2;
end

% Set the number of trials for each condition with the adaptive design
trialSetAdaptive = 16;
if studyType == 2
    trialSetAdaptive = 20;
end
trialsAdaptiveLoop = 32; % Number of adaptive trials for the subsequent loops
if studyType == 2
    trialsAdaptiveLoop = 32;
end
% adaptivePrimeTime = 150;

% Set the number of trials for each condition for the psychometric blocks
if studyType == 1
    % psychoPrimeTime = 150; % Duration of non-existent prime
    nBlockTrials = 64; % Number of trials for a block
    nTotalTrials = nE*nLoops*nBlockTrials;
    stepSetPsychometric = 18; % Number of contrast steps
    trialsPerStep = nTotalTrials/stepSetPsychometric; % Number of trials for a step
    trialSetPsycho = trialsPerStep/4; % Number of trials for each condition    
end

% Set the number of trials for each condition for the priming blocks
if studyType == 2
    trialSetPriming = 4;
    nBlockTrials = trialSetPriming*size(primeTimes,2)*4;
end

%%% Create stimulus variables for trials and blocks %%%
% Lookup - 05

%%% Trials for practice block

% Number of trials per condition
trialSet = trialSetPractice;
% The length of the prime (though it's not shown) for the practice block
practicePrimeTime = 150;
% Define a set of contrast differences to test
cntrstSpace = exp( linspace( log(1), log(.01), stepSetPractice ) );
% Vector of trial numbers
nTrials = ones(1, length(cntrstSpace)*4 )*trialSet;
% The duration for the prime (becomes additional time added to the
% interstimulus gap
durCnd = ones(1,sum(nTrials))*practicePrimeTime;
% The orientation of the target strips
tarSpace = [ zeros(1,trialSet*2) ones(1,trialSet*2) ];
% The prime orientations
priSpace = [ zeros(1,trialSet) ones(1,trialSet) ];
priSpace = [ priSpace priSpace ];
% Condition variables
cntrstCnd = [];
tarCnd = [];
priCnd = [];
for i = 1:length(cntrstSpace)
    cntrstCnd = [ cntrstCnd ones(1,trialSet*4)*cntrstSpace(i) ];
    tarCnd = [ tarCnd tarSpace ];
    priCnd = [ priCnd priSpace ];
end
% Define matrix with variables for conditions
practiceCnd = [ cntrstCnd; durCnd; tarCnd; priCnd ];
practiceCnd = practiceCnd';
% Randomly shuffle order
practiceOrd = randperm( size( practiceCnd, 1 ) );

%%% Trials for adaptive blocks %%%

% Prime duration: Fixed Fixed Fixed Fixed
% Target:         Left  Left  Right Right
% Prime:          Left  Right Left  Right

% Number of trials in a condition
trialSet = trialSetAdaptive;
% Condition variables
% durCnd = ones(1,trialSet*4)*adaptivePrimeTime;
durCnd = datasample( primeTimes, trialSetAdaptive*4 );
tarCnd = [ zeros(1,trialSet*2) ones(1,trialSet*2) ];
priCnd = [ zeros(1,trialSet) ones(1,trialSet) ];
priCnd = [ priCnd priCnd ];
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

%%% Psychometric blocks
if studyType == 1
    % Number of trials in a condition
    trialSet = trialSetPsycho;
    % Define a set of contrast differences to test
    cntrstSpace = exp( linspace( log(.005), log(1), stepSetPsychometric ) );
    % Vector of trial numbers
    nTrials = ones(1, length(cntrstSpace)*4 )*trialSet;
    % The duration for the prime (becomes additional time added to the
    % interstimulus gap
    % durCnd = ones(1,sum(nTrials))*psychoPrimeTime;
    durCnd = datasample( primeTimes, sum(nTrials) );
    % The orientation of the target strips
    tarSpace = [ zeros(1,trialSet*2) ones(1,trialSet*2) ];
    % The prime orientations
    priSpace = [ zeros(1,trialSet) ones(1,trialSet) ];
    priSpace = [ priSpace priSpace ];
    
    cntrstCnd = [];
    tarCnd = [];
    priCnd = [];
    for i = 1:length(cntrstSpace)
        cntrstCnd = [ cntrstCnd ones(1,trialSet*4)*cntrstSpace(i) ];
        tarCnd = [ tarCnd tarSpace ];
        priCnd = [ priCnd priSpace ];
    end
    % Create matrix for conditions
    psychoCnd = [ cntrstCnd; durCnd; tarCnd; priCnd ];
    psychoCnd = psychoCnd';    
    % Randomly shuffle order
    ordPsycho = randperm( size( psychoCnd, 1 ) );
    % Initialize starting trial
    trl_pc = 0;
end

% Priming blocks

if studyType == 2
    
    % Prime duration: 7    7     7     7     21   21    21    21   
    % Target:         Left Left  Right Right Left Left  Right Right
    % Prime:          Left Right Left  Right Left Right Left  Right
    
    % Prime duration: 63   63    63    63    189  189   189   189  
    % Target:         Left Left  Right Right Left Left  Right Right
    % Prime:          Left Right Left  Right Left Right Left  Right
    
    % Prime duration: 567  567   567   567  
    % Target:         Left Left  Right Right
    % Prime:          Left Right Left  Right
    
    % Vector of trial numbers
    trialSet = trialSetPriming;
    nTrials = ones(1, 4*size(primeTimes,2))*trialSet;
    % The duration for the prime
    durCnd = [ ones(1,trialSet*4)*primeTimes(1) ...
        ones(1,trialSet*4)*primeTimes(2) ...
        ones(1,trialSet*4)*primeTimes(3) ...
        ones(1,trialSet*4)*primeTimes(4) ...
        ones(1,trialSet*4)*primeTimes(5) ];
    % The orientation of the target strips
    tarCnd = [ zeros(1,trialSet*2) ones(1,trialSet*2) ];
    tarCnd = [ tarCnd tarCnd tarCnd tarCnd tarCnd ];
    % The prime orientations
    priCnd = [ zeros(1,trialSet) ones(1,trialSet) ];
    priCnd = [ priCnd priCnd ];
    priCnd = [ priCnd priCnd priCnd priCnd priCnd ];
    
    % Create matrix for conditions
    primeCnd = [ durCnd; tarCnd; priCnd ];
    primeCnd = primeCnd';
    
end

%%% Task instructions and blocks %%%

Task_instructions

% Initialize blcks
blck = 0;

%%% Run through the practice and experimental blocks %%%
% Lookup - 04

% Initial practice block
%PracticeBlock
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
        %AdaptivePsychometrics
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
        
        if studyType == 1
            PsychometricBlocks
            blck = blck + 1;
        end
        
        if studyType == 2
            PrimingBlocks
            blck = blck + 1;
        end
        
    end
    
end

%%% End of the experiment %%%
% Lookup - 07

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

% Now we have drawn to the screen we wait for a keyboard button press (any
% key) to terminate the demo.
KbStrokeWait;

% Clear the screen
clear Screen;
sca;

% Run script to add to experiment log
Experiment_log