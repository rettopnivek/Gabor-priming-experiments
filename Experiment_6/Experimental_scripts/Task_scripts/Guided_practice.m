%-----------------%
% Practice trials %
%-----------------%

% Starting options
foilContrasts = foilContrastDefault;
targetRatios = 5; % Very easy
primeTimes = 100;
primeTypes = 1; % Neither primed

% Slow down duration of target flash
oldPreTargetTime = preTargetTime;
preTargetTime = preTargetTime + 100;
oldTargetTime = targetTime;
targetTime = targetTime + 200;

% Number of trials
nTrials = 10;

% Generate conditions
Generate_conditions

% Set simulation parameters for high accuracy so that 
% debugging doesn't truncate early
selVal = strcmp( robotOptions, 'Pick_1' );
AllCnd( AllCnd(:,9) == 1, 10 ) = robotRows( selVal );
selVal = strcmp( robotOptions, 'Pick_0' );
AllCnd( AllCnd(:,9) == 0, 10 ) = robotRows( selVal );

% Randomize the presentation order
TrialOrder = randperm( size( AllCnd, 1 ) );

%%% Training set %%%

% Determine number of trials
totalTrials = 5; % Maximum number of possible trials

% Initialize matrix to store results
results = zeros(totalTrials,nCol_results);

% Guided practice introduction

% Color the screen grey
Background

instruct = [ ...
    'You will now complete 5 examples, guided by the experimenter.\n' ...
    'In these examples, the grid will be shown very slowly, to\n' ...
    'to help you understand the task.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, ...
    lnSpace, InstructionTime, pakSpace );

% Wait for a keystroke on the keyboard:
Post_instruction

for trl = 1:totalTrials
    
    % Define block type
    currentBlockType = 1;
    
    % Set trial index
    currentTrial = trl;
    
    % Extract relevant conditions,
    % run priming trial, and
    % save results
    Embedded_trial_in_loop
    
end

% Concatenate current results with previous
allResults = [ allResults; results ];

% Reset stimulus durations
targetTime = oldTargetTime;
preTargetTime = oldPreTargetTime;

% Increment block
currentBlock = currentBlock + 1;