%-----------------%
% Practice trials %
%-----------------%

% Starting options
foilContrasts = foilContrastDefault;
targetRatios = [ 2 1.5 1.25 1.125 ];
primeTimes = 100;
primeTypes = 1; % Neither primed

nTrials = 10;

% Generate conditions
Generate_conditions

% Set simulation parameters for high accuracy so that 
% debugging doesn't truncate early
selVal = strcmp( robotOptions, 'Pick_1' );
AllCnd( AllCnd(:,9) == 1, 10 ) = robotRows( selVal );
selVal = strcmp( robotOptions, 'Pick_0' );
AllCnd( AllCnd(:,9) == 0, 10 ) = robotRows( selVal );

% For each contrast level, randomize the presentation order
TrialOrder = zeros( 1, size( AllCnd, 1 ) );
tmp = 1:size( AllCnd, 1 );
for i = 1:size( targetRatios, 2 )
    selVal = AllCnd(:,2) == targetRatios( i );
    ni = sum( selVal );
    shft = ni * ( i - 1 );
TrialOrder( (1+shft):(ni+shft) ) = ...
    datasample( tmp( selVal ), ni );
end

clear tmp i ni selVal;

%%% Practice trials %%%

% Determine number of trials
totalTrials = size( AllCnd, 1 );

% Initialize matrix to store results
results = zeros(totalTrials,nCol_results);

% Practice block introduction

% Color the screen grey
Background;

instruct = [ 'As additional practice, you will now complete a sequence \n' ...
    'of trials that get progressively harder.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, pakSpace );

% Wait for a keystroke on the keyboard:
Post_instruction

for trl = 1:totalTrials
    
    % Define block type
    currentBlockType = 3;
    
    % Set trial index
    currentTrial = trl;

    % Extract relevant conditions,
    % run priming trial, and
    % save results
    Embedded_trial_in_loop
    
end

% Concatenate current results with previous
allResults = [ allResults; results ];

% Increment block
currentBlock = currentBlock + 1;