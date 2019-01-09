%-----------------%
% Practice trials %
%-----------------%

% Starting options
foilContrasts = foilContrastDefault;
targetRatios = 5; % Very easy
primeTimes = 100;
primeTypes = 1; % Neither primed

% Number of trials
nTrials = 25;

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
totalTrials = 81; % Maximum number of possible trials

% Initialize matrix to store results
results = zeros(totalTrials,nCol_results);

% Practice block introduction

% Color the screen grey
Background;

instruct = [ ...
    'You will now train on a set of very easy trials.\n' ...
    'You will need to get 10 trials correct in a row.\n' ...
    'The grid will appear very quickly now.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, ...
    lnSpace, InstructionTime, pakSpace );

% Wait for a keystroke on the keyboard:
Post_instruction

% Track accuracy
nCorrect = 0;

trl = 1;
while nCorrect < 10 && trl <= 80
    
    % Define block type
    currentBlockType = 2;
    
    % Set trial index
    currentTrial = trl;
    
    % Extract relevant conditions,
    % run priming trial, and
    % save results
    Embedded_trial_in_loop

    % Reset accuracy counter every time a mistake 
    % was made
    if Accuracy == 1
        nCorrect = nCorrect + 1;
    else
        nCorrect = 0;
    end
    
    trl = trl + 1;
end

% Trim results to only the completed trials
results = results(1:(trl-1),:);

% Concatenate current results with previous
allResults = [ allResults; results ];

% Increment block
currentBlock = currentBlock + 1;

% If subjects failed to meet accuracy cut-off
if trl >= 80
    
    % Clear the screen
    clear Screen;
    sca;
    
    
    % Indicate that a different ID number must be used
    string = sprintf( [ ...
        'The experiment has ended early. You will still earn\n' ...
        'full credit. Thank you for your participation!' ] );
            waitfor( msgbox(string,'Experiment finished') ); % Must close message to continue
    
    % Quit Matlab
    quit force;
end