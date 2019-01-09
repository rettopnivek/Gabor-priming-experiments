%------------------%
% Staircase trials %
%------------------%

% Define parameters
crit = .8; % Desired level of accuracy
N_width = 10; % Width of interval for moving average
N_int = 8; % Number of intervals to assess
interval = zeros( 1, N_width );
cnt = 1; % Initialize count variable
inc = 1;

% Define window within which no changes should occur
stable_win = binoinv( [.25,.75], N_width, crit )/N_width;

% Size of change to log of target contrast during each interval
x_change = linspace( .5, .1, N_width );

% Starting options
foilContrasts = foilContrastDefault;
targetRatios = 2;
primeTimes = mainPrimeTimes;
primeTypes = 1; % Neither primed

nTrials = 25;

% Generate conditions
Generate_conditions

% Set simulation parameters for high accuracy so that
% debugging doesn't truncate early
selVal = strcmp( robotOptions, 'Pick_1_A' );
AllCnd( AllCnd(:,9) == 1, 10 ) = robotRows( selVal );
selVal = strcmp( robotOptions, 'Pick_0_A' );
AllCnd( AllCnd(:,9) == 0, 10 ) = robotRows( selVal );

% Randomize the presentation order
TrialOrder = randperm( size( AllCnd, 1 ) );

%%% Staircase approach %%%

% Initial starting value for log of target contrast
x_cur = log( targetRatios * foilContrasts );

% Determine number of trials
totalTrials = N_int * N_width;

% Initialize matrix to store results
results = zeros(totalTrials,nCol_results);

% Calibration introduction

% Color the screen grey
Background;

instruct = [ 'Congratulations! You have finished all of the \n' ...
             'training. Now there will be a set of calibration \n' ...
             'trials.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, pakSpace );

% Wait for a keystroke on the keyboard:
Post_instruction

for trl = 1:totalTrials
    
    % Ajdust target contrast
    AllCnd(:,3) = exp( x_cur );
    AllCnd(:,2) = AllCnd(:,3) ./ AllCnd(:,1);
    
    % Adjust parameters
    Wald_race_psychophysics
    
    % Define block type
    currentBlockType = 4;
    
    % Set trial index
    currentTrial = trl;
    
    % Extract relevant conditions,
    % run priming trial, and
    % save results
    Embedded_trial_in_loop
    
    % Apply staircase algorithm
    [ est_ac, x_cur, cnt, interval ] = staircaseAlgorithm( Accuracy, ...
        x_cur, x_change(inc), interval, stable_win, cnt );
    if cnt == 1
        inc = inc + 1;
    end
    
    if inc > 10
        inc = 10;
    end
    
end

% Concatenate current results with previous
allResults = [ allResults; results ];

% Increment block
currentBlock = currentBlock + 1;
