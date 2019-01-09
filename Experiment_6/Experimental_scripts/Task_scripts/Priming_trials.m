%----------------%
% Priming trials %
%----------------%

% Set target contrast based on staircase trials
targetRatios = exp( x_cur ) / foilContrastDefault;

% Starting options
foilContrasts = foilContrastDefault;
primeTimes = mainPrimeTimes;
primeTypes = [ 2 3 ]; % Foil/target primed

% Number of trials
if experiment_ver == 2
    nTrials = 45;
    totalTrials = 60;
else
    nTrials = 34;
    totalTrials = 68;
end

% Generate conditions
if nb == 1
    % Generate conditions
    Generate_conditions
    
    % Initialize trial index
    currentTrial = 1;
    
    % Randomize the presentation order
    TrialOrder = randperm( size( AllCnd, 1 ) );
    
else
    % Update target contrast
    AllCnd(:,3) = exp( x_cur );
    AllCnd(:,2) = AllCnd(:,3) ./ AllCnd(:,1);
end

%%% Priming trials %%%

% Initialize matrix to store results
results = zeros(totalTrials,nCol_results);

nb_adjust = totalTrials * ( nb - 1 );
for trl = 1:totalTrials
    
    % Define block type
    currentBlockType = 5;
    
    % Extract relevant conditions,
    % run priming trial, and
    % save results
    Embedded_trial_in_loop
    
    % Increment trial index
    currentTrial = trl + nb_adjust;
    
end

% Apply staircase algorithm over entire set of accuracy values

% Accuracy is final column of results
interval = results( :, nCol_results );
% Shift results over by one
interval(2:totalTrials) = ...
    results( 1:( totalTrials - 1 ), nCol_results );
% Recompute stable window
stable_win = binoinv( [.25,.75], totalTrials, crit )/totalTrials;

% Apply staircase algorithm
cnt = totalTrials;
[ est_ac, x_cur, cnt, interval ] = staircaseAlgorithm( ...
    Accuracy, x_cur, ...
    x_change( size( x_change, 2 ) ), interval, ...
    stable_win, cnt );

% Concatenate current results with previous
allResults = [ allResults; results ];

% Increment block
currentBlock = currentBlock + 1;
