%--------------------------------------------%
% Extract conditions and run a priming trial %
%--------------------------------------------%

% Variables needed
% currentTrial
% TrialOrder
% currentBlockType
% AllCnd, where...
%   Column  1: Foil contrasts
%   Column  2: Target/foil ratios
%   Column  3: Target contrasts
%   Column  4: Prime durations
%   Column  5: Prime type
%   Column  6: Prime rotation
%   Column  7: Target rotation
%   Column  8: Onscreen alternatives
%   Column  9: Correct answer
%   Column 10: Row index for robot parameters

% Select current trial
sel = TrialOrder( currentTrial );

currentFoil = AllCnd(sel,1);
currentTarget = AllCnd(sel,3);
primeTimeCur = AllCnd(sel,4);
currentType = AllCnd(sel,5);
primeRotation = AllCnd(sel,6);
targetRotation = AllCnd(sel,7);
onscreenChoices = AllCnd(sel,8);
correctAnswer = AllCnd(sel,9);
rowIndex = AllCnd(sel,10);

% Specify stimulus timing

% Duration of fixation
fixTime = totalTime - preTargetTime - targetTime - maskTime;
% Duration of placeholder
placeTime = preTargetTime - primeTimeCur;
% Create vector of durations
FT = [ ...
    fixTime ...
    placeTime ...
    primeTimeCur ...
    targetTime ...
    maskTime ...
    ];

% Create textures for prime/target stimuli
Target_prime_creation

% Run script for generating single trial
Gabor_priming_trial

% Save responses
Data_logger