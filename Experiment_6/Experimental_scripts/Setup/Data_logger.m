%----------------------------------------%
% Script to save results of single trial %
%----------------------------------------%

% Variables that need to be defined:
% currentBlockType ( 1 - 5 )

% Total number of columns: 23
% Current column
cc = 1;

% Data on overall experiment 
%    1) Trial number
results(trl,cc) = currentTrialTracker;  cc = cc + 1;
currentTrial = currentTrialTracker + 1; % Increment
%    2) Block number
results(trl,cc) = currentBlock;  cc = cc + 1;
%    3) Block type
results(trl,cc) = blockTypes( currentBlockType );  cc = cc + 1;

% Data on stimulus characteristics
%    4) Foil contrast
results(trl,cc) = currentFoil;  cc = cc + 1;
%    5) Target contrast
results(trl,cc) = currentTarget;  cc = cc + 1;
%    6) Target ratio
results(trl,cc) = currentTarget / currentFoil;  cc = cc + 1;
%    7) Max illuminance
results(trl,cc) = PP;  cc = cc + 1;
%    8) Angle for stimuli
results(trl,cc) = rot;  cc = cc + 1;
%    9) Fixation time
results(trl,cc) = time_check(1);  cc = cc + 1;
%   10) Placeholder time
results(trl,cc) = time_check(2) - time_check(1);  cc = cc + 1;
%   11) Prime time
results(trl,cc) = time_check(3) - time_check(2);  cc = cc + 1;
%   12) Target time
results(trl,cc) = time_check(4) - time_check(3);  cc = cc + 1;
%   13) Post-target mask time
results(trl,cc) = time_check(5) - time_check(4);  cc = cc + 1;
%   14) Feedback time
results(trl,14) = feedbackTime;  cc = cc + 1;

% Condition relevant data
%   15) Prime duration
results(trl,cc) = primeTimeCur;  cc = cc + 1;
%   16) Prime rotation
results(trl,cc) = primeRotation;  cc = cc + 1;
%   17) Prime type
results(trl,cc) = currentType;  cc = cc + 1;
%   18) Target rotation
results(trl,cc) = targetRotation;  cc = cc + 1;
%   19) Correct answer
results(trl,cc) = correctAnswer;  cc = cc + 1;
%   20) Onscreen choice rotation
if onscreenChoices == 1
    % Same
    choiceRotation = targetRotation;
elseif onscreenChoices == 2
    % Different
    choiceRotation = 1 - targetRotation;
else
    % None
    choiceRotation = 2;
end
results(trl,cc) = choiceRotation;  cc = cc + 1;
%   21) Onscreen alternative type
results(trl,cc) = onscreenChoices;  cc = cc + 1;

% Response data
%   22) Response time
results(trl,cc) = RT;  cc = cc + 1;
%   23) Choice
results(trl,cc) = resp;  cc = cc + 1;
%   24) Accuracy
results(trl,cc) = Accuracy;  cc = cc + 1;

