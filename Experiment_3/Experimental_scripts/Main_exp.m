%------------------------%
% Main experiment script %
%------------------------%

%{
Purpose:
Matlab and Psychtoolbox code for running variants of a priming task using
overlaid Gabor patches.

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
- Use the key shortcut Win+P to change from dual to single monitor (Win is
  the key with the Windows logo).
%}

% Add path for additional functions
orig_dir = cd('Internal_functions');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

% Add path for trial scripts
orig_dir = cd('Stimuli');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

% Add path for psychophysics functions
orig_dir = cd('Psychophysics');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

% Add path for task scripts
orig_dir = cd('Task_scripts');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

%%% Experiment set up %%%

% Record when the experiment begins
startTime = clock;

%%% Specify subject ID %%%
Subject_ID_input

% Convert the ID number from a string to a number for later indexing
for i = 9:12
    if ( csvOutputFile(i)=='.' )
        SubjNum = str2num( csvOutputFile(9:(i-1)) );
    end
end;

%%% Demographics %%%
Demographics

% Set debug options
if debug == 1
    % Skip Psychtoolbox synch tests
    Screen('Preference', 'SkipSyncTests', 1);
    robotPar = [ 5.5 2.97 .6 1.44 0 ];
end

% Run script to initialize stimuli/responses
Stimulus_initialization

% Initialize empty matrix to store all results
allResults = [];

%%% Instructions %%%
Task_instructions

%%% Experimental trials %%%

% Run guided practice and training trials
blckType = -1;
Guided_practice
Training_trials

% Run practice trials
blckType = 0;
Practice_trials

% If calibration trials are based on the adaptive method
if CalType == 1
    
    % Define sequence of adaptive/main study trials
    blockType = [ 1 2 2 2 1 2 2 2 ];
    nBlocksPriming = sum( blockType == 2 );
    nBlocksAdaptive = sum( blockType == 1 );
    nBlocks = size( blockType, 2 );
    nTrialsAdaptive = [ 80 ones( 1, nBlocksAdaptive - 1 )*32 ];
    adaptiveInc = 1;
    
    % Loop over blocks
    for blck = 1:size( blockType, 2 )
        
        blckType = blockType(blck);
        
        % Color the screen grey
        Screen('FillRect', window, [ .5 .5 .5 ]);
        
        if blckType == 1
            string = '\nCalibration';
        else
            string = '\nMain study';
        end
        
        % Block progress
        blockProgress( window, center, blck, nBlocks, string, [ 150 .5 pakSpace  lnSpace 300 ] );
        
        if blckType == 1
            Adaptive_trials
            adaptiveInc = adaptiveInc + 1;
        else
            if StudyType == 1
                Priming_trials
            else
                Psychophysics_trials
            end
        end
       
    end
    
else
    
    % Define sequence of adaptive/main study trials
    blockType = [ 1 2 2 2 2 2 2 ];
    nBlocksPriming = sum( blockType == 2 );
    nBlocks = size( blockType, 2 );
    
    % Loop over blocks
    for blck = 1:size( blockType, 2 )
        
        blckType = blockType(blck);
        
        % Color the screen grey
        Screen('FillRect', window, [ .5 .5 .5 ]);
        
        if blckType == 1
            string = '\nCalibration';
        else
            string = '\nMain study';
        end
        
        % Block progress
        blockProgress( window, center, blck, nBlocks, string, [ 150 .5 pakSpace  lnSpace 300 ] );
        
        if blckType == 1
            Staircase_trials
        else
            
            targetThreshold = exp( x_cur )/foilContrastDefault;
            
            if StudyType == 1
                Priming_trials
            else
                Psychophysics_trials
            end
            
            % Carry out calibration using current block of results
            sel = results(:,5) == 0;
            N_width = sum( sel );
            if N_width > 1
                interval = zeros( 1, N_width );
                interval(1:(N_width-1)) = results(2:N_width,3);
                stable_win = binoinv( [.25,.75], N_width, crit )/N_width;
                
                % Apply staircase algorithm
                cnt = N_width;
                [ est_ac, x_cur, cnt, interval ] = staircaseAlgorithm( ...
                    results(N_width,3), x_cur, ...
                    x_change( size( x_change, 2 ) ), interval, ...
                    stable_win, cnt );                
            end
            
        end
        
    end
    
    
end

%%% End of the experiment %%%

% Record end of experiment
endTime = clock;

cd('Subjects');
% Save a csv file
fid = fopen(csvOutputFile, 'wt'); % Open for writing
fprintf(fid, 'Subject,RT,Choice,Accuracy,Target,PrimeType,PrimeDuration,FoilContrast,TargetContrast,PP,Angle,BlockType\n');
for i=1:size(allResults,1)
    fprintf(fid, '%d,', SubjNum );
    fprintf(fid, '%d,', allResults(i,1:(size(allResults,2)-1)));
    fprintf(fid, '%d', allResults(i,size(allResults,2)));
    fprintf(fid, '\n');
end
fclose(fid);

% Record Matlab output as well
if CalType == 1
    save(matOutputFile,'allResults','startTime','endTime','SubjNum', ...
        'posterior','alpha_prior','beta_prior','alpha_grid','beta_grid');
else
    save(matOutputFile,'allResults','startTime','endTime','SubjNum' );
end
% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

instruct = [ 'Congratulations, you have finished the study! \n' ...
    'Please let the experimenter know that you are done \n' ...
    'so that you can pick up your debriefing form. Thanks \n' ...
    'again for your participation! \n' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, pakSpace );

% Clear the screen
clear Screen;
sca;

% Return to original directory
cd('..');

% Run script to add to experiment log
Experiment_log
