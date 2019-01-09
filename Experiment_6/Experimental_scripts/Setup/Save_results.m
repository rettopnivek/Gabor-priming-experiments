%---------------------------------------%
% Save results into .csv and .mat files %
%---------------------------------------%

% Create header for .csv file
csvHeader = [ ...
    ... % Numeric results
    'Subject,' ...                 % Col  1
    'Trial,' ...                   % Col  2
    'Block,' ...                   % Col  3
    'Block_type,' ...              % Col  4
    'Foil_contrast,' ...           % Col  5
    'Target_contrast,' ...         % Col  6
    'Target_to_foil_contrast,' ... % Col  7
    'Max_illuminance,' ...         % Col  8
    'Angle,' ...                   % Col  9
    'Fixation_time,' ...           % Col 10
    'Placeholder_time,' ...        % Col 11
    'Prime_time,' ...              % Col 12
    'Target_time,' ...             % Col 13
    'Mask_time,' ...               % Col 14
    'Feedback_time,' ...           % Col 15
    'Prime_duration,' ...          % Col 16
    'Prime_rotation,' ...          % Col 17
    'Prime_type,' ...              % Col 18
    'Target_rotation,' ...         % Col 19
    'Correct_answer,' ...          % Col 20
    'Choice_rotation,' ...         % Col 21
    'Choice_display,' ...          % Col 22
    'RT,' ...                      % Col 23
    'Choice,' ...                  % Col 24
    'Accuracy,' ...                % Col 25
    ... % Results with interpretable labels
    'Choice_label,' ...            % Col 26
    'Prime_rotation_label,' ...    % Col 27
    'Target_rotation_label,' ...   % Col 28
    'Choice_rotation_label,' ...   % Col 29
    'Prime_type_label,' ...        % Col 30
    'Correct_answer_label,' ...    % Col 31
    'Onscreen_choices_labels,' ... % Col 32
    'Block_type_label,' ...        % Col 33
    ... % Type of experiment
    'Experiment' ...               % Col 34
    '\n' ...
    ];

% Number of columns
nCol = size( allResults, 2 );
nRow = size( allResults, 1 );

% Save a csv file
fid = fopen(csvOutputFile, 'wt'); % Open for writing
% Add column labels
fprintf(fid, csvHeader );
% Loop over each row
for i=1:nRow
    
    % Subject ID
    fprintf( fid, '%d', SubjNum );
    fprintf( fid, ',' ); % Add comma
    
    % Add all numerical results
    fprintf( fid, '%d,', allResults(i,1:nCol) );
    
    % Add character based results
    interpLab = [ ...
        ... % Choice
        keyAssignments{ allResults(i,23) + 1 } ',' ...
        ... % Prime rotation
        rotationLabels{ allResults(i,16) + 1 } ',' ...
        ... % Target rotation
        rotationLabels{ allResults(i,18) + 1 } ',' ...
        ... % Onscreen choice rotation
        rotationLabels{ allResults(i,20) + 1 } ',' ...
        ... % Prime type
        primeTypeLabels{ allResults(i,17) } ',' ...
        ... % Correct answer
        keyAssignments{ allResults(i,19) + 1 } ',' ...
        ... % Onscreen choices
        onscreenAltLabels{ allResults(i,21) } ',' ...
        ... % Block type
        blockTypeLabels{ allResults(i,3) + 3 } ',' ...
        ];
    fprintf( fid, interpLab );
    
    if experiment_ver == 1
            fprintf( fid, 'B' );
    else
            fprintf( fid, 'C' );
    end
    
    % Add a new line
    fprintf(fid, '\n');
end
fclose(fid);

% Save a matlab file
save(matOutputFile,'allResults');
