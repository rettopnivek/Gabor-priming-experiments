%--------------------%
% Example of a trial %
% Page 4             %
%--------------------%

%%% Stimulus characteristics %%%

currentFoil = foilContrastDefault;
currentTarget = .125;
targetRotation = datasample( [ 0 1 ], 1 );
currentType = datasample( [ 2 3 ], 1 );
rowIndex = 1;
primeTimeCur = 100;
currentBlockType = 5;

if currentType == 1
    primeRotation = 2;
elseif currentType == 3
    primeRotation = targetRotation;
else
    primeRotation = 1 - targetRotation;
end

correctAnswer = targetRotation;

%%% Fixation %%%

FixationDisplay

% Instructions
string = 'First, you will see a fixation dot:';
[nx,ny,bbox] = DrawFormattedText( window, string, ...
    'center', center(2)-imSize/2-150, ...
    [], [], [], [], lnSpace, [], [] );

% Flip to display
Screen('Flip', window);

% Wait for a keystroke on the keyboard:
Post_instruction

%%% Placeholder %%%

static_display = 1;
PlaceholderDisplay

% Instructions
string = 'Next, you will see a set of horizontal lines:';
[nx,ny,bbox] = DrawFormattedText( window, string, ...
    'center', center(2)-imSize/2-150, ...
    [], [], [], [], lnSpace, [], [] );

% Flip to display
Screen('Flip', window);

% Wait for a keystroke on the keyboard:
Post_instruction

%%% Prime %%%

Target_prime_creation
PrimeDisplay

% Deterimine boundaries of text
initString =  '2) angled in the opposite direction as the darkest stripes in the grid. ';
[dim] = Screen( window, 'TextBounds', initString );

% Instructions
string = [ ...
    'Next you will see a set of stripes that can be \n' ...
    '1) angled in the same direction as the darkest stripes in the grid. \n' ...
    '2) angled in the opposite direction as the darkest stripes in the grid. \n' ...
    '3) angled straight up and down.' ];
[nx,ny,bbox] = DrawFormattedText( window, string, ...
    center(1)-dim(3)/2, center(2)-imSize/2-250, ...
    [], [], [], [], lnSpace, [], [] );

% Flip to display
Screen('Flip', window);

% Wait for a keystroke on the keyboard:
Post_instruction

%%% Target %%%

TargetDisplay

% Instructions
string = [ ...
    'Then, surrounded by a set of dots, you will \n' ...
    'see the grid. It will flash very briefly, so \n' ...
    'it is important to pay attention!' ];
[nx,ny,bbox] = DrawFormattedText( window, string, ...
    'center', center(2)-imSize/2-250, ...
    [], [], [], [], lnSpace, [], [] );

% Flip to display
Screen('Flip', window);

% Wait for a keystroke on the keyboard:
Post_instruction

%%% Choice %%%

if debug == 0
    
    check = 0;
    while check == 0
        
        % Briefly set experiment version to three
        oldExperiment = experiment_ver;
        
        experiment_ver = 3;
        MaskDisplay
        
        % Instructions
        string = [ ...
        'Finally, you will see shifting static. This is when \n' ...
        'you should make your response using the "j" and \n' ...
        '"k" keys. You will then be told if you were correct.' ];
        [nx,ny,bbox] = DrawFormattedText( window, string, ...
            'center', center(2)-imSize/2-250, ...
            [], [], [], [], lnSpace, [], [] );
        
        % Reset experiment version
        experiment_ver = oldExperiment;
        
        % Flip to the screen
        vbl = Screen('Flip', window, vbl + (waitframes - 0.5) * ifi);
        
        % Check for input
        [ press, secs, keynum ] = KbCheck;
        
        if press
            check = 1;
        end
        
        % Delay measurment for .5 ms
        WaitSecs(0.0005);
    end
    
else
    
    % Briefly set experiment version to three
    oldExperiment = experiment_ver;
    
    experiment_ver = 3;
    MaskDisplay
    
    % Instructions
    string = [ ...
        'Finally, you will see shifting static. This is when \n' ...
        'you should make your response using the "j" and \n' ...
        '"k" keys. You will then be told if you were correct.' ];
    [nx,ny,bbox] = DrawFormattedText( window, string, ...
        'center', center(2)-imSize/2-250, ...
        [], [], [], [], lnSpace, [], [] );
    
    % Reset experiment version
    experiment_ver = oldExperiment;
    
    % Flip to display
    Screen('Flip', window);
    
    WaitSecs( InstructionTime );
end

