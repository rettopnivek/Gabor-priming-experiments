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

correctAnswer = datasample( [ 1 2 ], 1 );

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
        
        MaskDisplay
        
        % Instructions
        string = [ ...
        'Next, you will see shifting static for a set \n' ...
        'interval of time.' ];
        [nx,ny,bbox] = DrawFormattedText( window, string, ...
            'center', center(2)-imSize/2-250, ...
            [], [], [], [], lnSpace, [], [] );
        
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
    
    MaskDisplay
    
    % Instructions
        string = [ ...
        'Next, you will see shifting static for a set \n' ...
        'interval of time.' ];
    [nx,ny,bbox] = DrawFormattedText( window, string, ...
        'center', center(2)-imSize/2-250, ...
        [], [], [], [], lnSpace, [], [] );
    
    % Flip to display
    Screen('Flip', window);
    
    WaitSecs( InstructionTime );
end


%%% Draw stripes that are differ from the onscreen choices %%%

% Color the screen grey
Background

% Draw the onscreen choices
SameDifferentChoices

% Choice options
string = [ '     Same?', '          ', 'Different?' ];
DrawFormattedText(window, string, ...
    'center',vChoicePos, color_black );

string = [ ...
    'Finally, you will see the test set of stripes. If you \n' ...
    'think they match the darker stripes from the previously seen \n' ...
    'grid, press "j". If you think they are different, press "k". \n' ...
    'You will then receive feedback on your choice.' ];
[nx,ny,bbox] = DrawFormattedText( window, string, ...
    'center',center(2)-imSize/2-250, [], [], [], [], lnSpace, [], [] );

% Flip to display
Screen('Flip', window);

% Wait for a keystroke on the keyboard:
Post_instruction

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

