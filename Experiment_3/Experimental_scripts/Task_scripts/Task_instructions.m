%-------------------%
% Task instructions %
%-------------------%

% Index
% Lookup - 01:  Initial description of stimuli
% Lookup - 02:  Key assignment
% Lookup - 03:  Description of task goal
% Lookup - 04:  Introduce an example of a trial
% Lookup - 05:  Placeholder stimulus
% Lookup - 06:  Prime
% Lookup - 07:  Target
% Lookup - 08:  Mask
% Lookup - 09:  Choice
% Lookup - 10:  Feedback
% Lookup - 11:  Block structure
% Lookup - 12:  Final notes

%%%
%%% Define variables for stimuli examples
%%%

% Create gabor patches oriented to the left
gabor = Overlaid_Gabors(imSize,PP,.2,0,-45,stripes,stimWin,0);
gaborTexLeft1 = Screen('MakeTexture', window, gabor);
gabor = Overlaid_Gabors(imSize,PP,.15,0,-45,stripes,stimWin,0);
gaborTexLeft2 = Screen('MakeTexture', window, gabor);

% Create right gabor patch
gabor = Overlaid_Gabors(imSize,PP,.2,0,45,stripes,stimWin,0);
gaborTexRight1 = Screen('MakeTexture', window, gabor);
gabor = Overlaid_Gabors(imSize,PP,.05,0,45,stripes,stimWin,0);
gaborTexRight2 = Screen('MakeTexture', window, gabor);

% Create gabor grids
gabor = Overlaid_Gabors(imSize,PP,.2,.2,-45,stripes,stimWin,0);
gaborTexGuess = Screen('MakeTexture', window, gabor);
gabor = Overlaid_Gabors(imSize,PP,.125,.05,-45,stripes,stimWin,0);
gaborTexEasy = Screen('MakeTexture', window, gabor);

% Set up options for example trial
currentFoil = foilContrastDefault;
currentTarget = .125;
primeTimeCur = 50;
correctAnswer = 1;
if StudyType == 1
    currentType = 2;
else
    currentType = 0;
end

% Based on target value, set rotation angle
rot = 45;
thetaT = rot;
if correctAnswer == 0
    thetaT = thetaT - 90;
end

% Create the gabor patch
gabor = Overlaid_Gabors(imSize,PP,currentTarget,currentFoil,thetaT,stripes,stimWin,0);
% Create possible prime
if currentType == 0
    gaborPrime = Overlaid_Gabors(imSize,PP,1,0,90,stripes,choiceR,0);
elseif currentType == 1
    gaborPrime = Overlaid_Gabors(imSize,PP,1,0,thetaT,stripes,choiceR,0);
elseif currentType == 2
    if correctAnswer == 1
        gaborPrime = Overlaid_Gabors(imSize,PP,1,0,thetaT-90,stripes,choiceR,0);
    else
        gaborPrime = Overlaid_Gabors(imSize,PP,1,0,rot,stripes,choiceR,0);
    end
end
static_display = 1;

%%%
%%% Initial description of stimuli
%%%
% Lookup - 01

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

%%% Draw the stimuli %%%

% Draw the gabor patches
hShift = 90;
Screen('DrawTextures', window, gaborTexLeft1, [],...
    [center(1)-imSize-hShift,center(2)-imSize/2,center(1)-hShift,center(2)+imSize/2], ...
    [], [], [], []);
Screen('DrawTextures', window, gaborTexRight1, [],...
    [center(1)+hShift,center(2)-imSize/2,center(1)+imSize+hShift,center(2)+imSize/2], ...
    [], [], [], []);
Screen('DrawTextures', window, gaborTexGuess, [],...
    [center(1)-imSize/2,center(2)-imSize/2,center(1)+imSize/2,center(2)+imSize/2], ...
    [], [], [], []);
    
string = 'Mixed';
[dim] = Screen( window, 'TextBounds', string ); % Dimensions for 1 line of text
% Bounding box for text
txtBox = [ center(1) - 1.2*dim(3)/2, center(2) + imSize/2 + dim(4) - 10, ...
    center(1) + 1.2*dim(3)/2,  center(2) + imSize/2 - 10 ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], [], [], txtBox );

string = '>>>';
[dim] = Screen( window, 'TextBounds', string ); % Dimensions for 1 line of text
% Bounding box for text
txtBox = [ center(1) - hShift + 1.2*dim(3)/2, center(2) + dim(4)/2, ...
    center(1) - imSize/2 - 1.2*dim(3)/2,  center(2) - dim(4) ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], [], [], txtBox );

string = '<<<';
[dim] = Screen( window, 'TextBounds', string ); % Dimensions for 1 line of text
% Bounding box for text
txtBox = [ center(1) + imSize/2 + 1.2*dim(3)/2, center(2) + dim(4)/2, ...
    center(1) + hShift - 1.2*dim(3)/2,  center(2) - dim(4) ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], [], [], txtBox );

% Present instructions
string = [ ...
    'In this task, you will see a grid made by \n' ...
    'mixing 2 sets of stripes. Each set of stripes \n' ...
    'has a matching key on the keyboard.' ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
WaitSecs( InstructionTime );
KbStrokeWait;

%%%
%%% Key assignment
%%%
% Lookup - 02

%%% Stripes rotated to the left %%%

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

%%% Draw the stimuli %%%

% Draw a gabor patch
hShift = 90;
Screen('DrawTextures', window, gaborTexLeft1, [],...
    [center(1)-imSize/2,center(2)-imSize/2,center(1)+imSize/2,center(2)+imSize/2], ...
    [], [], [], []);

string = [ ...
    'As practice, press the matching key to the set \n' ...
    'of stripes currently shown.' ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

check = 0;
inc = 0;
while check ~= keyOptions(1)
    
    [ ~, tmp ] = KbPressWait;
    tmp2 = 1:256;
    check = tmp2( tmp == 1 );
    
    inc = inc + 1;
    if inc > 0
        % Color the screen grey
        Screen('FillRect', window, [ .5 .5 .5 ]);
        
        %%% Draw the stimuli %%%
        
        % Draw a gabor patch
        hShift = 90;
        Screen('DrawTextures', window, gaborTexLeft1, [],...
            [center(1)-imSize/2,center(2)-imSize/2,center(1)+imSize/2,center(2)+imSize/2], ...
            [], [], [], []);
        
        string = [ ...
            'Whoops! Try again.' ];
        [nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );
        
        % Flip to the screen (i.e. display stimuli)
        Screen('Flip', window);
        
    end
end

%%% Stripes rotated to the right %%%

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

%%% Draw the stimuli %%%

% Draw a gabor patch
hShift = 90;
Screen('DrawTextures', window, gaborTexRight1, [],...
    [center(1)-imSize/2,center(2)-imSize/2,center(1)+imSize/2,center(2)+imSize/2], ...
    [], [], [], []);

string = [ ...
    'Now press the matching key to the new \n' ...
    'set of stripes shown.' ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

check = 0;
inc = 0;
while check ~= keyOptions(2)
    
    [ ~, tmp ] = KbPressWait;
    tmp2 = 1:256;
    check = tmp2( tmp == 1 );
    
    inc = inc + 1;
    if inc > 0
        % Color the screen grey
        Screen('FillRect', window, [ .5 .5 .5 ]);
        
        %%% Draw the stimuli %%%
        
        % Draw a gabor patch
        hShift = 90;
        Screen('DrawTextures', window, gaborTexRight1, [],...
            [center(1)-imSize/2,center(2)-imSize/2,center(1)+imSize/2,center(2)+imSize/2], ...
            [], [], [], []);
        
        string = 'Whoops! Try again.';
        [nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );
        
        % Flip to the screen (i.e. display stimuli)
        Screen('Flip', window);
        
    end
end

%%%
%%% Description of task goal
%%%
% Lookup - 03

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

%%% Draw the stimuli %%%

% Draw the gabor patches
hShift = 90;
Screen('DrawTextures', window, gaborTexLeft2, [],...
    [center(1)-imSize-hShift,center(2)-imSize/2,center(1)-hShift,center(2)+imSize/2], ...
    [], [], [], []);
Screen('DrawTextures', window, gaborTexRight2, [],...
    [center(1)+hShift,center(2)-imSize/2,center(1)+imSize+hShift,center(2)+imSize/2], ...
    [], [], [], []);
Screen('DrawTextures', window, gaborTexEasy, [],...
    [center(1)-imSize/2,center(2)-imSize/2,center(1)+imSize/2,center(2)+imSize/2], ...
    [], [], [], []);

% Include labels
string = '75%';
[dim] = Screen( window, 'TextBounds', string ); % Dimensions for 1 line of text
% Bounding box for text
txtBox = [ center(1) - imSize - hShift - 1.2*(dim(3)-imSize)/2, center(2) - imSize/2 - 10, ...
    center(1) - hShift + 1.2*(dim(3)-imSize)/2, center(2) - imSize/2 - 10 - dim(4) ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], [], [], txtBox );

string = '25%';
[dim] = Screen( window, 'TextBounds', string ); % Dimensions for 1 line of text
% Bounding box for text
txtBox = [ center(1) + hShift - 1.2*(dim(3)-imSize)/2, center(2) - imSize/2 - 10, ...
    center(1) + imSize + hShift + 1.2*(dim(3)-imSize)/2, center(2) - imSize/2 - 10 - dim(4) ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], [], [], txtBox );

string = '>>>';
[dim] = Screen( window, 'TextBounds', string ); % Dimensions for 1 line of text
% Bounding box for text
txtBox = [ center(1) - hShift + 1.2*dim(3)/2, center(2) + dim(4)/2, ...
    center(1) - imSize/2 - 1.2*dim(3)/2,  center(2) - dim(4) ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], [], [], txtBox );

string = '<<<';
[dim] = Screen( window, 'TextBounds', string ); % Dimensions for 1 line of text
% Bounding box for text
txtBox = [ center(1) + imSize/2 + 1.2*dim(3)/2, center(2) + dim(4)/2, ...
    center(1) + hShift - 1.2*dim(3)/2,  center(2) - dim(4) ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], [], [], txtBox );

% Present instructions
string = [ ...
    'The stripes will not be mixed together equally.\n' ...
    'Your goal will be to indicate which set of \n' ...
    'stripes is darker.' ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
WaitSecs( InstructionTime );
KbStrokeWait;

%%%
%%% Introduce an example of a trial
%%%
% Lookup - 04

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Present instructions
string = 'Here is what to expect from a trial during the experiment:';
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
WaitSecs( InstructionTime );
KbStrokeWait;

%%%
%%% Placeholder stimulus
%%%
% Lookup - 05

static_display = 1;
PlaceholderDisplay
string = 'First, you will see a set of horizontal lines:';
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
WaitSecs( InstructionTime );
KbStrokeWait;

%%%
%%% Prime
%%%
% Lookup - 06

if StudyType == 1
    % Deterimine boundaries of text
    initString =  'Next you will see a set of lines or dots that (each 1/3 of the time) can be ';
    [dim] = Screen( window, 'TextBounds', initString );
    
    NeitherPrime = 0;
    currentPrime = 0;
    PrimeDisplay
    string = [ ...
        'Next you will see a set of dots that can be \n' ...
        '1) angled in the same direction as the darkest stripes in the grid. \n' ...
        '2) angled in the opposite direction as the darkest stripes in the grid. \n' ...
        '3) angled straight up and down.' ];
    [nx,ny,bbox] = DrawFormattedText( window, string,center(1)-dim(3)/2,center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );
else
    NeitherPrime = 1;
    currentPrime = 0;
    PrimeDisplay
    
    string = [ ...
        'Next you will see a set of dots angled straight up and\n' ...
        'down. The amount of time that they are shown on screen\n' ...
        'will vary.' ];
    [nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );
end

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
WaitSecs( InstructionTime );
KbStrokeWait;

%%%
%%% Target
%%%
% Lookup - 06

TargetDisplay
string = [ ...
    'Then, surrounded by a set of dots, you will \n' ...
    'see the grid. It will flash very briefly, so \n' ...
    'it is important to pay attention!' ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
WaitSecs( InstructionTime );
KbStrokeWait;

%%%
%%% Mask
%%%
% Lookup - 07

for frame = 1:29
    MaskDisplay
    
    string = 'Next, you will see shifting static:';
    [nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
end

% Press any key to continue to the next screen
KbStrokeWait;

%%%
%%% Choice
%%%
% Lookup - 08

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

string = [ ...
    'Once you see a blank screen, use the keyboard to indicate which \n' ...
    'set of stripes in the grid you think were darker. Try to respond \n' ...
    'quickly and accurately. If you are too slow, the next trial will \n' ...
    'start. Try making a response now:' ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

[ RT, resp ] = getResponseMultiple(keyOptions(1:2),[ 0 1 ], 10000);
Accuracy = resp == correctAnswer;

%%%
%%% Feedback
%%%
% Lookup - 09

if ( Accuracy == 1 )
    DrawFormattedText(window, 'Correct!', 'center','center', [ 0 0 0 ] );
else
    DrawFormattedText(window, 'Wrong!', 'center','center', [ 0 0 0 ] );
end

string = [ 'Finally, you will receive feedback on your choice:' ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen
Screen('Flip', window);

% Press any key to continue to the next screen
WaitSecs( InstructionTime );
KbStrokeWait;

%%%
%%% Block structure
%%%
% Lookup - 10

% Deterimine boundaries of text
initString =  'Following a set of practice trials, the experiment consists of:';
[dim] = Screen( window, 'TextBounds', initString );

string = [ ...
    'The experiment consists of:\n' ...
    '1) Three blocks of training                                    \n' ...
    '2) Calibration trials                                          \n' ...
    '3) Trials for the main study                                   \n' ...
    'At the start of each block of trials, you will be shown a      \n' ...
    'progress bar indicating how many more blocks you have          \n' ...
    'before finishing.' ];
[nx,ny,bbox] = DrawFormattedText( window, string,center(1)-dim(3)/2,'center', [], [], [], [], lnSpace, [], [] );

% Flip to the screen
Screen('Flip', window);

% Press any key to continue to the next screen
KbStrokeWait;

%%%
%%% Final notes
%%%
% Lookup - 11

instruct = [ ...
    'The task should take about 30 minutes. Try to be \n' ...
    'as fast and accurate as possible! The task can be \n' ...
    'dull, and it can get hard at times, but the data \n' ...
    'are very useful. We appreciate your effort!' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, pakSpace );

% Close all textures
Screen('Close');