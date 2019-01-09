%---------------------------%
% Experimental instructions %
%---------------------------%

%{
Index:
Lookup - 01:  First set of instructions & examples of stimuli
Lookup - 02:  Second set of instructions & examples of stimuli
Lookup - 03:  First frame (prime) from a single trial
Lookup - 04:  Second frame (target) from a single trial
Lookup - 05:  Third frame (choices) from a single trial
Lookup - 06:  Fourth frame (feedback) from a single trial
Lookup - 07:  Final details on task
%}

%%% Define variables for stimulus example %%%

% Create left gabor patch
cntrstPrp = 1;
cntrstLure = .5*cntrstPrp;
cntrst = .5 - cntrstLure;
rot = 45;
gabor = Overlaid_Gabors(imSize,stripes,amp,cntrst,rot,gaussWin,0,1,stepYes);
gaborTexLeft = Screen('MakeTexture', window, gabor);

% Create right gabor patch
cntrstPrp = 1;
cntrstLure = .5*cntrstPrp;
cntrst = .5 + cntrstLure;
rot = 45;
gabor = Overlaid_Gabors(imSize,stripes,amp,cntrst,rot,gaussWin,0,1,stepYes);
gaborTexRight = Screen('MakeTexture', window, gabor);

% Create gabor grid
cntrstPrp = .6;
cntrstLure = .5*cntrstPrp;
cntrst = .5 + cntrstLure;
rot = 45;
gabor = Overlaid_Gabors(imSize,stripes,amp,cntrst,rot,gaussWin,0,1,stepYes);
gaborTexEasy = Screen('MakeTexture', window, gabor);

% Create gabor grid
cntrstPrp = .6;
cntrstLure = .5*cntrstPrp;
cntrst = .5 - cntrstLure;
rot = 45;
gabor = Overlaid_Gabors(imSize,stripes,amp,cntrst,rot,gaussWin,0,1,stepYes);
gaborTexEasy2 = Screen('MakeTexture', window, gabor);

% Create gabor grid
cntrstPrp = 0;
cntrstLure = .5*cntrstPrp;
cntrst = .5 + cntrstLure;
rot = 45;
gabor = Overlaid_Gabors(imSize,stripes,amp,cntrst,rot,gaussWin,0,1,stepYes);
gaborTexHard = Screen('MakeTexture', window, gabor);

% Create noise patch
noiseWin = Noise_function(imSize,gaussWinNoise,0,1);
noiseWinTex = Screen('MakeTexture', window, noiseWin);

% Define an set of angles in degrees
ang = linspace( 5, 360, nPoints );

%%%
%%% First set of instructions & examples of stimuli %%%
%%%
% Lookup - 01

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

%%% Draw the stimuli %%%

% Draw the gabor patches
hShift = 90;
Screen('DrawTextures', window, gaborTexLeft, [],...
    [center(1)-imSize-hShift,center(2)-imSize/2,center(1)-hShift,center(2)+imSize/2], ...
    [], [], [], []);
Screen('DrawTextures', window, gaborTexRight, [],...
    [center(1)+hShift,center(2)-imSize/2,center(1)+imSize+hShift,center(2)+imSize/2], ...
    [], [], [], []);
Screen('DrawTextures', window, gaborTexHard, [],...
    [center(1)-imSize/2,center(2)-imSize/2,center(1)+imSize/2,center(2)+imSize/2], ...
    [], [], [], []);

% Include labels
string = 'Left';
[dim] = Screen( window, 'TextBounds', string ); % Dimensions for 1 line of text
% Bounding box for text
txtBox = [ center(1) - imSize - hShift - 1.2*(dim(3)-imSize)/2, center(2) - imSize/2 - 10, ... 
    center(1) - hShift + 1.2*(dim(3)-imSize)/2, center(2) - imSize/2 - 10 - dim(4) ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], [], [], txtBox );

string = 'Right';
[dim] = Screen( window, 'TextBounds', string ); % Dimensions for 1 line of text
% Bounding box for text
txtBox = [ center(1) + hShift - 1.2*(dim(3)-imSize)/2, center(2) - imSize/2 - 10, ... 
    center(1) + imSize + hShift + 1.2*(dim(3)-imSize)/2, center(2) - imSize/2 - 10 - dim(4) ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], [], [], txtBox );

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
string = 'In this task, you will see a grid made by \nmixing 2 sets of stripes angled to the left \nand the right respectively.';
lnSpace = 1.5;
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
if ( debug == 0 )
    WaitSecs( instructionWaitTime );
end;
KbStrokeWait;

%%%
%%% Second set of instructions & examples of stimuli %%%
%%%
% Lookup - 02

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

%%% Draw the stimuli %%%

% Draw the gabor patches
hShift = 90;
Screen('DrawTextures', window, gaborTexLeft, [],...
    [center(1)-imSize-hShift,center(2)-imSize/2,center(1)-hShift,center(2)+imSize/2], ...
    [], [], [], []);
Screen('DrawTextures', window, gaborTexRight, [],...
    [center(1)+hShift,center(2)-imSize/2,center(1)+imSize+hShift,center(2)+imSize/2], ...
    [], [], [], []);
Screen('DrawTextures', window, gaborTexEasy, [],...
    [center(1)-imSize/2,center(2)-imSize/2,center(1)+imSize/2,center(2)+imSize/2], ...
    [], [], [], []);

% Include labels
string = '20%';
[dim] = Screen( window, 'TextBounds', string ); % Dimensions for 1 line of text
% Bounding box for text
txtBox = [ center(1) - imSize - hShift - 1.2*(dim(3)-imSize)/2, center(2) - imSize/2 - 10, ... 
    center(1) - hShift + 1.2*(dim(3)-imSize)/2, center(2) - imSize/2 - 10 - dim(4) ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], [], [], txtBox );

string = '80%';
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

string = 'Angled right';
[dim] = Screen( window, 'TextBounds', string ); % Dimensions for 1 line of text
% Bounding box for text
txtBox = [ center(1) - 1.2*dim(3)/2, center(2) + imSize/2 + dim(4) - 10, ... 
    center(1) + 1.2*dim(3)/2,  center(2) + imSize/2 - 10 ];
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], [], [], txtBox );

% Present instructions
string = 'The stripes will not be mixed together equally.\nOne set of stripes will appear to be darker,\nand your goal is to indicate its angle.';
lnSpace = 1.5;
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
if ( debug == 0 )
    WaitSecs( instructionWaitTime );
end;
KbStrokeWait;

%%% Introduce an example of a trial %%%

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Present instructions
string = 'Here is an example of a single trial.';
lnSpace = 1.5;
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
KbStrokeWait;

%%%
%%% First frame (prime) from a single trial %%%
%%%
% Lookup - 03

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Draw the annulus
Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );

% Draw the clock prime
PrimeYes = 1;
currentPrime = 0;
RandomYes = 0;
ChoiceYes = 0;
DrawOvals

% Present instructions
string = 'First a set of dots will flash briefly. Their angle \nwill only match the darker set half of the time.';
lnSpace = 1.5;
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
if ( debug == 0 )
    WaitSecs( instructionWaitTime );
end;
KbStrokeWait;

%%%
%%% Second frame (target) from a single trial %%%
%%%
% Lookup - 04

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Draw the overlaid gabor patch
Screen('DrawTextures', window, gaborTexEasy, [],...
    [], [], [], [], []);

% Mask the prime
PrimeYes = 0;
RandomYes = 1;
ChoiceYes = 0;
DrawOvals

% Draw the annulus
Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );

% Present instructions
string = 'Next you will see the grid flash very briefly, \nso it is important to pay attention.';
lnSpace = 1.5;
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
if ( debug == 0 )
    WaitSecs( instructionWaitTime );
end;
KbStrokeWait;

%%%
%%% Third frame (choices) from a single trial %%%
%%%
% Lookup - 05

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Draw a noise patch
Screen('DrawTextures', window, noiseWinTex, [],...
    [], [], [], [], []);

% Mask the prime
PrimeYes = 0;
RandomYes = 0;
ChoiceYes = 1;
DrawOvals

% Draw the annulus
Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );

% Present instructions
string = 'Press J if the darker set was angled\nto the left, or K if the darker set was angled\nto the right. Give it a try.';
lnSpace = 1.5;
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
if ( debug == 0 )
    % Have subjects make an actual choice
    [ RT, resp ] = getResponseMultiple(keyOptions(1:2),0:1,120);
    Accuracy = resp == 1;
else
    Accuracy = 0;
    KbStrokeWait;
end;

%%%
%%% Fourth frame (feedback) from a single trial %%%
%%%
% Lookup - 06

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Present instructions
string = 'Finally, you will receive feedback on your \nperformance. You will be told if you were \ncorrect or wrong.';
lnSpace = 1.5;
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

if ( Accuracy == 1 )
    DrawFormattedText(window, 'Correct!', 'center','center', [ 0 0 0 ] );
else
    DrawFormattedText(window, 'Wrong!', 'center','center', [ 0 0 0 ] );
end

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
if ( debug == 0 )
    WaitSecs( instructionWaitTime );
end;
KbStrokeWait;

%%%
%%% Final details on task %%%
%%%
% Lookup - 07

% Determine number of blocks and trials to report
LB = nE*nLoops;
SB = nLoops - 1;
LT = nBlockTrials;
ST = trialsAdaptiveLoop;
numBl = [ num2str(SB) ' short blocks' ];
if SB == 1
    numBl = '1 short block';
end

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Present instructions
string = [ 'There will be ' num2str(nPBtask) ...
    ' practice trials, which start \n' ...
    'off easy and get progressively harder. The \n' ...
    'main experiment has ' num2str(LB+1) ...
    ' blocks with ' num2str(LT) ' trials, \n' ...
    'and ' numBl ' with ' num2str(ST) ' trials.' ];
lnSpace = 1.5;
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
KbStrokeWait;

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Present instructions
string = 'The task should take 30 - 40 minutes. Try to be \nas fast and accurate as possible! The task can be \ndull, and it can get hard at times, but the data \nare very useful. We appreciate your effort!';
lnSpace = 1.5;
[nx,ny,bbox] = DrawFormattedText( window, string,'center','center', [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
if ( debug == 0 )
    WaitSecs( instructionWaitTime );
end;
KbStrokeWait;