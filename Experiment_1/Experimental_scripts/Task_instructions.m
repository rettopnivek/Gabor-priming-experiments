%---------------------------%
% Experimental instructions %
%---------------------------%

%{
Index:
Lookup - 01:  First set of instructions & stimulus example
Lookup - 02:  Second set of instructions & stimulus example
Lookup - 03:  Stimulus example with priming
Lookup - 04:  Study structure for psychometric version
Lookup - 05:  Study structure for priming version
Lookup - 06:  Final comments
%}

%%% Define variables for stimulus example %%%

% Create the gabor patch
cntrstPrp = .25;
cntrstLure = .5*cntrstPrp;
cntrst = .5 + cntrstLure;
rot = 45;
gabor = Overlaid_Gabors(imSize,stripes,1,cntrst,rot,gaussWin,0,1);
gaborTex = Screen('MakeTexture', window, gabor);

% Create noise patch
noiseWin = Noise_function(imSize,gaussWinNoise,0,1);
noiseWinTex = Screen('MakeTexture', window, noiseWin);

% Define an set of angles in degrees
ang = linspace( 5, 360, nPoints );

%%% First set of instructions & stimulus example %%%
% Lookup - 01

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

%%% Draw the stimulus example %%%

% Draw the overlaid gabor patch
Screen('DrawTextures', window, gaborTex, [],...
    [], [], [], [], []);

% Mask the prime
PrimeYes = 0;
RandomYes = 1;
ChoiceYes = 0;
DrawOvals

% Draw the annulus
Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );

%%% Define the instructions %%%
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'In this task, you will see within a circle a grid that ';
instruct{2} = 'is made from two sets of overlapping lines. One set of ';
instruct{3} = 'lines will be darker than the other. Your goal is to ';
instruct{4} = 'indicate in which direction the darker stripes are rotated.';
instruct{5} = 'For instance, below the darker stripes are rotated to ';
instruct{6} = 'the right.';
instruct{7} = ' ';
instruct{8} = ' ';
instruct{9} = ' ';
instruct{10} = ' ';
instruct{11} = ' ';
instruct{12} = ' ';
instruct{13} = ' ';
instruct{14} = ' ';
instruct{15} = ' ';
instruct{16} = 'Press any key to continue';
lenIns = length(instruct);

% Determine the size of the longest string of text on the x-axis
xDim = zeros(1,lenIns);
for ln = 1:(lenIns - 1)
    %
    [dim ] = Screen( window, 'TextBounds', char(instruct{1}) );
    xDim(ln) = dim(3);
end

xInstruct = center(1) - max( xDim )/2;

% Determine the size of the text on the y-axis 
[dim ] = Screen( window, 'TextBounds', char(instruct{1}) );
% Determine the y-axis coordinates for each line of text
yInstruct = ones(1,lenIns)*dim(4) + 4;
yInstruct = ( center(2) - sum(yInstruct)/2 ) + cumsum(yInstruct) - yInstruct(1);

% Display instructions
for ln = 1:(lenIns - 1)
    DrawFormattedText(window, char(instruct{ln}), xInstruct, yInstruct(ln), [ 0 0 0 ] );
end;

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Learning time
WaitSecs(2);

% Redraw the overlaid gabor patch
Screen('DrawTextures', window, gaborTex, [],...
    [], [], [], [], []);

% Mask the prime
PrimeYes = 0;
RandomYes = 1;
ChoiceYes = 0;
DrawOvals

% Draw the annulus
Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );

%%% Display the 'Press any key' statement %%%
for ln = 1:(lenIns)
    DrawFormattedText(window, char(instruct{ln}), xInstruct, yInstruct(ln), [ 0 0 0 ] );
end;

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Wait for a key press to continue
KbStrokeWait;

%%% Second set of instructions & stimulus example %%%
% Lookup - 02

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

%%% Draw the stimulus example %%%

% Draw a noise patch
Screen('DrawTextures', window, noiseWinTex, [],...
    [], [], [], [], []);

% Draw the annulus
Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );

% Mask the prime
PrimeYes = 0;
RandomYes = 0;
ChoiceYes = 1;
DrawOvals

%%% Define the instructions %%%
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'From trial to trial the grid can be rotated at different ';
instruct{2} = 'angles. Also, the grid will flash very quickly, so it is ';
instruct{3} = 'important to pay attention. After the grid flashes, you ';
instruct{4} = 'will use the "j" and "k" keys to indicate which way the ';
instruct{5} = 'darker stripes are rotated. The choices will be shown at ';
instruct{6} = 'the same angles as the stripes, as shown below: ';
instruct{7} = ' ';
instruct{8} = ' ';
instruct{9} = ' ';
instruct{10} = ' ';
instruct{11} = ' ';
instruct{12} = ' ';
instruct{13} = ' ';
instruct{14} = ' ';
instruct{15} = ' ';
instruct{16} = 'Press any key to continue';
lenIns = length(instruct);

% Determine the size of the longest string of text on the x-axis
xDim = zeros(1,lenIns);
for ln = 1:(lenIns - 1)
    %
    [dim ] = Screen( window, 'TextBounds', char(instruct{1}) );
    xDim(ln) = dim(3);
end

xInstruct = center(1) - max( xDim )/2;

% Determine the size of the text on the y-axis 
[dim ] = Screen( window, 'TextBounds', char(instruct{1}) );
% Determine the y-axis coordinates for each line of text
yInstruct = ones(1,lenIns)*dim(4) + 4;
yInstruct = ( center(2) - sum(yInstruct)/2 ) + cumsum(yInstruct) - yInstruct(1);

% Display instructions
for ln = 1:(lenIns - 1)
    DrawFormattedText(window, char(instruct{ln}), xInstruct, yInstruct(ln), [ 0 0 0 ] );
end;

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Learning time
WaitSecs(2);

% Redraw the example stimulus
% Draw a noise patch
Screen('DrawTextures', window, noiseWinTex, [],...
    [], [], [], [], []);

% Draw the annulus
Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );

% Mask the prime
PrimeYes = 0;
RandomYes = 0;
ChoiceYes = 1;
DrawOvals

%%% Display the 'Press any key' statement %%%
for ln = 1:(lenIns)
    DrawFormattedText(window, char(instruct{ln}), xInstruct, yInstruct(ln), [ 0 0 0 ] );
end;

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Wait for a key press to continue
KbStrokeWait;

%%% Stimulus example with priming %%%
% Lookup - 03

if studyType == 2
    % If running the study version using priming
    
    % Color the screen grey
    Screen('FillRect', window, [ .5 .5 .5 ]);
    
    %%% Draw the priming example %%%
    
    % Draw the annulus
    Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
    Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );
    
    % Draw the clock prime
    PrimeYes = 1;
    currentPrime = 0;
    RandomYes = 0;
    ChoiceYes = 0;
    DrawOvals
    
    %%% Define the instructions %%%
    instruct = num2str( ones(2,1) ); % Create a cell string array
    instruct = cellstr(instruct);
    % Fill the array with the individual lines of instructions
    instruct{1} = 'In some blocks, before the grid flashes, you will see a set of ';
    instruct{2} = 'dots rotated at the same angle as either the light or dark ';
    instruct{3} = 'stripes. These dots will sometimes flash extremely quickly, ';
    instruct{4} = 'and sometimes will be shown for a long time. Regardless of the ';
    instruct{5} = 'angle of the dots, you need to indicate to which side the ';
    instruct{6} = 'darker stripes are rotated.';
    instruct{7} = ' ';
    instruct{8} = ' ';
    instruct{9} = ' ';
    instruct{10} = ' ';
    instruct{11} = ' ';
    instruct{12} = ' ';
    instruct{13} = ' ';
    instruct{14} = ' ';
    instruct{15} = ' ';
    instruct{16} = 'Press any key to continue';
    lenIns = length(instruct);
    
    % Determine the size of the longest string of text on the x-axis
    xDim = zeros(1,lenIns);
    for ln = 1:(lenIns - 1)
        %
        [dim ] = Screen( window, 'TextBounds', char(instruct{1}) );
        xDim(ln) = dim(3);
    end
    
    xInstruct = center(1) - max( xDim )/2;
    
    % Determine the size of the text on the y-axis
    [dim ] = Screen( window, 'TextBounds', char(instruct{1}) );
    % Determine the y-axis coordinates for each line of text
    yInstruct = ones(1,lenIns)*dim(4) + 4;
    yInstruct = ( center(2) - sum(yInstruct)/2 ) + cumsum(yInstruct) - yInstruct(1);
    
    % Display instructions
    for ln = 1:(lenIns - 1)
        DrawFormattedText(window, char(instruct{ln}), xInstruct, yInstruct(ln), [ 0 0 0 ] );
    end;
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Learning time
    WaitSecs(2);
    
    % Redraw the priming example
    % Draw the annulus
    Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
    Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );
    
    % Draw the clock prime
    PrimeYes = 1;
    RandomYes = 0;
    ChoiceYes = 0;
    DrawOvals
    
    %%% Display the 'Press any key' statement %%%
    for ln = 1:(lenIns)
        DrawFormattedText(window, char(instruct{ln}), xInstruct, yInstruct(ln), [ 0 0 0 ] );
    end;
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Wait for a key press to continue
    KbStrokeWait;
    
end

%%% Study structure for psychometric version %%%
% Lookup - 04

% Determine number of blocks and trials to report
LB = nE*nLoops + 1;
SB = nLoops - 1;
LT = nBlockTrials;
ST = trialsAdaptiveLoop;
numBl = 'blocks';
if SB == 1
    numBl = 'block';
end

if studyType == 1
    % If running the psychometric version
    
    %%% Define the instructions %%%
    instruct = num2str( ones(2,1) ); % Create a cell string array
    instruct = cellstr(instruct);
    % Fill the array with the individual lines of instructions
    instruct{1} = 'This study has two parts:';
    instruct{2} = [ '1) A set of ' num2str(nPBtask) ' practice trials that get progressively harder '];
    instruct{3} = '   to help you get use to the task.';
    instruct{4} = [ '2) ' num2str(LB) ' blocks of ' num2str(LT) ' trials, and ' num2str(SB) ' short ' numBl ' of ' num2str(ST) ' trials, ' ];
    instruct{5} = '   in which you will be tested on the full range of difficulties.';
    instruct{6} = ' ';
    instruct{7} = ' ';
    instruct{8} = ' ';
    instruct{9} = ' ';
    instruct{10} = ' ';
    instruct{11} = ' ';
    instruct{12} = ' ';
    instruct{13} = ' ';
    instruct{14} = ' ';
    instruct{15} = ' ';
    instruct{16} = 'Press any key to continue';
    lenIns = length(instruct);
    
    % Display the instructions
    displayInstruct % Call separate Matlab script
    
end

%%% Study structure for priming version %%%
% Lookup - 05

% Determine number of blocks and trials to report
LB = nE*nLoops;
SB = nLoops - 1;
LT = nBlockTrials;
ST = trialsAdaptiveLoop;
numBl = 'blocks';
if SB == 1
    numBl = 'block';
end

% '   to help you get use to the task (no dots will be shown ';
% '   Intermixed with these blocks will be 2 short blocks with';
% '   32 trials.'

if studyType == 2
    % If running the priming version
    
    %%% Define the instructions %%%
    instruct = num2str( ones(2,1) ); % Create a cell string array
    instruct = cellstr(instruct);
    % Fill the array with the individual lines of instructions
    instruct{1} = 'This study has two parts:';
    instruct{2} = [ '1) A set of ' num2str(nPBtask) ' practice trials that get progressively harder ' ];
    instruct{3} = '   to help you get use to the task (no dots will be shown ';
    instruct{4} = '   before the grid flashes).';
    instruct{5} = [  '2) ' num2str(LB+1) ' blocks of ' num2str(LT) ' trials each, in which the dots will be ' ];
    instruct{6} = '   shown, followed by the grid flash and the choices. ';
    instruct{7} = [ '   Intermixed with these blocks will be ' num2str(SB) ' short ' numBl ' with ' ];
    instruct{8} = [ '   ' num2str(ST) ' trials. ' ];
    instruct{9} = ' ';
    instruct{10} = ' ';
    instruct{10} = ' ';
    instruct{11} = ' ';
    instruct{12} = ' ';
    instruct{13} = ' ';
    instruct{14} = ' ';
    instruct{15} = ' ';
    instruct{16} = 'Press any key to continue';
    lenIns = length(instruct);
    
    % Display the instructions
    displayInstruct % Call separate Matlab script
    
end

%%% Final comments %%%
% Lookup - 06

%%% Define the instructions %%%
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'The study should take about 45 minutes. Try to be as fast ';
instruct{2} = 'and accurate as possible! The task can be dull, and it can ';
instruct{3} = 'get hard at times, but the data is very useful. We appreciate ';
instruct{4} = 'your effort!';
instruct{5} = ' ';
instruct{6} = ' ';
instruct{7} = ' ';
instruct{8} = ' ';
instruct{9} = ' ';
instruct{10} = ' ';
instruct{11} = ' ';
instruct{12} = ' ';
instruct{13} = ' ';
instruct{14} = ' ';
instruct{15} = ' ';
instruct{16} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

