%----------------------------------%
% Performance feedback for a block %
%----------------------------------%

%{
Notes forthcoming
%}

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);
clrExample = [ .2 .2 .2 ]; % Color of progress bar

% Provide a progress bar indicating how far a subject has gotten through
% the experiment
string = 'Progress:' ;
[dim] = Screen(window, 'TextBounds', string);
strPos = dim(4)/2 + round( dim(4)*2 );
DrawFormattedText(window, string, 'center',center(2)-strPos, clrExample );

% Define dimensions of progress bar
totalLength = round( ( center(1)*2 )*.2 );
startX = round( center(1) - totalLength/2 );
clrBorders = [ 0 0 0 ];
clrProgress = [ .2 .2 .2 ];

% Draw lines to indicate borders of the progress bar
lineWidthPix = 5;
Screen('DrawLine', window, clrBorders, ...
    startX, round( center(2) - dim(4)*.8 ), ...
    startX, round( center(2) + dim(4)*.8 ), lineWidthPix);
Screen('DrawLine', window, clrBorders, ...
    startX+totalLength, round( center(2) - dim(4)*.8 ), ...
    startX+totalLength, round( center(2) + dim(4)*.8 ), lineWidthPix);

rectProgress = [ startX, ...
    round( center(2) - dim(4)/2 ), ...
    round( startX + totalLength*( blck/nBlocks ) ), ...
    round( center(2) + dim(4)/2 ) ];

% Draw the progress bar
Screen('FillRect', window, clrProgress, rectProgress);

% Display on screen
Screen('Flip', window);

WaitSecs( 2 );