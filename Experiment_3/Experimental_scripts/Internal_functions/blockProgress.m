function [ barSize ] = blockProgress( window, center, blck, nBlock, notes, opt )
% Purpose:
%   Displays the current block along with any notes and a progress bar
%   below.
% Arguments:
%   window   - A pointer to a PTB screen
%   center   - A vector with the x and y-axis values for the screen center
%              in pixels
%   blck  -    The current block number
%   nBlock   - The total number of blocks
%   notes    - A character vector of additional notes to add
%   opt      - A vector giving the y-axis offset for the progress bar, the
%              duration before the 'Press any key' is shown, the y-axis
%              offset for the 'Press any key', the line spacing for text,
%              and the width of the progress bar
% Returns:
%   The size of the progress bar.

% Extract options
offset = opt(1); % Offset for progress bar
waitTime = opt(2); % Duration before 'Press any key'
waitPos = opt(3); % Position for 'Press any key'
lnSpace = opt(4); % Line spacing
width = opt(5); % Width of bar

% Display current block
string = [ 'Block ' num2str(blck) notes ];
[ nx, ny, bbox ] = DrawFormattedText( window, string, 'center','center', [], [], [], [], lnSpace, [], [] );
% Display label for progress bar
[ nx, ny, bbox ] = DrawFormattedText( window, 'Progress:', 'center',center(2)+offset, [], [], [], [], lnSpace, [], [] );
% Calculate size of progress bar
hght = bbox(4) - bbox(2);
barSize = [ center(1) - width/2, bbox(4) + 0.9*hght, ...
            center(1) + width/2, bbox(4) + 1.9*hght ];
% Calculate how much of the bar is filled
fillPrp = width*(blck/nBlock);
fillSize = [ barSize(1), barSize(2), barSize(1)+fillPrp, barSize(4) ];
Screen('FillRect', window, [ 0 0 1 ], fillSize );
% Draw progress bar
Screen('FrameRect', window, [ 0 0 0 ], barSize, 4);

Screen('Flip', window);
WaitSecs(waitTime);

% Add in 'Press any key'
string = 'Press any key to continue';
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)+waitPos, [], [], [], [], lnSpace, [], [] );
% Current block
string = [ 'Block ' num2str(blck) notes ];
[ nx, ny, bbox ] = DrawFormattedText( window, string, 'center','center', [], [], [], [], lnSpace, [], [] );
% Display label for progress bar
[ nx, ny, bbox ] = DrawFormattedText( window, 'Progress:', 'center',center(2)+offset, [], [], [], [], lnSpace, [], [] );
% Draw progress bar
Screen('FillRect', window, [ 0 0 1 ], fillSize );
Screen('FrameRect', window, [ 0 0 0 ], barSize, 4);

Screen('Flip', window);
KbStrokeWait;

end

