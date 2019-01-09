%--------------------------------%
% Initial description of stimuli %
% Page 1                         %
%--------------------------------%

%%% Display instructions %%%

% Color the screen grey
Background

% Draw the gabor patches
Screen('DrawTextures', window, gaborTexLeft, [], posLeft, ...
    [], [], [], []);
Screen('DrawTextures', window, gaborTexRight, [], posRight, ...
    [], [], [], []);
Screen('DrawTextures', window, gaborTexGuess, [], posCenter, ...
    [], [], [], []);

% Add labels
string = 'Mixed';
% Dimensions for 1 line of text
[dim] = Screen( window, 'TextBounds', string );
% Bounding box for text
txtBox = [ ...
    center(1) - 1.2*dim(3)/2, ...
    center(2) + imSize/2 + dim(4) - 10, ...
    center(1) + 1.2*dim(3)/2, ...
    center(2) + imSize/2 - 10 ];
[nx,ny,bbox] = DrawFormattedText( window, string, ...
    'center','center', [], [], [], [], [], [], txtBox );

string = '>>>';
% Dimensions for 1 line of text
[dim] = Screen( window, 'TextBounds', string );
% Bounding box for text
txtBox = [ center(1) - hShift + 1.2*dim(3)/2, center(2) + dim(4)/2, ...
    center(1) - imSize/2 - 1.2*dim(3)/2,  center(2) - dim(4) ];
[nx,ny,bbox] = DrawFormattedText( window, string, ...
    'center','center', [], [], [], [], [], [], txtBox );

string = '<<<';
% Dimensions for 1 line of text
[dim] = Screen( window, 'TextBounds', string );
% Bounding box for text
txtBox = [ center(1) + imSize/2 + 1.2*dim(3)/2, center(2) + dim(4)/2, ...
    center(1) + hShift - 1.2*dim(3)/2,  center(2) - dim(4) ];
[nx,ny,bbox] = DrawFormattedText( window, string, ...
    'center','center', [], [], [], [], [], [], txtBox );

% Present instructions
string = [ ...
    'In this task, you will see a grid made by \n' ...
    'mixing 2 sets of stripes. Each set of stripes \n' ...
    'has a matching key on the keyboard.' ];
[nx,ny,bbox] = DrawFormattedText( window, string, 'center', ...
    center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Press any key to continue to the next screen
WaitSecs( InstructionTime );
Post_instruction
