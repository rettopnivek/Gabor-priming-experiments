%-----------------%
% Block structure %
% Page 5          %
%-----------------%

%%% Draw instructions %%%

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
[nx,ny,bbox] = DrawFormattedText( window, string, ...
    center(1)-dim(3)/2, 'center', [], [], [], [], lnSpace, [], [] );

% Flip to the screen
Screen('Flip', window);

% Wait for a keystroke on the keyboard:
Post_instruction
