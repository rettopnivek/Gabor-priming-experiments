%--------------------------------%
% Introduction for first session %
% Page 1                         %
%--------------------------------%

% Color the screen grey
Background

instruct = [ ...
    'Thanks for agreeing to take my study! \n' ...
    'If possible, it would be great if you \n' ...
    'come back for two more sessions. Now, \n' ...
    'we will introduce the task.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, ...
    lnSpace, InstructionTime, pakSpace );

% Wait for a keystroke on the keyboard:
Post_instruction