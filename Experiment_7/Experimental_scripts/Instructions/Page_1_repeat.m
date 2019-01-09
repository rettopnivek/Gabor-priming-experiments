%----------------------------------%
% Introduction for repeat sessions %
% Page 1                           %
%----------------------------------%

% Color the screen grey
Background

instruct = [ ...
    'Thanks for coming in for another session! \n' ...
    'There will be a review of the task just \n' ...
    'in case, and then you can get started.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, ...
    lnSpace, InstructionTime, pakSpace );

% Wait for a keystroke on the keyboard:
Post_instruction