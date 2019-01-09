%----------------%
% Fixation point %
%----------------%

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Draw a fixation dot
Screen('DrawDots', window, [center(1); center(2)], dotSizePix, dotColor, [], 0);
