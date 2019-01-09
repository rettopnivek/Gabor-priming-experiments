%--------%
% Target %
%--------%

% Color the screen grey
Background

% Draw the overlaid gabor patches
gaborTex = Screen('MakeTexture', window, gabor);
Screen('DrawTextures', window, gaborTex, [],...
    [], [], [], [], []);

% Draw the annulus
Annulus

% Mask the prime
DrawOvals
