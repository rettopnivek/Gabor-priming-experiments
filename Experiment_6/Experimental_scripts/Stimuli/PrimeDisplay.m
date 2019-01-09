%-------%
% Prime %
%-------%

% Color the screen grey
Background

% Draw the annulus
Annulus

% Prime using high contrast gabor patch
gaborTex = Screen('MakeTexture', window, gaborPrime);
Screen('DrawTextures', window, gaborTex, [],...
    [], [], [], [], []);
