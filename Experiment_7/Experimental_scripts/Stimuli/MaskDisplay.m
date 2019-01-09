%------%
% Mask %
%------%

% Color the screen grey
Background

% Create noise patch
NoisePatch = Noise_function(imSize,choiceR(1),0,1);
noiseWinTex = Screen('MakeTexture', window, NoisePatch);

% Draw the noise patch
Screen('DrawTextures', window, noiseWinTex, [],...
    [], [], [], [], []);

% Draw the annulus
Annulus

% Mask the prime
DrawOvals
