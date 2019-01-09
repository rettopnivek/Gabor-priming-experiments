%------%
% Mask %
%------%

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Create noise patch
NoisePatch = Noise_function(imSize,choiceR(1),0,1);
noiseWinTex = Screen('MakeTexture', window, NoisePatch);

% Draw the noise patch
Screen('DrawTextures', window, noiseWinTex, [],...
    [], [], [], [], []);

% Draw the annulus
Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );

% Mask the prime
PrimeYes = 2;
DrawOvals
