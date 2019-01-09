%--------%
% Target %
%--------%

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Draw the overlaid gabor patches
gaborTex = Screen('MakeTexture', window, gabor);
Screen('DrawTextures', window, gaborTex, [],...
    [], [], [], [], []);

% Draw the annulus
Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );

% Mask the prime
PrimeYes = 2;
PrimeMask = 1;
DrawOvals
