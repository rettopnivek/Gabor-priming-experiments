%-------%
% Prime %
%-------%

% Present an annulus. Within the ring, present a set of dots at different
% angles to function as the prime.

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Draw the annulus
Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );

% Draw prime in the annulus

% Prime using dots
if PrimeVer == 1
    PrimeYes = 1;
    DrawOvals
else
    % Prime using high contrast gabor patch
    gaborTex = Screen('MakeTexture', window, gaborPrime);
    Screen('DrawTextures', window, gaborTex, [],...
        [], [], [], [], []);
end