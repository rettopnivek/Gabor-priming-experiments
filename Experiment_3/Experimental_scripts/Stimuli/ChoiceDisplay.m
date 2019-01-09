%----------------%
% Choice options %
%----------------%

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Choice options
% 1) Angled left/right assignments
% 2) Alternating forced choice assignments
% 3) Blank left/right assignments

% Draw a noise patch
% Create noise patch
% NoisePatch = Noise_function(imSize,choiceR,0,1);
% noiseWinTex = Screen('MakeTexture', window, NoisePatch);

% Draw the noise patch
% Screen('DrawTextures', window, noiseWinTex, [],...
%    [], [], [], [], []);

% Draw the annulus
% Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
% Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );

% Mask the prime
% PrimeYes = 2;
% DrawOvals

% Flip to the screen
Screen('Flip', window);

if debug == 0
    % Measure response time and choice
    [ RT, resp ] = getResponseMultiple(keyOptions(1:2),[ 0 1 ], timeout);
else
    % Simulate a response
    [ RT, resp ] = Robot( correctAnswer, currentTarget, robotPar );
end

% Check if timeout response
if RT > timeout
    resp = 2;
    accuracy = 0;
end
