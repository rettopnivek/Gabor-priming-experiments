%---------------------------------%
% Common stimuli for instructions %
%---------------------------------%

% Target and foil contrasts
TC = .25;
FC = 0;

% Create gabor patch oriented to the left
gabor = Overlaid_Gabors( imSize, PP, ...
    TC, FC, -45, stripesTarget, stimWin, 0 );
gaborTexLeft = Screen('MakeTexture', window, gabor);

% Create gabor patch oriented to the right
gabor = Overlaid_Gabors( imSize, PP, ...
    TC, FC, 45, stripesTarget, stimWin, 0 );
gaborTexRight = Screen('MakeTexture', window, gabor);

% Create grid
gabor = Overlaid_Gabors(imSize,PP,TC,TC,-45,stripesTarget,stimWin,0);
gaborTexGuess = Screen('MakeTexture', window, gabor);

% Target and foil contrasts
TC1 = .15;
TC2 = .05;
FC = 0;

% Create gabor patch oriented to the left
gabor = Overlaid_Gabors(imSize,PP,TC,FC,-45,stripesTarget,stimWin,0);
gaborTexLeft2 = Screen('MakeTexture', window, gabor);

% Create gabor patch oriented to the right
gabor = Overlaid_Gabors(imSize,PP,TC2,FC,45,stripesTarget,stimWin,0);
gaborTexRight2 = Screen('MakeTexture', window, gabor);

% Create grid
gabor = Overlaid_Gabors(imSize,PP,TC1,TC2,-45,stripesTarget,stimWin,0);
gaborTexEasy = Screen('MakeTexture', window, gabor);

% Positions for textures
hShift = 90;
posLeft = [ ...
    center(1) - imSize - hShift, ...
    center(2) - imSize/2 , ...
    center(1) - hShift, ...
    center(2) + imSize/2 ...
    ];
posRight = [ ...
    center(1) + hShift, ...
    center(2) - imSize/2 , ...
    center(1) + imSize + hShift, ...
    center(2) + imSize/2 ...
    ];
posCenter = [ ...
    center(1) - imSize/2, ...
    center(2) - imSize/2 , ...
    center(1) + imSize/2, ...
    center(2) + imSize/2 ...
    ];
