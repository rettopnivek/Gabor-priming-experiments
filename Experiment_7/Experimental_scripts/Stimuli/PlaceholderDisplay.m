%-------------%
% Placeholder %
%-------------%

% Color the screen grey
Background

% Create noise patch
if static_display == 1
    NoisePatch = Overlaid_Gabors(imSize,PP,1,0,180,stripes,choiceR,0);
    static_display = static_display + 1;
end
noiseWinTex = Screen('MakeTexture', window, NoisePatch);

% Draw the noise patch
Screen('DrawTextures', window, noiseWinTex, [],...
    [], [], [], [], []);

% Draw the annulus
Annulus