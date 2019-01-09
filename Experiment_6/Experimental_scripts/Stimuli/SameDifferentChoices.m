%-----------------------------------%
% Choice display for same/different %
%-----------------------------------%


if onscreenChoices == 1
    % Same
    
    % Create gabor patch example rotated in same direction as target
    gabor = Overlaid_Gabors(imSize,PP,.5,0,thetaT,stripes,choiceR(1),0);
    
end

if onscreenChoices == 2
    % Different
    
    % Create gabor patch example rotated in opposite direction as target
    if targetRotation == 1
        % Rotate left
        gabor = Overlaid_Gabors(imSize,PP,.5,0,thetaT - 90,stripes,choiceR(1),0);
    else
        % Rotate right
        gabor = Overlaid_Gabors(imSize,PP,.5,0,thetaT + 90,stripes,choiceR(1),0);
    end
    
end

% Create texture
gaborChoiceTex = Screen('MakeTexture', window, gabor);

% Draw texture to screen
Screen('DrawTextures', window, gaborChoiceTex, [], ...
    topPos, [], [], [], []);
Screen('DrawTextures', window, gaborChoiceTex, [], ...
    botPos, [], [], [], []);
