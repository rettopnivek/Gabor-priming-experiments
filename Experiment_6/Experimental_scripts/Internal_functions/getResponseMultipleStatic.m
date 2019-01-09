function [ RT, resp ] = getResponseMultipleStatic( keys, valuesAssigned, timeout, frameTime, nuisanceInputs )
% Purpose:
%   A function to measure the choice and response time while displaying
%   a constantly shifting pattern of static and a mask for prime choices.
% Arguments:
%   keys           - A vector giving the number(s) representing the key(s)
%                    to press to a particular choice (use KbName function
%                    to determine the number)
%   valuesAssigned - The response values assigned to each choice
%   timeout        - The interval of time to record a response
%   frameTime      - The duration (in seconds) for a frame refresh
%   nuisanceInputs - An array with a the inputs for the static and
%                    prime masks
% Returns
%   RT   - A response time (in seconds), calculated starting from when the
%          function was called.
%   resp - Indicates whether the left choice (1) or right choice (2) was
%          picked.

window = nuisanceInputs{1};
imSize = nuisanceInputs{2};
choiceR = nuisanceInputs{3};
center = nuisanceInputs{4};
ovalSize1 = nuisanceInputs{5};
ovalSize2 = nuisanceInputs{6};
ovalRadius1 = nuisanceInputs{7};
ovalRadius2 = nuisanceInputs{8};
RadiusPrp = nuisanceInputs{9};
rot = nuisanceInputs{10};
nPoints = nuisanceInputs{11};
lnSpace = nuisanceInputs{12};
color_black = nuisanceInputs{13};

% Get the starting time
st_time = GetSecs;
trackFrame = st_time;

% Determine how long to check for a response
stop = st_time + timeout;

% Initialize measurements
resp = 0;
RT = 0;

% While no response is registered
while ~resp
    
    % Check for input
    [ press, secs, keynum ] = KbCheck;
    
    % Check if the input is valid
    if press
        if ( sum( keynum( keys ) ) == 1 )
            RT = secs - st_time; % Calculate response time
            resp = valuesAssigned( keynum( keys ) == 1 );
            break;
        end
    end
    
    if (GetSecs > stop)
        break;
    end;
    
    if ( GetSecs > trackFrame )
        
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
        
        % Flip to the screen (i.e. display stimuli)
        Screen('Flip', window);
        
        trackFrame = trackFrame + frameTime;
    end;
    
    % Delay measurment for .5 ms
    WaitSecs(0.0005);
end

if (RT == 0)
    RT = GetSecs - st_time;
end;

FlushEvents('keyDown');

end

