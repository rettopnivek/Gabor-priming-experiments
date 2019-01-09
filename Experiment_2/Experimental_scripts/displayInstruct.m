%------------------------------%
% Code to display instructions %
%------------------------------%

% Determine the size of the longest string of text on the x-axis

xDim = zeros(1,lenIns);
for ln = 1:(lenIns - 1)
    %
    dim = Screen( window, 'TextBounds', char(instruct{ln}) );
    xDim(ln) = dim(3);
end

xInstruct = center(1) - max( xDim )/2;

% Determine the size of the text on the y-axis 
dim = Screen( window, 'TextBounds', char(instruct{1}) );
% Determine the y-axis coordinates for each line of text
yInstruct = ones(1,lenIns)*dim(4) + 4;
yInstruct = ( center(2) - sum(yInstruct)/2 ) + cumsum(yInstruct) - yInstruct(1);

% Display instructions
for ln = 1:(lenIns - 1)
    DrawFormattedText(window, char(instruct{ln}), xInstruct, yInstruct(ln), [ 0 0 0 ] );
end;

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Learning time
WaitSecs(2);

% Display the 'Press any key' statement
for ln = 1:(lenIns)
    DrawFormattedText(window, char(instruct{ln}), xInstruct, yInstruct(ln), [ 0 0 0 ] );
end;

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

if debug == 0
    % Wait for a key press to continue
    KbStrokeWait;
else
    WaitSecs(.2)
end;
