%----------------%
% Choice options %
%----------------%

% Color the screen grey
Background

if experiment_ver == 2
    % Define nuisance inputs for shifting static display
    nuisanceInputs = {
        window;
        imSize;
        choiceR;
        center;
        ovalSize1;
        ovalSize2;
        ovalRadius1;
        ovalRadius2;
        RadiusPrp;
        rot;
        nPoints;
        lnSpace;
        color_black;
        };
    
    % Record/simulate response
    [ RT, resp ] = keyboardResponses( debug, robotParam, rowIndex, ...
        keys2AFC, assignedValues, timeout, ifi, nuisanceInputs, 1, ...
        robotWait );
    
end

% Same-different task
if experiment_ver == 3
    
    % Display onscreen choice
    SameDifferentChoices
    
    % Choice options
    string = [ '     Same?', '          ', 'Different?' ];
    DrawFormattedText(window, string, ...
        'center',vChoicePos, color_black );
    
    % Flip to display
    Screen('Flip', window);
    
    % Record/simulate response
    [ RT, resp ] = keyboardResponses( debug, robotParam, rowIndex, ...
        keys2AFC, assignedValues, timeout, [], [], 0, robotWait );
    
end

% Check if timeout response
if RT > timeout
    resp = 2;
end
