%----------------%
% Choice options %
%----------------%

% Color the screen grey
Background

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

% Check if timeout response
if RT > timeout
    resp = 2;
end
