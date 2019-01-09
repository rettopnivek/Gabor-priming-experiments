%----------------%
% Choice options %
%----------------%

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

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
    primeMask;
    NeitherPrime;
    maskString;
    optionalString;
    lnSpace;
};

if debug == 0
    % Measure response time and choice
    % [ RT, resp ] = getResponseMultiple(keyOptions(1:2),[ 0 1 ], timeout);
    [ RT, resp ] = getResponseMultipleStatic(keyOptions(1:2),[ 0 1 ], ...
        timeout, ifi, nuisanceInputs );
else
    % Simulate a response
    [ RT, resp ] = Robot( correctAnswer, currentTarget, robotPar );
end

% Check if timeout response
if RT > timeout
    resp = 2;
    accuracy = 0;
end
