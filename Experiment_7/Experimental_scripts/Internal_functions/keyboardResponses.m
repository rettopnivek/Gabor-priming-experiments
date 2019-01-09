function [ RT, resp ] = keyboardResponses( debug, param, rowIndex, ...
    keys, valuesAssigned, timeout, frameTime, nuisanceInputs, ...
    staticYes, robotWait )
% Purpose:
%   Manages both actual and simulated keyboard presses for the 
%   experiment.
% Arguments:
%   debug          - If 0, collects responses from subject, otherwise
%                    responses are simulated
%   rowIndex       - The row index from which to draw the parameters 
%                    used to simulate responses
%   param          - A matrix of parameter values for the Wald race 
%                    model used to generate parameter values
%   keys           - A vector giving the number(s) representing the key(s)
%                    to press to a particular choice (use KbName function
%                    to determine the number)
%   valuesAssigned - The response values assigned to each choice
%   timeout        - The interval of time to record a response
%   frameTime      - The duration (in seconds) for a frame refresh
%   nuisanceInputs - An array with a the inputs for the static and
%                    prime masks
%   staticYes      - Set to 1 to have shifting static until a subject 
%                    makes a response
%   robotWait      - The row in the matrix of parameters for simulating 
%                    responses that corresponds to waiting for a simple
%                    button press.
% Returns:
%   A response time and choice.

% For 'any key' responses
if rowIndex == robotWait
    
    if debug == 0
        % For subject-based responses
        
        % Wait for a keystroke on the keyboard:
        KbStrokeWait;
        
    else
        % For simulated responses
        
        % Don't record any response
        Robot( rowIndex, param, robotWait );
        
    end
    
end

% Experimental responses
if rowIndex ~= robotWait
        
    if debug == 0
        % For subject-based responses
        
            if staticYes == 1
                % With static
                [ RT, resp ] = getResponseMultipleStatic( keys, valuesAssigned, timeout, frameTime, nuisanceInputs );
            else
                % Without static
                [ RT, resp ] = getResponseMultiple( keys, valuesAssigned, timeout );
            end
    else
        % For simulated responses
        [ RT, resp ] = Robot( rowIndex, param, robotWait );
    end
    
end

end

