function [ RT, resp ] = Robot( rowIndex, param, robotWait )
% Purpose:
%   Simulates a response from a subject.
% Arguments:
%   rowIndex       - The row index from which to draw the parameters 
%                    used to simulate responses
%   param          - A matrix of parameter values for the Wald race 
%                    model used to generate parameter values
%   robotWait      - The row in the matrix of parameters for simulating 
%                    responses that corresponds to waiting for a simple
%                    button press.
% Returns:
%   A response time and choice.

if rowIndex == robotWait
    % Fix duration before 'any key' responses
    RT = param(rowIndex,1);
    resp = 2;
else
    % Simulate RT and response via a Wald race model
    
    % Simulate finishing times from a Wald race model
    t1 = rinvgauss( param(rowIndex,1), param(rowIndex,2) );
    t0 = rinvgauss( param(rowIndex,3), param(rowIndex,4) );
    
    % Decision depends on finishing time that was fastest
    obs_resp = 1;
    obs_RT = t1;
    if t0 < t1
        obs_resp = 0;
        obs_RT = t0;
    end
    % Shift decision time by non-decision component
    % to produce observed response time
    RT = obs_RT + param(rowIndex,5);
    resp = obs_resp;
    
    % Occasionally induce a timeout response
    u = rand;
    if u > .95
        RT = 5.005; % Timeout response
    end
    
end

% If desired, pause Matlab for the duration of the simulated response time
if param(rowIndex,6) == 1
    pause( RT );
end

end
