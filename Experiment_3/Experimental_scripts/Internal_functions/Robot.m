function [ RT, resp ] = Robot( answer, cntrst, param )
% Purpose:
%   Simulates a response from a subject.
% Arguments:
%   answer - The correct answer.
%   cntrst - The contrast for the target grating
%   param  - A vector of 4 values, the slope and cut-off for the logistic
%            function and the mean and shape parameter for the inverse 
%            gaussian
% Returns:
%   A response time and choice.

% Calculate probability of choice given a contrast level
theta = f_alpha_beta( log(cntrst), param(1), param(2) );

if answer == 0
    theta = 1 - theta;
end;

% Simulate a response
resp = binornd(1,theta);

% Simulate a response time
RT = rinvgauss( param(3), param(4) );

if param(5) == 1
    pause( RT );
end

end
