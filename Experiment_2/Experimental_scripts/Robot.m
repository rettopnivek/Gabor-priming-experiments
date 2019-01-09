function [ RT, resp ] = Robot( answer, cntrst, param )
%Purpose:
% Simulates a response from a subject.
% Arguments:
% answer - the correct answer.
% cntrst - the mixing proportion for the overlaid gabors.
% param  - a vector of 4 values, the slope and cut-off for the logistic
%          function and the mean and shape parameter for the inverse 
%          gaussian.
% Returns:
% A response time and choice.

% Calculate probability of choice given a contrast level
theta = .5 + .5 ./ ( 1 + exp( -param(1) .* ( log( cntrst ) + ...
    param(2) ) ) );
if answer == 0
    theta = 1 - theta;
end;
% Simulate a response
resp = binornd(1,theta);

% Simulate a response time
RT = rinvgauss( param(3), param(4) );

end

