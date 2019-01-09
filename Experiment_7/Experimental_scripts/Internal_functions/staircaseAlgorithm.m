function [ est_ac, x_cur, cnt, interval ] = staircaseAlgorithm( y, ...
    x_cur, x_change, interval, stable_win, cnt )
% Purpose:
%   Implements a staircase algorithm for determining a log contrast value 
%   within a desired stable window using a set number of intervals.
% Arguments:
%   y          - An observed accuracy value
%   x_cur      - The current log contrast value
%   x_change   - The amount to increment the log contrast value up or down
%   interval   - A vector of size N_width containing the previous
%                accuracies
%   stable_win - The lower and upper boundaries of the stable window
%   cnt        - The current count, which resets once N_width trials 
%                occur
% Returns:
%   The estimated accuracy, the updated log contrast value, the updated 
%   count, and the updated vector of values for calculating the mean.

% Set window for moving average
N_width = size( interval, 2 );
interval( 1:(N_width-1) ) = interval( 2:N_width );
interval( N_width ) = y;

est_ac = 0; 
if cnt == N_width
    
    est_ac = mean( interval );
    
    if est_ac < stable_win(1)
        x_cur = x_cur + x_change;
    end
    
    if est_ac > stable_win(2)
        x_cur = x_cur - x_change;
    end
    
    cnt = 1; % Reset index
else
    cnt = cnt + 1; % Increment index
end

end

