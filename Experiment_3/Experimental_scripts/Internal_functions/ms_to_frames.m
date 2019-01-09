function [ number_frames ] = ms_to_frames( interval, ifi )
% Purpose:
%   Calculates the number of frames to approximate a set time interval in 
%   ms.
% Arguments:
%   interval - The interval of time (in ms)
%   ifi      - The screen refresh rate (in ms)
% Returns:
%   The number of frames needed to approximate the time interval given the
%   refresh rate.

cutoff = (ifi)/2;
remainder = mod(interval,ifi);
if (remainder > cutoff)
    number_frames = ceil(interval/ifi);
end;
if (remainder <= cutoff)
    number_frames = floor(interval/ifi);

end

