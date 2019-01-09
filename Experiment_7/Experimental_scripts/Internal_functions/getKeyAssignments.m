function [ keyNumbers ] = getKeyAssignments( string, stVal, endVal )
% Purpose:
%   A function to identify the numbers assigned to specific keyboard keys
% Arguments:
%   string - A vector of the characters whose numerical values are needed 
%            (e.g. 'asdf')
%   stVal  - The numerical value at which to begin the search
%   endVal - The numerical value at which to conclude the search
% Returns:
%   A vector of numbers, which can be used in functions like 'getResponse' 
%   or 'keynum'

nInputs = length(string);
allKeys = KbName(stVal:endVal);
nKeys = length(allKeys);
keyNumbers = zeros(1,nInputs);
indexVal = stVal:endVal;

% Loop through the inputs
for ni = 1:nInputs
    % Create a vector of the possible keys with a known value to ignore
    check = -1*ones(1,nKeys);
    % Loop through the possible set of keys
    for nk = 1:nKeys
        % If the index is not empty
        if ( isempty(allKeys{nk}) == 0 )
            % Determine which of the keys match with the input
            check(nk) = sum((allKeys{nk} == string(ni) )-1);
        end;
    end;
    keyNumbers(ni) = indexVal(check > -1);
end;

end

