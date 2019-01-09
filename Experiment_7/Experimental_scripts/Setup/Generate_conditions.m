%------------------------------%
% Generate balanced conditions %
%------------------------------%

% Make sure that these variables 
% are already defined:
% foilContrasts ( 0 - 1 )
% targetRatios ( 0 onward )
% primeTimes ( 0 - 267 )
% primeTypes ( 1 = Neither, 2 = Foil, 3 = Target )
% targetRotations ( 0 = Left, 1 = right )
% allOnscreenChoices ( 1 = Same, 2 = Different, 3 = None )

%%% Generate conditions %%%

% Column  1: Foil contrasts
% Column  2: Target/foil ratios
% Column  3: Target contrasts
% Column  4: Prime durations
% Column  5: Prime type
% Column  6: Prime rotation ( 0 = left, 1 = right )
% Column  7: Target rotation ( 0 = left, 1 = right )
% Column  8: Choice options shown onscreen 
%            ( 1 = Same, 2 = Different, 3 = None )
% Column  9: Correct answer
% Column 10: Row index for robot parameters

% Determine the number of levels for each variable
nLevels = [ ...
    size( foilContrasts, 2 ) ...
    size( targetRatios, 2 ) ...
	size( primeTimes, 2 ) ...
	size( primeTypes, 2 ) ...
    size( targetRotations, 2 ) ...
    size( allOnscreenChoices, 2 ) ];

% Total number of conditions
TotalCnd = prod( nLevels );

% Determine combinations of foil contrasts, target/foil ratios, and
% the corresponding target contrasts
smallCnd = zeros(TotalCnd,10);
inc = 1;
for c1 = 1:nLevels(1) % Foil contrast
    for c2 = 1:nLevels(2) % Target ratios
        for c4 = 1:nLevels(3) % Durations
            for c5 = 1:nLevels(4) % Prime types
                for c7 = 1:nLevels(5) % Target rotation
                    for c8 = 1:nLevels(6) % Onscreen alternatives
                        
                        % Set values for each variable of interest
                        smallCnd(inc,1) = foilContrasts( c1 );
                        smallCnd(inc,2) = targetRatios( c2 );
                        smallCnd(inc,4) = primeTimes( c4 );
                        smallCnd(inc,5) = primeTypes( c5 );
                        smallCnd(inc,7) = targetRotations( c7 );
                        smallCnd(inc,8) = allOnscreenChoices( c8 );
                        
                        inc = inc + 1;
                    end
                end
            end
        end
    end
end

% Fill in additional columns

% Target contrasts
smallCnd(:,3) = smallCnd(:,1) .* smallCnd(:,2);

% Correct answer
smallCnd(:,9) = smallCnd(:,7);

% Prime rotations
smallCnd(:,6) = smallCnd(:,7);
% Flip rotations for foil primed conditions
sel = smallCnd(:,5) == 2;
smallCnd( sel, 6 ) = 1 - smallCnd( sel, 6 );
% Set to 2 for neither-primed
sel = smallCnd(:,5) == 1;
smallCnd( sel, 6 ) = 2;

% Fill in index for robot parameters
smallCnd(:,10) = 1; % Chance performance

% Repeat each combination of conditions by the desired number of trials
AllCnd = repmat( smallCnd, nTrials, 1 );

% Clean up workspace
clear smallCnd;
