%------------------------------%
% Generate balanced conditions %
%------------------------------%

%%% Generate conditions %%%

% Column 1: Foil contrasts
% Column 2: Target/foil ratios
% Column 3: Target contrasts
% Column 4: Prime durations
% Column 5: Prime type
% Column 6: Correct answer

% Determine the number of levels for each variable
nLevels = [ size( foilContrasts,2) ...
    size( targetRatios, 2 ) ...
	size( primeTimes, 2 ) ...
	size( primeTypes, 2 ) ...
	size( correctAnswers, 2 ) ];

% Total number of conditions
TotalCnd = prod( nLevels );

% Determine combinations of foil contrasts, target/foil ratios, and
% the corresponding target contrasts
smallCnd = zeros(TotalCnd,6);
inc = 1;
for c1 = 1:nLevels(1)
	for c2 = 1:nLevels(2)
		for c3 = 1:nLevels(3)
			for c4 = 1:nLevels(4)
				for c5 = 1:nLevels(5)
					
					% Set values for each variable of interest
					smallCnd(inc,1) = foilContrasts( c1 );
					smallCnd(inc,2) = targetRatios( c2 );
					smallCnd(inc,4) = primeTimes( c3 );
					smallCnd(inc,5) = primeTypes( c4 );
					smallCnd(inc,6) = correctAnswers( c5 );
					
					inc = inc + 1;
                end
            end
        end
    end
end
smallCnd(:,3) = smallCnd(:,1) .* smallCnd(:,2);

% Repeat each combination of conditions by the desired number of trials
AllCnd = repmat( smallCnd, nTrials, 1 );

% Clean up workspace
clear smallCnd;
