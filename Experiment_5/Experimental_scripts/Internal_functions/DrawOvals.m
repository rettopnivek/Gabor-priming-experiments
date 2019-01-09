%---------------------------------------------------%
% Script to create priming ovals and choice options %
%---------------------------------------------------%

% Determine the size of the rectangle containing the oval
ovalWidth3 = round( (ovalRadius2 - ovalRadius1)/2 );
ovalWidth4 = round( ((ovalRadius2 - ovalRadius1)/2)*RadiusPrp );
ovalRadius3 = ovalRadius1 + ovalWidth3; % Determine the radius

% Set starting angle
startAng = rot + 180;

%%% Draw the prime %%%
if PrimeYes == 1
    
    % If neither orientation is primed, ovals are placed at top/bottom
    if NeitherPrime == 1
        
        % Convert the angle to radians
        angRad = [ DegreesToRadians( 90 ) ...
            DegreesToRadians( 90+180 ) ];
        
        for i = 1:2
            % Define x and y values for center of point
            Yoval = sin(angRad(i))*ovalRadius3;
            Xoval = cos(angRad(i))*ovalRadius3;
            
            ovalPoint = [ center(1) + Xoval - ovalWidth4, ...
                center(2) + Yoval - ovalWidth4, ...
                center(1) + Xoval + ovalWidth4, ...
                center(2) + Yoval + ovalWidth4 ];
            
            % Draw oval on screen
            Screen('FillOval', window, [ 0 0 0 ], ovalPoint );
            
        end
        
        % Otherwise, draw ovals at matching orientations to target or foil
    else
        % Determine whether prime is on the left/right
        if currentPrime == 0
            angRad = [ DegreesToRadians(-180 + (90-rot) ) ...
                DegreesToRadians((90-rot)) ];
        else
            angRad = [ DegreesToRadians(-90 + (90 - rot) ) ...
                DegreesToRadians(-180 - rot) ];
        end
        
        for i = 1:2
            % Define x and y values for center of point
            Yoval = sin(angRad(i))*ovalRadius3;
            Xoval = cos(angRad(i))*ovalRadius3;
            
            ovalPoint = [ center(1) + Xoval - ovalWidth4, ...
                center(2) + Yoval - ovalWidth4, ...
                center(1) + Xoval + ovalWidth4, ...
                center(2) + Yoval + ovalWidth4 ];
            
            % Draw oval on screen
            Screen('FillOval', window, [ 0 0 0 ], ovalPoint );
            
        end
    end
end

%%% Draw the mask for the prime %%%

if PrimeYes == 2
    
    % Options for prime mask:
    % 1) A text based mask
    % 2) A moving dots mask
    
    % Define an set of equally spaced angles in degrees
    ang = linspace( 5, 360, nPoints );
    
    % Text based mask
    if primeMask == 1
        
        % Screen('TextSize',window,TxtSz*.65); % Shrink text size
        % Screen('TextStyle', window, 1 ); % Set to bold
        
        % Loop through random angles
        for i = 1:(nPoints-1)
            
            string = datasample( maskString, 1 );
            dim = Screen( window, 'TextBounds', char(string) );
            
            % Convert the angle to radians
            angRad = DegreesToRadians( ang(i) );
            
            % Define x and y values for center of point
            Yoval = sin(angRad)*(ovalRadius2 - 8);
            Xoval = cos(angRad)*(ovalRadius2 - 8);
            
            strXPos = center(1) + Xoval - dim(3)/2;
            strYPos = center(2) + Yoval - ( dim(4)/2 )*.9;
            
            % Draw mask using the string
            Screen('DrawText',window, string, strXPos, strYPos, [ 0 0 0 ] );
            
        end
        
        % Restore original text setting
        % Screen('TextSize',window,TxtSz);
        % Screen('TextStyle', window, 0 );
    end
    
    if primeMask == 2
        
        % Moving dots mask
        
        % Randomly shift angle every trial
        shft = unifrnd(-.15,.15);
        
        % Loop through random angles
        for i = 1:(nPoints-1)
            
            % Convert angle to radians
            angRad = DegreesToRadians( ang(i)+shft );
            
            % Define x and y values for center of point
            Yoval = sin(angRad)*ovalRadius3;
            Xoval = cos(angRad)*ovalRadius3;
            
            ovalPoint = [ center(1) + Xoval - ovalWidth4, ...
                center(2) + Yoval - ovalWidth4, ...
                center(1) + Xoval + ovalWidth4, ...
                center(2) + Yoval + ovalWidth4 ];
            
            % Draw oval on screen
            Screen('FillOval', window, [ 0 0 0 ], ovalPoint );
            
        end
        
    end
    
end
