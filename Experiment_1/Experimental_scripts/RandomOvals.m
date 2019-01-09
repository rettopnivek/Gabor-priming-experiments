
% Define an set of angles in degrees
ang = unifrnd(0,ones(1,nPoints)*360);

% Determine the size of the rectangle containing the oval
ovalWidth3 = round( (ovalRadius2 - ovalRadius1)/2 );
ovalWidth4 = round( ((ovalRadius2 - ovalRadius1)/2)*.8 );
ovalRadius3 = ovalRadius1 + ovalWidth3; % Determine the radius

for i = 1:nPoints
    % Forthcoming
    
    % Convert the angle to radians
    angRad = DegreesToRadians( ang(i) );
    
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