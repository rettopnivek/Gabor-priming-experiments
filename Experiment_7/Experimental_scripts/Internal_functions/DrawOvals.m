%---------------------------------------------------%
% Script to create priming ovals and choice options %
%---------------------------------------------------%

% Determine the size of the rectangle containing the oval
ovalWidth3 = round( (ovalRadius2 - ovalRadius1)/2 );
ovalWidth4 = round( ((ovalRadius2 - ovalRadius1)/2)*RadiusPrp );
ovalRadius3 = ovalRadius1 + ovalWidth3; % Determine the radius

% Set starting angle
startAng = rot + 180;

%%% Draw the mask for the prime %%%
% Define an set of equally spaced angles in degrees
ang = linspace( 5, 360, nPoints );

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

