%---------------------------------------------------%
% Script to create priming ovals and choice options %
%---------------------------------------------------%

% Determine the size of the rectangle containing the oval
ovalWidth3 = round( (ovalRadius2 - ovalRadius1)/2 );
ovalWidth4 = round( ((ovalRadius2 - ovalRadius1)/2)*RadiusPrp );
ovalRadius3 = ovalRadius1 + ovalWidth3; % Determine the radius

startAng = rot + 180;

if PrimeYes == 1
    
    % Determine whether prime is on the left/right
    if currentPrime == 0
        ornt = 1;
    else
        ornt = 2;
    end
    
    % Convert the angle to radians
    angRad = [ DegreesToRadians( startAng + 90*(ornt-1) ) ...
        DegreesToRadians( startAng + 180 + 90*(ornt-1) ) ];

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
if RandomYes == 1
    
    % Loop through random angles
    for i = 1:nPoints
        
        string = '*';
        dim = Screen( window, 'TextBounds', char(string) );
        
        % Convert the angle to radians
        angRad = DegreesToRadians( ang(i) );
        
        % Define x and y values for center of point
        Yoval = sin(angRad)*(ovalRadius2 - 8);
        Xoval = cos(angRad)*(ovalRadius2 - 8);
        
        strXPos = center(1) + Xoval - dim(3)/2;
        strYPos = center(2) + Yoval - ( dim(4)/2 )*.75;
           
        % Draw mask using the string
        Screen('DrawText',window, string, strXPos, strYPos, [ 0 0 0 ] );
    end
    
end
if ChoiceYes == 1
    
    for j = 1:2
       % Convert the angle to radians
       angRad = [ DegreesToRadians( startAng + 90*(j-1) ) ...
           DegreesToRadians( startAng + 180 + 90*(j-1) ) ];
        
       string = keys(j);
       dim = Screen( window, 'TextBounds', char(string) );
       
       % i = 1;
       for i = 1:2
           % Define x and y values for center of point
           Yoval = sin(angRad(i))*(ovalRadius2 + 15);
           Xoval = cos(angRad(i))*(ovalRadius2 + 15);
           
           strXPos = center(1) + Xoval - dim(3)/2;
           strYPos = center(2) + Yoval - dim(4)/2;
           
           % Draw key choices to string
           DrawFormattedText(window, string, strXPos, strYPos, [ 0 0 0 ] );
           
       end
    
    end
    
end
