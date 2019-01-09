%-----------------------%
% Prime/Target creation %
%-----------------------%

% Variables that need to be defined:
% currentTarget
% currentFoil
% currentType

% Based on target value, set rotation angle
rot = datasample( theta, 1 );
if targetRotation == 1
    thetaT = rot; % Rotated right
else
    thetaT = rot - 90; % Rotated left
end

% Create the target gabor patch
gabor = Overlaid_Gabors(imSize,PP,currentTarget,currentFoil,thetaT,stripesTarget,stimWin,0);

% Create possible prime
if currentType == 1
    % Neither primed
    gaborPrime = Overlaid_Gabors(imSize,PP,1,0,90,stripes,choiceR,0);
elseif currentType == 3
    % Target primed
    gaborPrime = Overlaid_Gabors(imSize,PP,1,0,thetaT,stripes,choiceR,0);
elseif currentType == 2
    % Foil primed
    if targetRotation == 1
        % Target is rotated right, so prime rotated left
        gaborPrime = Overlaid_Gabors(imSize,PP,1,0,rot-90,stripes,choiceR,0);
    else
        % Target is rotated left, so prime rotated right
        gaborPrime = Overlaid_Gabors(imSize,PP,1,0,rot,stripes,choiceR,0);
    end
end
static_display = 1;