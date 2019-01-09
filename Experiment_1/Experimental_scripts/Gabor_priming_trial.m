%-------------------------------------------%
% Script to generate a single priming trial %
%-------------------------------------------%

% Define an set of equally spaced angles in degrees
ang = linspace( 5, 360, nPoints );
% ang = unifrnd(0,ones(1,nPoints)*360);

tmp = ifi*1000;
% Structure of a trial
% 1) Fixation
% 2) Clock prime + Fixation
% 3) Target + Clock noise
% 4) Post-target mask
frameVector = [ ms_to_frames(FT(1),tmp) ms_to_frames(FT(2),tmp) ...
                ms_to_frames(FT(3),tmp) ms_to_frames(FT(4),tmp) ];
numFrames = sum(frameVector);
frame_index = cumsum(frameVector);

% Loop through individual frames
sec0 = GetSecs; % Baseline timepoint
time_check = zeros(1, length( frame_index ) ); % Records stimulus presentation time
for frame = 1:numFrames
    
    % Fixation dot
    if ( frame <= frame_index(1) )
        
        % Color the screen grey
        Screen('FillRect', window, [ .5 .5 .5 ]);
        
        % Draw a fixation dot
        Screen('DrawDots', window, [center(1); center(2)], dotSizePix, dotColor, [], 0);
        
    end;
        
    % Presentation of annulus and clock prime
    if ( frame > frame_index(1) && frame <= frame_index(2) )
        
        % Color the screen grey
        Screen('FillRect', window, [ .5 .5 .5 ]);
        
        % Draw the annulus
        Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
        Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );
        
        % Draw the clock prime
        RandomYes = 0;
        ChoiceYes = 0;
        DrawOvals
        
    end
    
    % Target presentation
    if ( frame > frame_index(2) && frame <= frame_index(3) )
        
        % Color the screen grey
        Screen('FillRect', window, [ .5 .5 .5 ]);
        
        % Draw the overlaid gabor patch
          Screen('DrawTextures', window, gaborTex, [],...
            [], [], [], [], []);
        
        % Draw the annulus
        Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
        Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );
        
        % Mask the prime
        PrimeYes = 0;
        RandomYes = 1;
        ChoiceYes = 0;
        DrawOvals
        
    end;
    
    % Post-target mask
    if ( frame > frame_index(3) )
        
        % Color the screen grey
        Screen('FillRect', window, [ .5 .5 .5 ]);
        
        % Draw a noise patch
        Screen('DrawTextures', window, noiseWinTex, [],...
               [], [], [], [], []);
                
        % Draw the annulus
        Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
        Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );
        
        % Mask the prime
        PrimeYes = 0;
        RandomYes = 1;
        ChoiceYes = 0;
        DrawOvals
    end
    
    % Flip to the screen
    Screen('Flip', window);
    
    % Check timing of stimuli
    for chk = 1:length(frame_index)
        if frame == frame_index(chk)
            time_check(chk) = GetSecs - sec0;
        end
    end
    
end;

% Present choices for subject
% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Draw a noise patch
Screen('DrawTextures', window, noiseWinTex, [],...
    [], [], [], [], []);

% Draw the annulus
Screen('FrameOval', window, [ 0 0 0 ], ovalSize1 );
Screen('FrameOval', window, [ 0 0 0 ], ovalSize2 );

% Mask the prime
PrimeYes = 0;
RandomYes = 0;
ChoiceYes = 1;
DrawOvals

% Flip to the screen
Screen('Flip', window);

% Measure response time and choice
[ RT, resp ] = getResponseMultiple(keyOptions(1:2),0:1, timeout);
% Check if timeout response
if RT > timeout
    resp = 2;
    accuracy = 0;
end

% Clear event buffer
FlushEvents;