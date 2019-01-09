%-------------------------------------------%
% Script to generate a single priming trial %
%-------------------------------------------%

% Scripts for stimuli/choice located in 'Stimuli' subdirectory

% Structure of trial
% 1) Fixation
% 2) Placeholder
% 3) Prime
% 4) Target + prime mask
% 5) Target mask + prime mask
% 6) Choice (record response)

% Create index to keep track of what is shown on each frame
tmp = ifi*1000; % Number of ms a frame is displayed
frameVector = [ ms_to_frames(FT(1),tmp) ms_to_frames(FT(2),tmp) ...
    ms_to_frames(FT(3),tmp) ms_to_frames(FT(4),tmp) ...
    ms_to_frames(FT(5),tmp) ];
numFrames = sum(frameVector);
frame_index = cumsum(frameVector);

% Record presentation time of stimuli
sec0 = GetSecs; % Baseline timepoint
time_check = zeros(1, length( frame_index ) );

% Loop through individual frames
for frame = 1:numFrames
    
    % Fixation
    if ( frame <= frame_index(1) )
        FixationDisplay
    end;
    
    % Placeholder
    if ( frame > frame_index(1) && frame <= frame_index(2) )
        PlaceholderDisplay
    end;
    
    % Prime
    if ( frame > frame_index(2) && frame <= frame_index(3) )
        PrimeDisplay
    end
    
    % Target + prime mask
    if ( frame > frame_index(3) && frame <= frame_index(4) )
        TargetDisplay
    end
    
    % Target mask + prime mask
    if ( frame > frame_index(4) )
        MaskDisplay
    end
    
    % Flip to the screen
    Screen('Flip', window);
    
    % Check timing of stimuli
    for chk = 1:length(frame_index)
        if frame == frame_index(chk)
            time_check(chk) = GetSecs - sec0;
        end
    end
    
end

% Choice (record response)
ChoiceDisplay

% Close all textures
Screen('Close');

% Clear event buffer
FlushEvents;