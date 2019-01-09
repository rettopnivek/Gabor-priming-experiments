%-------------------------------------------%
% Script to generate a single priming trial %
%-------------------------------------------%

% Scripts for stimuli/choice located in 'Stimuli' subdirectory

% Structure of trial
% 1) Fixation
% 2) Placeholder
% 3) Prime
% 4) Target + prime mask
% 5) Full mask
% 6) Choice (record response)
% 7) Feedback

% Create index to keep track of what is shown on each frame
tmp = ifi*1000; % Number of ms a frame is displayed
frameVector = [ ...
    ms_to_frames(FT(1),tmp) ... % Fixation
    ms_to_frames(FT(2),tmp) ... % Placeholder
    ms_to_frames(FT(3),tmp) ... % Prime
    ms_to_frames(FT(4),tmp) ... % Target + prime mask
    ms_to_frames(FT(5),tmp) ... % Full mask
    ];
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
    
    % Full mask
    if ( frame > frame_index(4) && frame < frame_index(5) )
        MaskDisplay
    end
    
    % Flip to the screen
    vbl = Screen('Flip', window, vbl + (waitframes - 0.5) * ifi);
    
    % Check timing of stimuli
    for chk = 1:length(frame_index)
        if frame == frame_index(chk)
            time_check(chk) = GetSecs - sec0;
        end
    end
    
end

% Choice (record response)
ChoiceDisplay

% Feedback
Accuracy = resp == correctAnswer;
FeedbackDisplay

% Close all textures
Screen('Close');

% Clear event buffer
FlushEvents;