%----------------%
% Key assignment %
% Page 3         %
%----------------%

%%% Draw stripes rotated to the left %%%

% Color the screen grey
Background

% Draw a gabor patch
hShift = 90;
Screen('DrawTextures', window, gaborTexLeft, [], posCenter, ...
    [], [], [], []);

string = [ ...
    'As practice, press the matching key to the set \n' ...
    'of stripes currently shown.' ];
[nx,ny,bbox] = DrawFormattedText( window, string, ...
    'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

if debug == 0
    
    % Loop until subject presses correct key
    check = 0;
    inc = 0;
    while check ~= keyOptions(1)
        
        [ ~, tmp ] = KbPressWait;
        tmp2 = 1:256;
        check = tmp2( tmp == 1 );
        
        inc = inc + 1;
        if inc > 0
            
            %%% Draw stripes rotated to the left %%%
            
            % Color the screen grey
            Background
            
            % Draw a gabor patch
            hShift = 90;
            Screen('DrawTextures', window, gaborTexLeft, [], posCenter, ...
                [], [], [], []);
            
            string = [ ...
                'Whoops! Try again.' ];
            [nx,ny,bbox] = DrawFormattedText( window, string, ...
                'center', center(2)-imSize/2-150, ...
                [], [], [], [], lnSpace, [], [] );
            
            % Flip to the screen (i.e. display stimuli)
            Screen('Flip', window);
            
        end
    end
else
    WaitSecs( InstructionTime );
end

%%% Draw stripes rotated to the right %%%

% Color the screen grey
Background

% Draw a gabor patch
hShift = 90;
Screen('DrawTextures', window, gaborTexRight, [], posCenter, ...
    [], [], [], []);

string = [ ...
    'As practice, press the matching key to the set \n' ...
    'of stripes currently shown.' ];
[nx,ny,bbox] = DrawFormattedText( window, string, ...
    'center',center(2)-imSize/2-150, [], [], [], [], lnSpace, [], [] );

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

if debug == 0
    
    % Loop until subject presses correct key
    check = 0;
    inc = 0;
    while check ~= keyOptions(2)
        
        [ ~, tmp ] = KbPressWait;
        tmp2 = 1:256;
        check = tmp2( tmp == 1 );
        
        inc = inc + 1;
        if inc > 0
            
            %%% Draw stripes rotated to the right %%%
            
            % Color the screen grey
            Background
            
            % Draw a gabor patch
            hShift = 90;
            Screen('DrawTextures', window, gaborTexRight, [], posCenter, ...
                [], [], [], []);
            
            string = [ ...
                'Whoops! Try again.' ];
            [nx,ny,bbox] = DrawFormattedText( window, string, ...
                'center', center(2)-imSize/2-150, ...
                [], [], [], [], lnSpace, [], [] );
            
            % Flip to the screen (i.e. display stimuli)
            Screen('Flip', window);
            
        end
    end
else
    WaitSecs( InstructionTime );
end
