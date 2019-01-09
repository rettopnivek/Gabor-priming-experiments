%----------------%
% Key assignment %
% Page 3         %
%----------------%

%%% Draw stripes that are the same as the onscreen choices %%%

% Color the screen grey
Background

% Set options
onscreenChoices = 1;
thetaT = -45;
targetRotation = 0;

% Draw the onscreen choices
SameDifferentChoices

% Choice options
string = [ '     Same?', '          ', 'Different?' ];
DrawFormattedText(window, string, ...
    'center',vChoicePos, color_black );

% Draw a gabor patch
hShift = 90;
Screen('DrawTextures', window, gaborTexEasy, [], posLeft, ...
    [], [], [], []);

string = [ ...
    'As practice, compare the set of darker stripes in the \n' ...
    'grid on the left to the two center sets of stripes. \n' ...
    'Press "j" if they are the same or "k" if they differ.' ];
[nx,ny,bbox] = DrawFormattedText( window, string, ...
    'center',center(2)-imSize/2-250, [], [], [], [], lnSpace, [], [] );

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
            
            %%% Draw stripes that are the same as the onscreen choices %%%
            
            % Color the screen grey
            Background
            
            % Draw the onscreen choices
            SameDifferentChoices
            
            % Choice options
            string = [ '     Same?', '          ', 'Different?' ];
            DrawFormattedText(window, string, ...
                'center',vChoicePos, color_black );
            
            % Draw a gabor patch
            hShift = 90;
            Screen('DrawTextures', window, gaborTexEasy, [], posLeft, ...
                [], [], [], []);
            
            string = [ ...
                'Whoops! Try again.' ];
            [nx,ny,bbox] = DrawFormattedText( window, string, ...
                'center', center(2)-imSize/2-250, ...
                [], [], [], [], lnSpace, [], [] );
            
            % Flip to the screen (i.e. display stimuli)
            Screen('Flip', window);
            
        end
    end
else
    WaitSecs( InstructionTime );
end

%%% Draw stripes that are differ from the onscreen choices %%%

% Color the screen grey
Background

% Set options
onscreenChoices = 2;
thetaT = -45;
targetRotation = 0;

% Draw the onscreen choices
SameDifferentChoices

% Choice options
string = [ '     Same?', '          ', 'Different?' ];
DrawFormattedText(window, string, ...
    'center',vChoicePos, color_black );

% Draw a gabor patch
hShift = 90;
Screen('DrawTextures', window, gaborTexEasy, [], posLeft, ...
    [], [], [], []);

string = [ ...
    'Once again, compare the set of darker stripes in the \n' ...
    'grid on the left to the two center sets of stripes. \n' ...
    'Press "j" if they are the same or "k" if they differ.' ];
[nx,ny,bbox] = DrawFormattedText( window, string, ...
    'center',center(2)-imSize/2-250, [], [], [], [], lnSpace, [], [] );

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
            
            %%% Draw stripes that are differ from the onscreen choices %%%
            
            % Color the screen grey
            Background
            
            % Set options
            onscreenChoices = 2;
            thetaT = 45;
            targetRotation = 1;
            
            % Draw the onscreen choices
            SameDifferentChoices
            
            % Choice options
            string = [ '     Same?', '          ', 'Different?' ];
            DrawFormattedText(window, string, ...
                'center',vChoicePos, color_black );
            
            % Draw a gabor patch
            hShift = 90;
            Screen('DrawTextures', window, gaborTexEasy, [], posLeft, ...
                [], [], [], []);
            
            string = [ ...
                'Whoops! Try again.' ];
            [nx,ny,bbox] = DrawFormattedText( window, string, ...
                'center', center(2)-imSize/2-250, ...
                [], [], [], [], lnSpace, [], [] );
            
            % Flip to the screen (i.e. display stimuli)
            Screen('Flip', window);
            
        end
    end
else
    WaitSecs( InstructionTime );
end

