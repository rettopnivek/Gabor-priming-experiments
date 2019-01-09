%----------%
% Feedback %
%----------%

% Color the screen grey
Background

if resp == 2
    % If a timeout response occurred
    DrawFormattedText(window, 'Too slow!', ...
        'center','center', color_black );
elseif Accuracy == 1
    % If a correct response was made
    DrawFormattedText(window, 'Correct!', ...
        'center','center', color_black );
else
    % If an incorrect response was made
    DrawFormattedText(window, 'Wrong!', ...
        'center','center', color_black );
end

% Flip to the screen
Screen('Flip', window);
WaitSecs(feedbackTime);
