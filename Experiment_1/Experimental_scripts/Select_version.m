%------------------------------------------------%
% Onscreen script to determine the type of study %
% to run                                         %
%------------------------------------------------%

%{
Notes:
This script should be run before creating a windows pointer for 
Psychtoolobox
%}

% Set initial variables
Typeconfirm = 0;
boxSize = [ 250, 100 ]; % Set the width and height of the dialog box
orig_dir = cd(); % Current working directory

while Typeconfirm == 0
    
    confirmChoices = 0;
    
    prompt = {'Please enter the version of study to run:'};
    dlg_title = 'Study version';
    num_lines = 1;
    studyType = inputdlg(prompt,dlg_title,num_lines);
    
    % If they choose cancel, exit Matlab
    if isempty(studyType)
        exit();
    end
        
    % Define the initial prompt
    stringPrompt = [ {sprintf('Is this the desired version?')} ...
                     {sprintf( [ 'Version ' studyType{1} ] )} ...
                     {''} ];
    
    % Convert the output to a number
    studyType = str2num(studyType{1});
    % Define the choices that can be selected
    Choices = [ {'Yes'} {'No'} ];

    % Create a list dialog box and determine the selection
    sel = listdlg('PromptString',stringPrompt,...
        'SelectionMode','single',... % So people can only pick one option
        'ListString',Choices,...
        'ListSize',boxSize,...
        'CancelString','Cancel');

    if isempty(sel)
        break
    elseif sel == 1
        confirmChoices = 1;
    end
    
    % Check if an appropriate version has been chosen
    lastCheck = 0;
    if studyType < 1 && studyType > 3
        lastCheck = 1;
    end
    
    if confirmChoices == 1
        if lastCheck == 0
            Typeconfirm = 1;
        else
            % Indicate that a different ID number must be used
            warning = sprintf('This is not an available study version.');
            waitfor( msgbox(warning,'Study version') ); % Must close message to continue
        end
    end

end

% If they choose cancel, exit Matlab
if isempty(sel)
    exit();
end

% Return to original directory
cd(orig_dir);

% Cleans up workspace
clear prompt dlg_title num_lines Typeconfirm boxSize ...
      confirmChoices stringPrompt sel Choices lastCheck;