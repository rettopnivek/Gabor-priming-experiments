%--------------%
% Folder paths %
%--------------%

% Add path for setup scripts
orig_dir = cd('Setup');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

% Add path for additional functions
orig_dir = cd('Internal_functions');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

% Add path for trial scripts
orig_dir = cd('Stimuli');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

% Add path for task scripts
orig_dir = cd('Task_scripts');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

% Add path for instruction scripts
orig_dir = cd('Instructions');
pathToFunctions = cd;
addpath(pathToFunctions);
cd(orig_dir);

