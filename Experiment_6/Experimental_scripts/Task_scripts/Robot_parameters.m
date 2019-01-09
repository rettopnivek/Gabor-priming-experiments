%------------------%
% Robot parameters %
%------------------%

robotParam = [ ...
      % 1) Chance
      % k1   x1     k0     x0    tau   pause
      0.80,  1.00,  0.80,  1.00  0.30  0; ...
      % 2) Highly accurate for picking choice = 1
      % k1   x1     k0     x0    tau   pause
      0.80,  4.00,  1.00,  0.01  0.30  0; ...
      % 3) Highly accurate for picking choice = 0
      % k1   x1     k0     x0    tau   pause
      1.00,  0.01,  0.80,  4.00  0.30  0; ...
      % 4) Delay for post instruction responses
      % RT   NA     NA     NA    NA    pause
      1.00,  0.00,  0.00,  0.00  0.00  0; ...
      % 5) Adjustable parameters for staircase approach ( choice = 1 )
      % k1   x1     k0     x0    tau   pause
      0.80,  3.00,  0.80,  1.00  0.30  0; ...
      % 5) Adjustable parameters for staircase approach ( choice = 0 )
      % k1   x1     k0     x0    tau   pause
      0.80,  1.00,  0.80,  3.00  0.30  0; ...
    ];

% Parameters for psychophysics aspect of task
robotAlphaBeta = [ 3, 2.9 ];

% Label rows for easy selection
robotOptions = { 'Chance', 'Pick_1', 'Pick_0', 'Wait', ...
                 'Pick_1_A', 'Pick_0_A' };
robotRows = 1:size( robotParam, 1 );

% Isolate row specific for specifying delay after instructions
robotWait = robotRows( strcmp( robotOptions, 'Wait' ) );