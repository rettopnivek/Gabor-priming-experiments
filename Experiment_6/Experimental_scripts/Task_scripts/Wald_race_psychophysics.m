%--------------------------------------------------%
% Simulating a psychophysics-based Wald race model %
%--------------------------------------------------%

% Truncated two-parameter logistic function
theta_val =  1 / ...
    ( 1 + exp( -robotAlphaBeta(1) * ( x_cur + robotAlphaBeta(2) ) ) );

% Compute drift rates
newPar = [ ...
    5 * theta_val, ...
    5 * ( 1 - theta_val ) ...
    ];

% Adjust robot parameters
selVal = strcmp( robotOptions, 'Pick_1_A' );
robotParam( selVal, 2 ) = newPar(1);
robotParam( selVal, 4 ) = newPar(2);
selVal = strcmp( robotOptions, 'Pick_0_A' );
robotParam( selVal, 4 ) = newPar(1);
robotParam( selVal, 2 ) = newPar(2);