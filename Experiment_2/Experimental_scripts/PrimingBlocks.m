%--------------------------------------------%
% Script to run main blocks of gabor priming %
% task                                       %
%--------------------------------------------%

% Set block type
blckType = 2;

% Matrix to record results
% 1) RT, 2) Choice, 3) Accuracy 4) Target,
% 5) Prime 6) Prime duration 7) Contrast 8) Block
% 9) Rotation angle 10 - 13) Stimulus timing 14) Type
results = zeros( nBlockTrials, 14);

% Randomly shuffle order
ord = randperm( size( primeCnd, 1 ) );

% Loop through trials
for trl = 1:sum(nTrials)
    
    % Set contrast difference
    cntrstLure = .5*cntrstPrp;
    
    % Create noise patch
    noiseWin = Noise_function(imSize,gaussWinNoise,0,1);
    noiseWinTex = Screen('MakeTexture', window, noiseWin);
    
    % Set up variables for each experimental trial
    primeTimeCur = primeCnd( ord(trl), 1 );
    fixTime = totalTime - primeTimeCur - targetTime - ...
        postMaskTime;
    FT = [ fixTime, primeTimeCur, targetTime, postMaskTime ];
    
    % Randomly select what the prime and target will be
    currentPrime = primeCnd( ord(trl), 3 );
    currentTarget = primeCnd( ord(trl), 2 );
    
    if ( currentTarget == 1 )
        cntrst = .5 + cntrstLure;
    else
        cntrst = .5 - cntrstLure;
    end
    
    % Create the gabor patch
    rot = datasample( rotRange, 1 );
    gabor = Overlaid_Gabors(imSize,stripes,amp,cntrst,rot,gaussWin,0,1,stepYes);
    gaborTex = Screen('MakeTexture', window, gabor);
    
    PrimeYes = 1;
    % Run script
    Gabor_priming_trial
    
    % Save responses
    Accuracy = currentTarget == resp;
    results(trl,:) = [ RT resp Accuracy currentTarget currentPrime ...
        primeTimeCur cntrstPrp blck rot time_check blckType ];
    
    % Color the screen grey
    Screen('FillRect', window, [ .5 .5 .5 ]);
    
    if yesFeedback==1
        % Report feedback
        if ( Accuracy == 1 )
            DrawFormattedText(window, 'Correct!', 'center','center', [ 0 0 0 ] );
        else
            DrawFormattedText(window, 'Wrong!', 'center','center', [ 0 0 0 ] );
        end
    end
    
    % Flip to the screen
    Screen('Flip', window);
    WaitSecs(.5);
    
end

% Store practice results
allResults = [ allResults; results ];

% Provide feeback at the end of the block
Block_feedback