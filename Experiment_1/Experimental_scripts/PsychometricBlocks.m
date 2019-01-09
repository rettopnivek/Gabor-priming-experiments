%------------------------------------------------%
% Script to run multiple blocks of gabor priming %
% for psychometric curve determination           %
%------------------------------------------------%

% Set block type
blckType = 2;

% Matrix to record results
% 1) RT, 2) Choice, 3) Accuracy 4) Target,
% 5) Prime 6) Prime duration 7) Contrast 8) Block
% 9) Rotation angle 10 - 13) Stimulus timing 14) Type
results = zeros( nBlockTrials, 14);

% Loop through trials
for curtrl = 1:nBlockTrials
    
    trl_pc = trl_pc + 1;
    
    % Set contrast difference
    cntrstPrp = psychoCnd( ordPsycho(trl_pc), 1 );
    cntrstLure = .5*cntrstPrp;
    
    % Create noise patch
    noiseWin = Noise_function(imSize,gaussWinNoise,0,1);
    noiseWinTex = Screen('MakeTexture', window, noiseWin);
    
    % Set up variables for each experimental trial
    primeTimeCur = psychoCnd( ordPsycho(trl_pc), 2 );
    fixTime = totalTime - primeTimeCur - targetTime - ...
        postMaskTime;
    FT = [ fixTime, primeTimeCur, targetTime, postMaskTime ];
    
    % Randomly select what the prime and target will be
    currentPrime = psychoCnd( ordPsycho(trl_pc), 4 );
    currentTarget = psychoCnd( ordPsycho(trl_pc), 3 );
    
    if ( currentTarget == 1 )
        cntrst = .5 + cntrstLure;
    else
        cntrst = .5 - cntrstLure;
    end
    
    % Create the gabor patch
    rot = datasample( rotRange, 1 );
    gabor = Overlaid_Gabors(imSize,stripes,1,cntrst,rot,gaussWin,0,1);
    gaborTex = Screen('MakeTexture', window, gabor);
    
    PrimeYes = 0;
    % Run script
    Gabor_priming_trial
    
    % Save responses
    Accuracy = currentTarget == resp;
    results(curtrl,:) = [ RT resp Accuracy currentTarget currentPrime ...
        primeTimeCur cntrstPrp blck rot time_check blckType ];
    
    % Report feedback
    if ( Accuracy == 1 )
        DrawFormattedText(window, 'Correct!', 'center','center', [ 0 0 0 ] );
    else
        DrawFormattedText(window, 'Wrong!', 'center','center', [ 0 0 0 ] );
    end
    
    % Flip to the screen
    Screen('Flip', window);
    WaitSecs(.5);
    
end

% Store practice results
allResults = [ allResults; results ];

% Provide feeback at the end of the block
Block_feedback
