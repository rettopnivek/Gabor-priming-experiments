%-------------------------------------------------%
% Script to run an initial practice block for the %
% gabor priming task                              %
%------------------------------------------------ %

% Define current block type
blckType = 0;

% Matrix to record results
% 1) RT, 2) Choice, 3) Accuracy 4) Target, 
% 5) Prime 6) Prime duration 7) Contrast 8) Block
% 9) Rotation angle 10 - 13) Stimulus timing 14) Type
results = zeros( nPBtask, 14);

% Shuffle order of target/prime presentations
practiceOrd = [];
for i = 1:stepSetPractice
    practiceOrd = [ practiceOrd, randperm( 4*trialSetPractice ) + ...
        (i-1)*4*trialSetPractice ];
end;

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Introduction to first practice block
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
instruct{1} = 'Practice block';
instruct{2} = 'You will now see a sequence of trials that get more ';
instruct{3} = 'difficult over time.';
instruct{4} = ' ';
instruct{5} = 'Press any key to begin';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

loop = nPBtask;
for trl = 1:loop
    
    % Set contrast proportion
    cntrstPrp = practiceCnd( trl, 1 ); % No randomization
    
    % Set contrast difference
    cntrstLure = .5*cntrstPrp;
    
    % Create noise patch
    noiseWin = Noise_function(imSize,gaussWinNoise,0,1);
    noiseWinTex = Screen('MakeTexture', window, noiseWin);
    
    % Set up variables for each experimental trial
    primeTimeCur = practiceCnd( practiceOrd(trl), 2 );
    fixTime = totalTime - primeTimeCur - targetTime - ...
        postMaskTime;
    FT = [ fixTime, primeTimeCur, targetTime, postMaskTime ];
    
    % Randomly select what the prime and target will be
    currentPrime = practiceCnd( practiceOrd(trl), 3 );
    currentTarget = practiceCnd( practiceOrd(trl), 4 );
    
    if ( currentTarget == 1 )
        cntrst = .5 + cntrstLure;
    else
        cntrst = .5 - cntrstLure;
    end
    
    % Create the gabor patch
    rot = datasample( rotRange, 1 );
    gabor = Overlaid_Gabors(imSize,stripes,amp,cntrst,rot,gaussWin,0,1,stepYes);
    gaborTex = Screen('MakeTexture', window, gabor);
    
    PrimeYes = 0;
    % Run script
    Gabor_priming_trial
    
    % Save responses
    Accuracy = currentTarget == resp;
    y = Accuracy;
    results(trl,:) = [ RT resp Accuracy currentTarget currentPrime ...
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
