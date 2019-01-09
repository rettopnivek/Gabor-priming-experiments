%-----------------------------------------------%
% Stair-case method to determine contrast level %
% that most closely produces a desired level of %
% accuracy                                      %
%-----------------------------------------------%

%%% Set up conditions %%%

% Prime duration: Fixed Fixed Fixed Fixed
% Target:         Left  Left  Right Right
% Prime:          Left  Right Left  Right

% The length of the prime (though it's not shown) for evaluating the
% psychometric function
practicePrimeTime = 150;

% Number of trials in a condition
trialSet = 20;
nTrials = ones( 1, 4 )*trialSet;

durCnd = ones(1,trialSet*4)*practicePrimeTime;

tarCnd = [ zeros(1,trialSet*2) ones(1,trialSet*2) ];
priCnd = [ zeros(1,trialSet) ones(1,trialSet) ];
priCnd = [ priCnd priCnd ];

% Randomly shuffle order
ord = randperm( sum(nTrials) );

% Define current block
blck = 1;

%%% Variables for stair-case method %%%

% Create steps
winSize = 10;
[ steps ] = exp( linspace( log(.01), log(1), winSize*2 ) );
inc = round(winSize);
cntrstPrp = steps(inc); % Initialize contrast difference

% Create moving window
mvAvgLength = winSize;
mvAvgWindow = zeros(1,mvAvgLength);
% Define criterion
crt = .65;
% Calculate 95% credible interval around criterion using beta-binomial
% model
A = 1 + sum(nTrials)*crt;
B = 1 + sum(nTrials) - sum(nTrials)*crt;
prb = [ .1, .9 ];
pd = makedist('Beta',A,B);
CI = icdf(pd,prb); % Calculate the credible interval

% Matrix to record results
% 1) RT, 2) Choice, 3) Accuracy 4) Target, 
% 5) Prime 6) Prime duration 7) Contrast 8) Block
% 9) Rotation angle 10 - 13) Stimulus timing
results = zeros( sum(nTrials), 13);

% Initialize variables
avgAccuracy = 0;
trkAcc = zeros(1,sum(nTrials));
mvAvg = 1;

% Color the screen grey
Screen('FillRect', window, [ .5 .5 .5 ]);

% Introduction to second practice block
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
instruct{1} = '2nd practice block';
instruct{2} = 'Note the darkness of the stripes will now vary.';
instruct{3} = ' ';
instruct{4} = ' ';
instruct{5} = 'Press any key to begin';
lenIns = length(instruct);

% Loop through trials
for trl = 1:sum(nTrials)
    
    % Set contrast difference
    cntrstLure = .5*cntrstPrp;
    
    % Create noise patch
    noiseWin = Noise_function(imSize,gaussWinNoise,0,1);
    noiseWinTex = Screen('MakeTexture', window, noiseWin);
    
    % Set up variables for each experimental trial
    primeTimeCur = durCnd( ord(trl) );
    fixTime = totalTime - primeTimeCur - targetTime - ...
        postMaskTime;
    FT = [ fixTime, primeTimeCur, targetTime, postMaskTime ];
    
    % Randomly select what the prime and target will be
    currentPrime = priCnd( ord(trl) );
    currentTarget = tarCnd( ord(trl) );
    
    if ( currentTarget == 1 )
        cntrst = .5 + cntrstLure;
    else
        cntrst = .5 - cntrstLure;
    end
    
    % Create the gabor patch
    rot = rand*80;
    gabor = Overlaid_Gabors(imSize,stripes,1,cntrst,rot,gaussWin,0,1);
    gaborTex = Screen('MakeTexture', window, gabor);
    
    PrimeYes = 0;
    % Run script
    Gabor_priming_trial
    
    % Save responses
    Accuracy = currentTarget == resp;
    results(trl,:) = [ RT resp Accuracy currentTarget currentPrime ...
        primeTimeCur cntrstPrp blck rot time_check ];
    
    % Determine accuracy within moving window
    mvAvgWindow( mvAvg ) = Accuracy;
    mvAvg = min( mvAvgLength, mvAvg + 1 );
    avgAccuracy = mean( mvAvgWindow );
    if mvAvg == mvAvgLength
        mvAvgWindow(1:(mvAvg-1)) = mvAvgWindow(2:mvAvg);
    end
    
    % If cumulative accuracy falls below the credible interval, make the
    % task easier
    if avgAccuracy < CI(1)
        inc = min( length(steps), inc + 1 );
        cntrstPrp = steps( inc );
    end
    % If cumulative accuracy falls above the credible interval, make the
    % task harder
    if avgAccuracy > CI(2)
        inc = max( 1, inc - 1 );
        cntrstPrp = steps( inc );
    end
    % Otherwise do nothing
    trkAcc(trl) = avgAccuracy; % Track moving average over trials
    
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

% Set contrast
cntrstPrp = mean( results(:,7) );