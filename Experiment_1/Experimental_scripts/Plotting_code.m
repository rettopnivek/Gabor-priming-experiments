%---------------------------%
% Examine staircase results %
%---------------------------%

% Extract relevant trials
results = allResults( 1:(sum(nTrials)),:);

% Calculate sequential average
nT = 1:sum( nTrials );
aT = results(:,3);
aT = cumsum( aT );
avgA = zeros(1,max(nT));
for i = nT
    avgA(i) = aT(i)/nT(i);
end

plot( nT, avgA );

%{
% Confidence interval around criterion (lower interval)
p1 = [ 1 CI(1) ]; p2 = [ max(nT), CI(1)];
theta = atan2( p2(2) - p1(2), p2(1) - p1(1));
r = sqrt( (p2(1) - p1(1))^2 + (p2(2) - p1(2))^2);

line = 0:0.01: r;
x = p1(1) + line*cos(theta);
y = p1(2) + line*sin(theta);

hold on
plot(x, y)

% Confidence interval around criterion (upper interval)
p1 = [ 1 CI(2) ]; p2 = [ max(nT), CI(2)];
theta = atan2( p2(2) - p1(2), p2(1) - p1(1));
r = sqrt( (p2(1) - p1(1))^2 + (p2(2) - p1(2))^2);

line = 0:0.01: r;
x = p1(1) + line*cos(theta);
y = p1(2) + line*sin(theta);

hold on
plot(x, y)

p1 = [ 1 crt ]; p2 = [ max(nT), crt];
theta = atan2( p2(2) - p1(2), p2(1) - p1(1));
r = sqrt( (p2(1) - p1(1))^2 + (p2(2) - p1(2))^2);

line = 0:0.01: r;
x = p1(1) + line*cos(theta);
y = p1(2) + line*sin(theta);

hold on
plot(x, y)
%}

% Contrast difference proportion
hold on
%plot(nT, ( results(:,7)-min( steps) )/( max(steps) - min(steps) ),[ '-', '.', 'm' ])
plot(nT, results(:,7),[ '-', '.', 'm' ])

% Moving average
hold on
plot(nT, trkAcc,[ '-', '.', 'r' ]);

% Final contrast level
hold on
mCntrst = ones(1,length(nT))*mean( results(:,7) );
plot(nT, mCntrst,[ '-', '.', 'g' ]);
