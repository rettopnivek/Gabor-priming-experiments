data {
  int No; // Number of observations
  int Nd; // Number of covariates for d'
  int Nk; // Number of covariates for kappa
  matrix[No,Nd] X_d; // Design matrix for d'
  matrix[No,Nk] X_k; // Design matrix for kappa
  int Y[No]; // Array for number of times subject picked right
  int Nt[No]; // The total number of trials per observation
  matrix[Nd+Nk,2] Priors; // Matrix of parameters for prior distributions
}
parameters {
  vector[Nd] beta_d;
  vector[Nk] beta_k;
}
model {
  // Variable declaration
  vector[No] dprime;
  vector[No] kappa;
  real theta[No];
  
  // Priors
  for (d in 1:Nd) beta_d[d] ~ normal( Priors[d,1], Priors[d,2] );
  for (k in 1:Nk) beta_k[k] ~ normal( Priors[Nd+k,1], Priors[Nd+k,2] );
  
  // Likelihood
  dprime <- X_d*beta_d;
  kappa <- X_k*beta_k;
  
  for (no in 1:No) {
    theta[no] <- 1.0 - normal_cdf(kappa[no],dprime[no],1.0);
  }
  
  Y ~ binomial(Nt,theta);
}
generated quantities {
  // Variable declaration
  vector[No] dprime;
  vector[No] kappa;
  real theta[No];
  vector[No] logLik;
  int sim1[No];
  int sim2[No];
  
  // Likelihood
  dprime <- X_d*beta_d;
  kappa <- X_k*beta_k;
  
  for (no in 1:No) {
    theta[no] <- 1.0 - normal_cdf(kappa[no],dprime[no],1.0);
    sim1[no] <- binomial_rng(Nt[no],theta[no]);
    sim2[no] <- binomial_rng(Nt[no],theta[no]);
    logLik[no] <- binomial_log(Y[no], Nt[no], theta[no]);
  }
  
}

