data {
  int No; // Number of observations
  int Nk;  // Number of covariates for kappa
  matrix[No,Nk] X_k; // Design matrix for kappa
  int Y[No]; // Array for number of times subject picked right
  int Nt[No]; // The total number of trials per observation
  matrix[Nk,2] Priors; // Matrix of parameters for prior distributions
}
parameters {
  vector[Nk] beta_k;
}
model {
  // Variable declaration
  vector[No] kappa;
  real theta[No];
  
  // Priors
  for (k in 1:Nk) beta_k[k] ~ normal( Priors[k,1], Priors[k,2] );
  
  // Likelihood
  kappa <- X_k*beta_k;
  
  for (no in 1:No) {
    theta[no] <- 1.0 - normal_cdf(kappa[no],0.0,1.0);
  }
  
  Y ~ binomial(Nt,theta);
}
generated quantities {
  // Variable declaration
  vector[No] kappa;
  real theta[No];
  vector[No] logLik;
  int sim1[No];
  int sim2[No];
  
  // Likelihood
  kappa <- X_k*beta_k;
  
  for (no in 1:No) {
    theta[no] <- 1.0 - normal_cdf(kappa[no],0.0,1.0);
    sim1[no] <- binomial_rng(Nt[no],theta[no]);
    sim2[no] <- binomial_rng(Nt[no],theta[no]);
    logLik[no] <- binomial_log(Y[no], Nt[no], theta[no]);
  }
}

