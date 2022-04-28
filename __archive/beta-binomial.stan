//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] S;
  vector[N] I;
  vector[N] delta;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
  vector[I] p;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  vector[I] y;
  alpha ~ cauchy(0, 10);
  beta ~ cauchy(0, 10);
  
  for (k in 1:N) {
    p ~ beta(alpha, beta);
    for (i in 1:S) {
      for(j in 1:I) {
        y[j] ~ bernoulli(p[j]);
      }
      delta[k] += sum(y) > 0
    }
  }
}
