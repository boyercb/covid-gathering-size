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
beta_proportion()
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  real<lower=0,upper=1> p_s;
  real<lower=0,upper=1> p_i;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0,upper=1> q;
}

model {
  q ~ beta(1,1);
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
generated quantities {
  int<lower=0,upper=3> y[N];
  simplex[3] theta;
  int<lower=0,upper=N> S;
  int<lower=0,upper=N> I;
  int<lower=0,upper=N> R;
  int<lower=0,upper=N> delta;

  theta = [p_s, p_i, 1 - p_s - p_i]';
  S = 0;
  I = 0;
  R = 0;
  
  for (i in 1:N) {
    y[i] = categorical_rng(theta);
    if (y[i] == 1)
      S += 1;
    else if (y[i] == 2)
      I += 1;
    else 
      R += 1;
  }

  {
    int k[S];
  
    for (i in 1:S) {
      k[i] = binomial_rng(I, q);
      k[i] = int_step(k[i]);
    }
  
    delta = sum(k);
  }

}

