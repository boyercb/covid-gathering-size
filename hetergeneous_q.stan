// This model simulates data based on our gathering model
data {
  int<lower=0> N;
  real<lower=0,upper=1> p_s;
  real<lower=0,upper=1> p_i;
}

parameters {
  
}

model {
  
}

generated quantities {
  int<lower=0,upper=3> y[N];
  simplex[3] theta;
  int<lower=0,upper=N> S;
  int<lower=0,upper=N> I;
  int<lower=0,upper=N> R;
  int<lower=0,upper=N> delta;

  S = 0;
  I = 0;
  R = 0;
  theta = [p_s, p_i, 1 - p_s - p_i]';

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
    int I_new[S];
    int k[S,I];
    real q[S,I];
    
    for (i in 1:S) {
      for (j in 1:I) {
        q[i,j] = beta_rng(1,1);
        k[i,j] = bernoulli_rng(q[i,j]);
      }
      I_new[i] = int_step(sum(k[i,:]));
    }
  
    delta = sum(I_new);
  }

}

