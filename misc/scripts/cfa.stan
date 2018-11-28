data {
  real g_alpha;
  real g_beta;
  int<lower = 0> N;
  int<lower = 0> Ni;
  int<lower = 0> Np;
  int<lower = 0> Nf;
  int<lower = 1, upper = Ni> items[N];
  int<lower = 1, upper = Np> persons[N];
  int<lower = 1, upper = Nf> factors[N];
  vector[N] response;
}
parameters {
  vector<lower = 0>[Ni] item_vars; // item vars heteroskedastic
  real<lower = 0> sigma_alpha; // sd of loadings, hyperparm
  vector<lower = 0>[Ni] alphas; // loadings
  vector[Ni] betas; // item intercepts, default uniform prior
  vector[Nf] thetas[Np]; // person scores for each factor
  cholesky_factor_corr[Nf] L; // Cholesky decomp of corr mat of random slopes
}
transformed parameters {
  vector[N] yhat;
  vector[N] item_sds_i;

  for (i in 1:N) {
    yhat[i] = alphas[items[i]] * thetas[persons[i], factors[i]] + betas[items[i]];
    item_sds_i[i] = sqrt(item_vars[items[i]]);
  }
}
model {
  vector[Nf] A = rep_vector(1, Nf); // Vector of random slope variances
  matrix[Nf, Nf] A0;

  // A = diag_matrix(rep_vector(1, Nf));
  L ~ lkj_corr_cholesky(Nf);
  A0 = diag_pre_multiply(A, L);
  thetas ~ multi_normal_cholesky(rep_vector(0, Nf), A0);

  alphas ~ lognormal(0, sigma_alpha);
  sigma_alpha ~ cauchy(0, 2.5); // hyperparm
  item_vars ~ inv_gamma(g_alpha, g_beta);

  response ~ normal(yhat, item_sds_i);
}
generated quantities {
  vector<lower = 0>[Ni] loadings_std; // obtain loadings_std
  matrix[Nf, Nf] R;

  R = tcrossprod(L);
  for (i in 1:Ni) {
    loadings_std[i] = alphas[i] / sqrt(square(alphas[i]) + item_vars[i]);
  }
}
