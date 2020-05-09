library(Matrix)
library(reticulate)
python_path <- BayesBridgeR::guess_python_path()
reticulate::use_python(python_path, required = TRUE)

# Simulate sparse binary design matrix and binomial outcome
set.seed(0)
n_obs <- 1000
n_pred <- 100
pred_freq <- .2
n_signal <- 10

nnz_per_col <- as.integer(pred_freq * n_obs)
row_index <- unlist(lapply(1:n_pred, function(j) rep(j, nnz_per_col)))
col_index <- unlist(
  lapply(1:n_pred, function (i) sample.int(n_obs, size = nnz_per_col, replace = FALSE))
)
val <- rep(1., nnz_per_col * n_pred)
X <- sparseMatrix(i = col_index, j = row_index, x = val)

beta_true <- c(rep(1., n_signal), rep(0., n_pred - n_signal))
X_beta <- as.vector(X %*% beta_true)
n_trial <- rep(1, n_obs)
n_success <- rbinom(n_obs, n_trial, prob = 1 / (1 + exp(-X_beta)))


# Generate posterior samples via Python 'bayesbridge' package
X_py <- reticulate::r_to_py(X)
outcome <- list(
  reticulate::np_array(n_success),
  reticulate::np_array(n_trial)
)
bayesbridge <- reticulate::import('bayesbridge')
bb <- bayesbridge$BayesBridge(
  outcome, X,
  model = 'logit',
  regularizing_slab_size = 1.,
  center_predictor = TRUE
)
n_burnin <- 0L
n_post_burnin <- 1100L
mcmc_output <- bb$gibbs(n_burnin, n_post_burnin, thin=1, bridge_exponent=.25)
mcmc_samples <- mcmc_output$samples


# Diagnose convergence and discard the non-stationary part.
plot(mcmc_samples$logp, type = 'l', xlab = 'MCMC iter', ylab = 'Log density')
n_burnin <- 100 # via visual diagnostic (to be automated)
beta_samples <- mcmc_samples$beta
beta_samples <- beta_samples[-1, ] # Exclude the intercept
beta_samples <- beta_samples[, -seq(1, n_burnin)]


# Visually summarize the posterior.
n_coef_to_plot <- 25

post_mean <- rowMeans(beta_samples)
lower_quantile <- apply(beta_samples, 1, quantile, prob = .025)
upper_quantile <- apply(beta_samples, 1, quantile, prob = .975)
y_min <- min(lower_quantile[1:n_coef_to_plot])
y_max <- max(upper_quantile[1:n_coef_to_plot])
plot(post_mean[1:n_coef_to_plot], pch=4, col='blue', ylim = c(y_min, y_max),
     ylab = 'Coef. estimates')
points(lower_quantile[1:n_coef_to_plot], pch=sprintf("\u2013"), col='green')
points(upper_quantile[1:n_coef_to_plot], pch=sprintf("\u2013"), col='green')
lines(beta_true[1:n_coef_to_plot], col='red', lty='dashed')
