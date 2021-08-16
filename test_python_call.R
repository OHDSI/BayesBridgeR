library(Matrix)
library(reticulate)

python_path <- reticulate::py_discover_config()$python
bayesbridger::setup_python_env(python_path=python_path)
bayesbridger::configure_python()

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
outcome <- list(n_success = n_success, n_trial = n_trial)

# Generate posterior samples via Python 'bayesbridge' package
bayesbridge <- reticulate::import("bayesbridge")
model <- bayesbridger::create_model(outcome, X)
prior <- bayesbridger::create_prior(
  bridge_exponent=.25,
  regularizing_slab_size = 1.
)
bb <- bayesbridge$BayesBridge(model, prior)
n_burnin <- 0L
n_post_burnin <- 1100L
mcmc_output <- bb$gibbs(n_burnin, n_post_burnin, thin=1, n_status_update = 10)
mcmc_samples <- mcmc_output$samples


# Diagnose convergence and discard the non-stationary part.
plot(mcmc_samples$logp, type = 'l', xlab = 'MCMC iter', ylab = 'Log density')
n_burnin <- 100 # via visual diagnostic (to be automated)
coef_samples <- mcmc_samples$coef
coef_samples <- coef_samples[-1, ] # Exclude the intercept
coef_samples <- coef_samples[, -seq(1, n_burnin)]


# Visually summarize the posterior.
n_coef_to_plot <- 25

post_mean <- rowMeans(coef_samples)
lower_quantile <- apply(coef_samples, 1, quantile, prob = .025)
upper_quantile <- apply(coef_samples, 1, quantile, prob = .975)
y_min <- min(lower_quantile[1:n_coef_to_plot])
y_max <- max(upper_quantile[1:n_coef_to_plot])
plot(post_mean[1:n_coef_to_plot], pch=4, col='blue', ylim = c(y_min, y_max),
     ylab = 'Coef. estimates')
points(lower_quantile[1:n_coef_to_plot], pch=sprintf("\u2013"), col='green')
points(upper_quantile[1:n_coef_to_plot], pch=sprintf("\u2013"), col='green')
lines(beta_true[1:n_coef_to_plot], col='red', lty='dashed')

