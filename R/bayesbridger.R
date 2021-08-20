#' Create model for BayesBridge
#'
#' @param outcome  integer vector, integer-valued numeric vector, or list with a
#'   pair of such vectors as elements `n_success` and `n_trial`. If a vector, it
#'   is assumed to represent binary outcomes.
#' @param design_mat  matrix or Matrix::dgRMatrix. Other sparse matrix types get
#'   internally converted to the CSR format by the Python package.
#' @param family  Only 'logit' is currently supported by this R wrapper. The original
#'   Python package supports other models. See https://bayes-bridge.readthedocs.io/.
#' @param center_pred  logical. For a sparse design matrix, centering predictor
#'   is done implicitly to preserve the sparse representation.
#'
#' @export
create_model <- function(outcome, design_mat, family = 'logit', center_pred = TRUE) {
  # Validate input arguments
  if (family == 'logit') {
    if (is.list(outcome) && length(outcome) == 2)  {
      if ("n_success" %in% names(outcome) && "n_trial" %in% names(outcome)) {
        n_success <- outcome$n_success
        n_trial <- outcome$n_trial
        check_logit_outcome_arg(n_success, n_trial)
      } else {
        stop("Invalid outcome list.")
      }
    } else if (is.vector(outcome)) {
      n_success <- outcome
      check_logit_outcome_arg(n_success)
      n_trial <- rep(1, length(n_success))
    } else {
      stop("Invalid outcome argument.")
    }
  } else {
    stop(paste(
      sprintf("The family %s is not yet supported by this R wrapper,"),
      "but may be available via direct (reticulate) access to the original Python package.",
      "Check https://bayes-bridge.readthedocs.io/."
    ))
  }
  # Initialize BayesBridge's RegressionModel
  outcome_py <- reticulate::tuple(
    reticulate::np_array(n_success),
    reticulate::np_array(n_trial)
  )
  design_mat_py <- reticulate::r_to_py(design_mat)
  model <- bayesbridge$RegressionModel(
    outcome_py, design_mat_py,
    family = family,
    center_predictor = TRUE
  )
  return(model)
}

check_logit_outcome_arg <- function(n_success, n_trial=NULL) {
  stopifnot(
    "Number of successes must be an integer-valued numeric vector" =
    is_numeric_vector(n_success) && is_integer_valued(n_success)
  )
  if (is.null(n_trial)) {
    stopifnot(
      "Number of successes must be binary unless number of trials is specified." =
      all(n_success == 0 || n_success == 1)
    )
  } else {
    stopifnot(
      "Number of trials must be an integer-valued numeric vector" =
      is_numeric_vector(n_trial) && is_integer_valued(n_trial)
    )
    stopifnot(
      "Number of successes must be <= number of trials" =
      n_success <= n_trial
    )
  }
}

is_numeric_vector <- function(v) {
  return(!is.list(v) && is.vector(v) && is.numeric(v))
}

is_integer_valued <- function(v) {
  return(all(v %% 1 == 0))
}

#' Create prior for BayesBridge
#'
#' @param bridge_exponent
#'   Exponent of the bridge prior on regression coefficients. For example, the
#'   value of 2 (albeit unsupported) would correspond to Gaussian prior and
#'   of 1 to double-exponential prior as in Bayesian Lasso.
#' @param regularizing_slab_size
#'   Standard deviation of the Gaussian tail-regularizer on the bridge prior.
#'   Used to impose soft prior constraints on a range of regression coefficients
#'   in case the data provides limited information (e.g. when complete separation
#'   occurs). One may, for example, set the slab size by first choosing a value
#'   which regression coefficients are very unlikely to exceed in magnitude and
#'   then dividing the value by 1.96.
#' @param n_fixed_effect
#'   Integer specifying the number of regression coefficients to be estimated
#'   without regularization. The first `n_fixed_effect` columns of the design
#'   matrix ix treated as such fixed effects.
#' @param sd_for_fixed_effect
#'   Double or numeric vector of length `n_fixed_effect`, specifying standard
#'   deviation(s) of Gaussian prior(s) on fixed effects. `Inf` corresponds to
#'   an uninformative flat prior.
#' @param sd_for_intercept
#'   Standard deviation of Gaussian prior on the intercept. `Inf` corresponds to
#'   an uninformative flat prior.
#' @param global_scale_prior_hyper_param
#'   NULL or named list with elements `log10_mean` and `log10_sd`, specifying
#'   the prior mean and standard deviation of `log10(global_scale)`. If NULL,
#'   the default reference prior for a scale parameter is used.
#'
#' @export
create_prior <- function(
    bridge_exponent = 0.25,
    regularizing_slab_size = 1,
    n_fixed_effect = 0L,
    sd_for_fixed_effect = Inf,
    sd_for_intercept = Inf,
    global_scale_prior_hyper_param = NULL
  ) {
  prior = bayesbridge$RegressionCoefPrior(
    bridge_exponent = bridge_exponent,
    regularizing_slab_size = regularizing_slab_size,
    n_fixed_effect = as.integer(n_fixed_effect),
    sd_for_fixed_effect = sd_for_fixed_effect,
    sd_for_intercept = sd_for_intercept,
    global_scale_prior_hyper_param = global_scale_prior_hyper_param
  )
  return(prior)
}

#' Instantiate the BayesBridge object for Gibbs sampling
#'
#' @param model
#'   RegressionModel object returned by the `create_model` function
#' @param prior
#'   RegressionCoefPrior object returned by the `create_prior` function
#'
#' @export
instantiate_bayesbridge <- function(model, prior) {
  return(bayesbridge$BayesBridge(model, prior))
}
