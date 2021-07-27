#' Create model for BayesBridge
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
  bayesbridge <- reticulate::import('bayesbridge')
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
#' @export
create_prior <- function() {

}

construct_bayesbridge <- function() {

}
