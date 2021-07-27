test_that("Non-binary outcome throws an error", {
  design_mat <- matrix(0, nrow = 3, ncol = 2)

  n_success <- c(0, 0.5, 1)
  expect_error(
    create_model(n_success, design_mat),
    "Number of successes must be an integer-valued numeric vector"
  )

  outcome <- list(n_success = c(0, 1, 1), n_trial = c(1, 1.5, 2))
  expect_error(
    create_model(outcome, design_mat),
    "Number of trials must be an integer-valued numeric vector"
  )
})

test_that("More success than trial throws an error", {
  design_mat <- matrix(0, nrow = 3, ncol = 2)
  outcome <- list(n_success = c(0, 1, 2), n_trial = c(1, 1, 1))
  expect_error(
    create_model(outcome, design_mat),
    "Number of successes must be <= number of trials"
  )
})
