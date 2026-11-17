test_that("boundary functions calculate the specified boundary family", {
  expect_equal(
    futility_boundary(sample_size = 10, n_max = 20, lambda1 = 0.20, gamma = 1),
    0.10
  )

  expect_equal(
    superiority_boundary(sample_size = 10, n_max = 20, lambda2 = 0.10, eta = 1),
    0.95
  )

  expect_equal(
    futility_boundary(sample_size = 20, n_max = 20, lambda1 = 0.20, gamma = 2),
    0.20
  )

  expect_equal(
    superiority_boundary(sample_size = 20, n_max = 20, lambda2 = 0.10, eta = 2),
    0.90
  )
})

test_that("tuning parameters enforce boundary coherence", {
  expect_invisible(validate_tuning_parameters(0.50, 0.49, 0, 3))

  expect_error(
    validate_tuning_parameters(0.51, 0.20, 1, 1),
    "lambda1"
  )

  expect_error(
    validate_tuning_parameters(0.20, 0.50, 1, 1),
    "lambda2"
  )

  expect_error(
    validate_tuning_parameters(0.20, 0.20, -1, 1),
    "gamma"
  )
})

test_that("boundary functions reject invalid analysis sizes", {
  expect_error(
    futility_boundary(sample_size = 0, n_max = 20, lambda1 = 0.20, gamma = 1),
    "sample_size"
  )

  expect_error(
    superiority_boundary(sample_size = 21, n_max = 20, lambda2 = 0.20, eta = 1),
    "sample_size"
  )
})