test_that("valid design inputs are accepted", {
  expect_invisible(validate_design_inputs(
    p0 = 0.20,
    p1 = 0.40,
    n_max = 40,
    looks = c(15, 25, 40),
    prior = c(1, 1),
    alpha = 0.10,
    beta = 0.20
  ))
})

test_that("target rate must exceed the null rate", {
  expect_error(
    validate_design_inputs(
      p0 = 0.20,
      p1 = 0.20,
      n_max = 40,
      looks = c(15, 25, 40),
      prior = c(1, 1),
      alpha = 0.10,
      beta = 0.20
    ),
    "p1"
  )
})

test_that("planned looks must end at the maximum sample size", {
  expect_error(
    validate_design_inputs(
      p0 = 0.20,
      p1 = 0.40,
      n_max = 40,
      looks = c(15, 25, 35),
      prior = c(1, 1),
      alpha = 0.10,
      beta = 0.20
    ),
    "ending at"
  )
})