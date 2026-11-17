test_that("posterior tail probability uses beta-binomial updating", {
  actual <- posterior_tail_probability(
    responses = 8,
    sample_size = 20,
    target_rate = 0.30,
    prior = c(1, 1)
  )

  expected <- 1 - stats::pbeta(0.30, shape1 = 9, shape2 = 13)

  expect_equal(actual, expected)
})

test_that("posterior tail probability respects a non-uniform prior", {
  actual <- posterior_tail_probability(
    responses = 8,
    sample_size = 20,
    target_rate = 0.30,
    prior = c(2, 3)
  )

  expected <- 1 - stats::pbeta(0.30, shape1 = 10, shape2 = 15)

  expect_equal(actual, expected)
})

test_that("posterior tail probability rejects invalid data", {
  expect_error(
    posterior_tail_probability(21, 20, 0.30),
    "cannot exceed"
  )

  expect_error(
    posterior_tail_probability(8, 20, 1),
    "target_rate"
  )

  expect_error(
    posterior_tail_probability(8, 20, 0.30, prior = c(1, 0)),
    "prior"
  )
})