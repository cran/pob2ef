decision_arguments <- list(
  n_max = 20,
  p0 = 0.20,
  p1 = 0.40,
  lambda1 = 0.20,
  lambda2 = 0.10,
  gamma = 1,
  eta = 1,
  prior = c(1, 1)
)

test_that("decision at a look stops for futility with sufficiently poor evidence", {
  decision <- do.call(
    decision_at_look,
    c(list(responses = 0, sample_size = 20), decision_arguments)
  )

  expect_identical(decision$action, "futility")
  expect_lt(decision$posterior_p0, decision$futility_cutoff)
})

test_that("decision at a look stops for superiority with sufficiently strong evidence", {
  decision <- do.call(
    decision_at_look,
    c(list(responses = 15, sample_size = 20), decision_arguments)
  )

  expect_identical(decision$action, "superiority")
  expect_gt(decision$posterior_p1, decision$superiority_cutoff)
})

test_that("decision at an interim look continues with intermediate evidence", {
  decision <- do.call(
    decision_at_look,
    c(list(responses = 4, sample_size = 10), decision_arguments)
  )

  expect_identical(decision$action, "continue")
  expect_gte(decision$posterior_p0, decision$futility_cutoff)
  expect_lte(decision$posterior_p1, decision$superiority_cutoff)
})

test_that("decision at the final look retains the grey zone", {
  decision <- do.call(
    decision_at_look,
    c(list(responses = 8, sample_size = 20), decision_arguments)
  )

  expect_identical(decision$action, "grey_zone")
  expect_gte(decision$posterior_p0, decision$futility_cutoff)
  expect_lte(decision$posterior_p1, decision$superiority_cutoff)
})

test_that("decision uses strict stopping inequalities", {
  decision <- decision_at_look(
    responses = 0,
    sample_size = 10,
    n_max = 20,
    p0 = 0.20,
    p1 = 0.40,
    lambda1 = posterior_tail_probability(0, 10, target_rate = 0.20),
    lambda2 = 0.10,
    gamma = 0,
    eta = 1
  )

  expect_identical(decision$action, "continue")
  expect_equal(decision$posterior_p0, decision$futility_cutoff)
})