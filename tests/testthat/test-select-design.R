test_that("candidate evaluation reports the required operating characteristics", {
  candidate <- evaluate_candidate_design(
    looks = 1,
    n_max = 1,
    p0 = 0.80,
    p1 = 0.90,
    lambda1 = 0.30,
    lambda2 = 0.10,
    gamma = 0,
    eta = 0
  )

  expect_named(
    candidate,
    c(
      "lambda1", "lambda2", "gamma", "eta", "type1_error", "type2_error",
      "grey_zone_probability_p1", "expected_sample_size_p0",
      "expected_sample_size_p1", "superiority_probability_p1"
    )
  )
  expect_equal(candidate$type1_error, 0)
  expect_equal(candidate$type2_error, 0.10)
})

test_that("select_design selects a feasible candidate by grey-zone probability", {
  design <- select_design(
    n_max = 1,
    looks = 1,
    p0 = 0.80,
    p1 = 0.90,
    alpha = 0.10,
    beta = 0.20,
    lambda1_grid = c(0.10, 0.30),
    lambda2_grid = 0.10,
    gamma_grid = 0,
    eta_grid = 0
  )

  expect_s3_class(design, "pob2ef_design")
  expect_true(all(design$feasible_candidates$type1_error <= 0.10))
  expect_true(all(design$feasible_candidates$type2_error <= 0.20))
  expect_equal(
    design$selected$grey_zone_probability_p1,
    min(design$feasible_candidates$grey_zone_probability_p1)
  )
})

test_that("select_design rejects a grid with no feasible candidate", {
  expect_error(
    select_design(
      n_max = 1,
      looks = 1,
      p0 = 0.80,
      p1 = 0.90,
      alpha = 0.10,
      beta = 0.05,
      lambda1_grid = 0.30,
      lambda2_grid = 0.10,
      gamma_grid = 0,
      eta_grid = 0
    ),
    "No grid candidate"
  )
})