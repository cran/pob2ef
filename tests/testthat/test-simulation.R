simulation_test_design <- function() {
  select_design(
    n_max = 1,
    looks = 1,
    p0 = 0.80,
    p1 = 0.90,
    alpha = 0.10,
    beta = 0.20,
    lambda1_grid = 0.30,
    lambda2_grid = 0.10,
    gamma_grid = 0,
    eta_grid = 0
  )
}

test_that("simulation is reproducible with a seed", {
  design <- simulation_test_design()
  first <- simulate_design(design, true_rates = c(0.30, 0.90), n_sim = 100, seed = 123)
  second <- simulate_design(design, true_rates = c(0.30, 0.90), n_sim = 100, seed = 123)

  expect_s3_class(first, "pob2ef_simulation")
  expect_equal(first$results, second$results)
  expect_equal(rowSums(first$results[, c(
    "superiority_probability", "futility_probability", "grey_zone_probability"
  )]), rep(1, 2))
})

test_that("simulation approximates a one-look analytical result", {
  design <- simulation_test_design()
  simulation <- simulate_design(design, true_rates = 0.30, n_sim = 5000, seed = 456)
  result <- simulation$results

  expect_equal(result$futility_probability, 0.70, tolerance = 0.03)
  expect_equal(result$grey_zone_probability, 0.30, tolerance = 0.03)
  expect_equal(result$expected_sample_size, 1)
})

test_that("simulation validates its inputs", {
  design <- simulation_test_design()

  expect_error(
    simulate_design(design, true_rates = 1, n_sim = 10),
    "true_rates"
  )
  expect_error(
    simulate_design(design, true_rates = 0.30, n_sim = 0),
    "n_sim"
  )
})