test_that("exact operating characteristics match a one-look analytical case", {
  operating_characteristics <- exact_operating_characteristics(
    looks = 1,
    n_max = 1,
    p0 = 0.80,
    p1 = 0.90,
    lambda1 = 0.30,
    lambda2 = 0.10,
    gamma = 0,
    eta = 0,
    true_rate = 0.30
  )

  expect_equal(operating_characteristics$futility_probability, 0.70)
  expect_equal(operating_characteristics$grey_zone_probability, 0.30)
  expect_equal(operating_characteristics$superiority_probability, 0)
  expect_equal(operating_characteristics$expected_sample_size, 1)
})

test_that("exact operating characteristics conserve trial probability", {
  operating_characteristics <- exact_operating_characteristics(
    looks = c(10, 20),
    n_max = 20,
    p0 = 0.20,
    p1 = 0.40,
    lambda1 = 0.20,
    lambda2 = 0.10,
    gamma = 1,
    eta = 1,
    true_rate = 0.30
  )

  total_probability <- operating_characteristics$superiority_probability +
    operating_characteristics$futility_probability +
    operating_characteristics$grey_zone_probability

  expect_equal(total_probability, 1, tolerance = 1e-12)
  expect_gte(operating_characteristics$expected_sample_size, 10)
  expect_lte(operating_characteristics$expected_sample_size, 20)
  expect_true(all(c("sample_size", "action", "probability") %in%
    names(operating_characteristics$stopping_probabilities)))
})

test_that("exact operating characteristics validates the true response rate", {
  expect_error(
    exact_operating_characteristics(
      looks = 1,
      n_max = 1,
      p0 = 0.20,
      p1 = 0.40,
      lambda1 = 0.20,
      lambda2 = 0.10,
      gamma = 0,
      eta = 0,
      true_rate = 1
    ),
    "true_rate"
  )
})