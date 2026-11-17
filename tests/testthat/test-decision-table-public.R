test_that("decision_table returns actions for a selected design", {
  design <- select_design(
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

  table <- decision_table(design)

  expect_s3_class(table, "data.frame")
  expect_equal(nrow(table), 2)
  expect_identical(table$action, c("futility", "grey_zone"))
})

test_that("decision_table requires a selected design object", {
  expect_error(
    decision_table(list()),
    "pob2ef_design"
  )
})