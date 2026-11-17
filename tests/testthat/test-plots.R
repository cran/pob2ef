test_that("plotting functions return their underlying data invisibly", {
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
  simulation <- simulate_design(design, true_rates = c(0.30, 0.90), n_sim = 10, seed = 1)
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit(grDevices::dev.off(), add = TRUE)

  table <- plot_decision_boundaries(design)
  expect_invisible(plot_decision_boundaries(design))
  expect_true(any(table$action == "grey_zone"))
  expect_invisible(plot_operating_characteristics(simulation))
})