decision_table_arguments <- list(
  looks = c(10, 20),
  n_max = 20,
  p0 = 0.20,
  p1 = 0.40,
  lambda1 = 0.20,
  lambda2 = 0.10,
  gamma = 1,
  eta = 1,
  prior = c(1, 1)
)

test_that("response action table enumerates every response count at every look", {
  table <- do.call(response_action_table, decision_table_arguments)

  expect_s3_class(table, "data.frame")
  expect_equal(nrow(table), 32)
  expect_named(
    table,
    c(
      "sample_size", "responses", "action", "posterior_p0", "posterior_p1",
      "futility_cutoff", "superiority_cutoff"
    )
  )
  expect_equal(table$responses[table$sample_size == 10], 0:10)
  expect_equal(table$responses[table$sample_size == 20], 0:20)
})

test_that("response action table reports the expected interim and final actions", {
  table <- do.call(response_action_table, decision_table_arguments)

  interim_futility <- table[table$sample_size == 10 & table$responses == 0, ]
  interim_continue <- table[table$sample_size == 10 & table$responses == 4, ]
  final_grey_zone <- table[table$sample_size == 20 & table$responses == 8, ]
  final_superiority <- table[table$sample_size == 20 & table$responses == 15, ]

  expect_identical(interim_futility$action, "futility")
  expect_identical(interim_continue$action, "continue")
  expect_identical(final_grey_zone$action, "grey_zone")
  expect_identical(final_superiority$action, "superiority")
})

test_that("response action table requires a final look at the maximum sample size", {
  arguments <- decision_table_arguments
  arguments$looks <- c(10, 15)

  expect_error(
    do.call(response_action_table, arguments),
    "ending at"
  )
})