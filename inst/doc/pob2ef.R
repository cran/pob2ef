## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(comment = NA)
library(pob2ef)

## ----select-design------------------------------------------------------------
design <- select_design(
  n_max = 20,
  looks = c(10, 20),
  p0 = 0.20,
  p1 = 0.40,
  alpha = 0.10,
  beta = 0.20,
  lambda1_grid = 0.50,
  lambda2_grid = 0.20,
  gamma_grid = 0,
  eta_grid = 0
)

design$selected

## ----decision-table-----------------------------------------------------------
actions <- decision_table(design)
actions[actions$sample_size == 20, c("responses", "action")]

## ----simulation---------------------------------------------------------------
simulation <- simulate_design(
  design,
  true_rates = c(0.20, 0.30, 0.40),
  n_sim = 500,
  seed = 2026
)

simulation$results

