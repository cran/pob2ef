validate_tuning_grid <- function(values, name, lower, upper, include_upper = TRUE) {
  if (!is.numeric(values) || length(values) == 0L || anyNA(values)) {
    stop(sprintf("`%s_grid` must be a nonempty numeric vector.", name), call. = FALSE)
  }

  within_bounds <- if (include_upper) {
    values >= lower & values <= upper
  } else {
    values >= lower & values < upper
  }

  if (any(!within_bounds)) {
    upper_operator <- if (include_upper) "[" else "["
    upper_bracket <- if (include_upper) "]" else ")"
    stop(
      sprintf("`%s_grid` values must lie in %s%s, %s%s.", name, upper_operator, lower, upper, upper_bracket),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

evaluate_candidate_design <- function(
    looks,
    n_max,
    p0,
    p1,
    lambda1,
    lambda2,
    gamma,
    eta,
    prior = c(1, 1)
) {
  operating_characteristics_p0 <- exact_operating_characteristics(
    looks = looks,
    n_max = n_max,
    p0 = p0,
    p1 = p1,
    lambda1 = lambda1,
    lambda2 = lambda2,
    gamma = gamma,
    eta = eta,
    true_rate = p0,
    prior = prior
  )
  operating_characteristics_p1 <- exact_operating_characteristics(
    looks = looks,
    n_max = n_max,
    p0 = p0,
    p1 = p1,
    lambda1 = lambda1,
    lambda2 = lambda2,
    gamma = gamma,
    eta = eta,
    true_rate = p1,
    prior = prior
  )

  data.frame(
    lambda1 = lambda1,
    lambda2 = lambda2,
    gamma = gamma,
    eta = eta,
    type1_error = operating_characteristics_p0$superiority_probability,
    type2_error = operating_characteristics_p1$futility_probability,
    grey_zone_probability_p1 = operating_characteristics_p1$grey_zone_probability,
    expected_sample_size_p0 = operating_characteristics_p0$expected_sample_size,
    expected_sample_size_p1 = operating_characteristics_p1$expected_sample_size,
    superiority_probability_p1 = operating_characteristics_p1$superiority_probability
  )
}

#' Select a partially optimized Bayesian phase II design
#'
#' Searches a grid of boundary parameters and returns the feasible candidate
#' that minimizes the final grey-zone probability at the desirable response rate
#' subject to the prespecified error constraints.
#'
#' @param n_max Maximum evaluable sample size.
#' @param looks Increasing planned analysis sample sizes ending at `n_max`.
#' @param p0 Unacceptable response-rate benchmark.
#' @param p1 Clinically desirable response-rate target, strictly greater than
#'   `p0`.
#' @param alpha Maximum type I error, defined as superiority probability at
#'   `p0`.
#' @param beta Maximum type II error, defined as futility probability at `p1`.
#' @param prior Two positive beta-prior shape parameters.
#' @param lambda1_grid Candidate values for the futility boundary level.
#' @param lambda2_grid Candidate values for the superiority boundary level.
#' @param gamma_grid Candidate values for the futility boundary shape.
#' @param eta_grid Candidate values for the superiority boundary shape.
#'
#' @return An object of class `pob2ef_design` containing the selected
#'   parameters, operating characteristics, feasible candidates, and all
#'   evaluated candidates.
#'
#' @examples
#' design <- select_design(
#'   n_max = 20,
#'   looks = c(10, 20),
#'   p0 = 0.20,
#'   p1 = 0.40,
#'   alpha = 0.10,
#'   beta = 0.20,
#'   lambda1_grid = 0.50,
#'   lambda2_grid = 0.20,
#'   gamma_grid = 0,
#'   eta_grid = 0
#' )
#' design$selected
#' @export
select_design <- function(
    n_max,
    looks,
    p0,
    p1,
    alpha,
    beta,
    prior = c(1, 1),
    lambda1_grid = seq(0.05, 0.50, by = 0.05),
    lambda2_grid = seq(0.05, 0.45, by = 0.05),
    gamma_grid = 0:3,
    eta_grid = 0:3
) {
  validate_design_inputs(p0, p1, n_max, looks, prior, alpha, beta)
  validate_tuning_grid(lambda1_grid, "lambda1", lower = 0, upper = 0.5)
  validate_tuning_grid(lambda2_grid, "lambda2", lower = 0, upper = 0.5, include_upper = FALSE)
  validate_tuning_grid(gamma_grid, "gamma", lower = 0, upper = Inf)
  validate_tuning_grid(eta_grid, "eta", lower = 0, upper = Inf)

  if (any(lambda1_grid == 0) || any(lambda2_grid == 0)) {
    stop("`lambda1_grid` and `lambda2_grid` must contain only positive values.", call. = FALSE)
  }

  tuning_grid <- expand.grid(
    lambda1 = lambda1_grid,
    lambda2 = lambda2_grid,
    gamma = gamma_grid,
    eta = eta_grid,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  candidate_results <- do.call(
    rbind,
    lapply(seq_len(nrow(tuning_grid)), function(candidate_index) {
      candidate <- tuning_grid[candidate_index, ]
      evaluate_candidate_design(
        looks = looks,
        n_max = n_max,
        p0 = p0,
        p1 = p1,
        lambda1 = candidate$lambda1,
        lambda2 = candidate$lambda2,
        gamma = candidate$gamma,
        eta = candidate$eta,
        prior = prior
      )
    })
  )
  candidate_results$feasible <- with(
    candidate_results,
    type1_error <= alpha & type2_error <= beta
  )
  feasible_candidates <- candidate_results[candidate_results$feasible, , drop = FALSE]

  if (nrow(feasible_candidates) == 0L) {
    stop("No grid candidate satisfies the specified type I and type II error constraints.", call. = FALSE)
  }

  selection_order <- order(
    feasible_candidates$grey_zone_probability_p1,
    feasible_candidates$expected_sample_size_p0
  )
  selected <- feasible_candidates[selection_order[1L], , drop = FALSE]

  structure(
    list(
      call = match.call(),
      inputs = list(
        n_max = n_max,
        looks = looks,
        p0 = p0,
        p1 = p1,
        alpha = alpha,
        beta = beta,
        prior = prior
      ),
      selected = selected,
      feasible_candidates = feasible_candidates,
      candidate_results = candidate_results
    ),
    class = "pob2ef_design"
  )
}