validate_pob2ef_design <- function(design) {
  if (!inherits(design, "pob2ef_design")) {
    stop("`design` must be a `pob2ef_design` object returned by `select_design()`.", call. = FALSE)
  }

  required_inputs <- c("n_max", "looks", "p0", "p1", "prior")
  required_parameters <- c("lambda1", "lambda2", "gamma", "eta")

  if (is.null(design$inputs) || !all(required_inputs %in% names(design$inputs)) ||
      is.null(design$selected) || nrow(design$selected) != 1L ||
      !all(required_parameters %in% names(design$selected))) {
    stop("`design` is missing required selected parameters or input settings.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Create a response-count decision table
#'
#' Returns posterior evidence, probability boundaries, and the futility,
#' superiority, continue, or final grey-zone action for every possible response
#' count at each planned analysis.
#'
#' @param design A `pob2ef_design` object returned by [select_design()].
#'
#' @return A data frame with one row per possible cumulative response count at
#'   each planned look.
#'
#' @examples
#' design <- select_design(
#'   n_max = 20, looks = c(10, 20), p0 = 0.20, p1 = 0.40,
#'   alpha = 0.10, beta = 0.20,
#'   lambda1_grid = 0.50, lambda2_grid = 0.20, gamma_grid = 0, eta_grid = 0
#' )
#' decision_table(design)
#' @export
decision_table <- function(design) {
  validate_pob2ef_design(design)

  response_action_table(
    looks = design$inputs$looks,
    n_max = design$inputs$n_max,
    p0 = design$inputs$p0,
    p1 = design$inputs$p1,
    lambda1 = design$selected$lambda1,
    lambda2 = design$selected$lambda2,
    gamma = design$selected$gamma,
    eta = design$selected$eta,
    prior = design$inputs$prior
  )
}