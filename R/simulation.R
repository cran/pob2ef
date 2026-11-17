validate_simulation_inputs <- function(true_rates, n_sim, seed) {
  if (!is.numeric(true_rates) || length(true_rates) == 0L || anyNA(true_rates) ||
      any(true_rates <= 0 | true_rates >= 1)) {
    stop("`true_rates` must contain probabilities strictly between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(n_sim) || length(n_sim) != 1L || is.na(n_sim) ||
      n_sim < 1 || n_sim != as.integer(n_sim)) {
    stop("`n_sim` must be one positive integer.", call. = FALSE)
  }

  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      seed != as.integer(seed))) {
    stop("`seed` must be NULL or one integer.", call. = FALSE)
  }

  invisible(TRUE)
}

simulate_one_trial <- function(design, true_rate) {
  previous_look <- 0L
  responses <- 0L

  for (sample_size in design$inputs$looks) {
    responses <- responses + stats::rbinom(
      n = 1L,
      size = sample_size - previous_look,
      prob = true_rate
    )
    decision <- decision_at_look(
      responses = responses,
      sample_size = sample_size,
      n_max = design$inputs$n_max,
      p0 = design$inputs$p0,
      p1 = design$inputs$p1,
      lambda1 = design$selected$lambda1,
      lambda2 = design$selected$lambda2,
      gamma = design$selected$gamma,
      eta = design$selected$eta,
      prior = design$inputs$prior
    )

    if (decision$action != "continue") {
      return(list(action = decision$action, sample_size = sample_size))
    }

    previous_look <- sample_size
  }

  stop("A final planned look must produce a decision.", call. = FALSE)
}

#' Simulate a selected design
#'
#' Estimates operating characteristics by Monte Carlo simulation for one or
#' more true response rates.
#'
#' @param design A `pob2ef_design` object returned by [select_design()].
#' @param true_rates True response rates to simulate.
#' @param n_sim Number of simulated trials per true response rate.
#' @param seed Optional integer random-number seed.
#'
#' @return An object of class `pob2ef_simulation` containing scenario-level
#'   operating-characteristic estimates and Monte Carlo standard errors.
#'
#' @examples
#' design <- select_design(
#'   n_max = 20, looks = c(10, 20), p0 = 0.20, p1 = 0.40,
#'   alpha = 0.10, beta = 0.20,
#'   lambda1_grid = 0.50, lambda2_grid = 0.20, gamma_grid = 0, eta_grid = 0
#' )
#' simulation <- simulate_design(
#'   design, true_rates = c(0.20, 0.30, 0.40), n_sim = 100, seed = 2026
#' )
#' simulation$results
#' @export
simulate_design <- function(design, true_rates, n_sim = 10000L, seed = NULL) {
  validate_pob2ef_design(design)
  validate_simulation_inputs(true_rates, n_sim, seed)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  results <- lapply(true_rates, function(true_rate) {
    trials <- lapply(seq_len(n_sim), function(simulation_index) {
      simulate_one_trial(design, true_rate)
    })
    actions <- vapply(trials, `[[`, character(1), "action")
    sample_sizes <- vapply(trials, `[[`, numeric(1), "sample_size")

    superiority_probability <- mean(actions == "superiority")
    futility_probability <- mean(actions == "futility")
    grey_zone_probability <- mean(actions == "grey_zone")

    data.frame(
      true_rate = true_rate,
      superiority_probability = superiority_probability,
      futility_probability = futility_probability,
      grey_zone_probability = grey_zone_probability,
      expected_sample_size = mean(sample_sizes),
      superiority_mcse = sqrt(superiority_probability * (1 - superiority_probability) / n_sim),
      futility_mcse = sqrt(futility_probability * (1 - futility_probability) / n_sim),
      grey_zone_mcse = sqrt(grey_zone_probability * (1 - grey_zone_probability) / n_sim),
      expected_sample_size_mcse = stats::sd(sample_sizes) / sqrt(n_sim)
    )
  })

  structure(
    list(
      call = match.call(),
      design = design,
      n_sim = n_sim,
      seed = seed,
      results = do.call(rbind, results)
    ),
    class = "pob2ef_simulation"
  )
}