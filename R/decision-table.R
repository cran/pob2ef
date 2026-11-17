validate_planned_looks <- function(looks, n_max) {
  if (!is.numeric(looks) || length(looks) == 0L || anyNA(looks) ||
      any(looks < 1) || any(looks != as.integer(looks)) ||
      is.unsorted(looks, strictly = TRUE) || utils::tail(looks, 1L) != n_max) {
    stop("`looks` must be increasing positive integers ending at `n_max`.", call. = FALSE)
  }

  invisible(TRUE)
}

response_action_table <- function(
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
  validate_boundary_sample_size(n_max, n_max)
  validate_planned_looks(looks, n_max)
  validate_decision_parameters(p0, p1, prior)
  validate_tuning_parameters(lambda1, lambda2, gamma, eta)

  tables <- lapply(looks, function(sample_size) {
    decisions <- lapply(seq.int(0, sample_size), function(responses) {
      decision_at_look(
        responses = responses,
        sample_size = sample_size,
        n_max = n_max,
        p0 = p0,
        p1 = p1,
        lambda1 = lambda1,
        lambda2 = lambda2,
        gamma = gamma,
        eta = eta,
        prior = prior
      )
    })

    data.frame(
      sample_size = sample_size,
      responses = seq.int(0, sample_size),
      action = vapply(decisions, `[[`, character(1), "action"),
      posterior_p0 = vapply(decisions, `[[`, numeric(1), "posterior_p0"),
      posterior_p1 = vapply(decisions, `[[`, numeric(1), "posterior_p1"),
      futility_cutoff = vapply(decisions, `[[`, numeric(1), "futility_cutoff"),
      superiority_cutoff = vapply(decisions, `[[`, numeric(1), "superiority_cutoff")
    )
  })

  do.call(rbind, tables)
}