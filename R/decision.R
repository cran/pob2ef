validate_decision_parameters <- function(p0, p1, prior) {
  if (!is.numeric(p0) || length(p0) != 1L || is.na(p0) || p0 <= 0 || p0 >= 1) {
    stop("`p0` must be one probability strictly between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(p1) || length(p1) != 1L || is.na(p1) || p1 <= p0 || p1 >= 1) {
    stop("`p1` must be one probability strictly between `p0` and 1.", call. = FALSE)
  }

  if (!is.numeric(prior) || length(prior) != 2L || anyNA(prior) || any(prior <= 0)) {
    stop("`prior` must contain two positive beta-shape parameters.", call. = FALSE)
  }

  invisible(TRUE)
}

decision_at_look <- function(
    responses,
    sample_size,
    n_max,
    p0,
    p1,
    lambda1,
    lambda2,
    gamma,
    eta,
    prior = c(1, 1)
) {
  validate_boundary_sample_size(sample_size, n_max)
  validate_decision_parameters(p0, p1, prior)
  validate_tuning_parameters(lambda1, lambda2, gamma, eta)

  posterior_p0 <- posterior_tail_probability(
    responses = responses,
    sample_size = sample_size,
    target_rate = p0,
    prior = prior
  )
  posterior_p1 <- posterior_tail_probability(
    responses = responses,
    sample_size = sample_size,
    target_rate = p1,
    prior = prior
  )
  futility_cutoff <- futility_boundary(sample_size, n_max, lambda1, gamma)
  superiority_cutoff <- superiority_boundary(sample_size, n_max, lambda2, eta)

  action <- if (posterior_p0 < futility_cutoff) {
    "futility"
  } else if (posterior_p1 > superiority_cutoff) {
    "superiority"
  } else if (sample_size == n_max) {
    "grey_zone"
  } else {
    "continue"
  }

  list(
    action = action,
    posterior_p0 = posterior_p0,
    posterior_p1 = posterior_p1,
    futility_cutoff = futility_cutoff,
    superiority_cutoff = superiority_cutoff
  )
}