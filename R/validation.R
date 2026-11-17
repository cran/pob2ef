validate_design_inputs <- function(p0, p1, n_max, looks, prior, alpha, beta) {
  if (!is.numeric(p0) || length(p0) != 1L || is.na(p0) || p0 <= 0 || p0 >= 1) {
    stop("`p0` must be one probability strictly between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(p1) || length(p1) != 1L || is.na(p1) || p1 <= p0 || p1 >= 1) {
    stop("`p1` must be one probability strictly between `p0` and 1.", call. = FALSE)
  }

  if (!is.numeric(n_max) || length(n_max) != 1L || is.na(n_max) ||
      n_max < 1 || n_max != as.integer(n_max)) {
    stop("`n_max` must be one positive integer.", call. = FALSE)
  }

  if (!is.numeric(looks) || length(looks) == 0L || anyNA(looks) ||
      any(looks < 1) || any(looks != as.integer(looks)) ||
      is.unsorted(looks, strictly = TRUE) || utils::tail(looks, 1L) != n_max) {
    stop("`looks` must be increasing positive integers ending at `n_max`.", call. = FALSE)
  }

  if (!is.numeric(prior) || length(prior) != 2L || anyNA(prior) || any(prior <= 0)) {
    stop("`prior` must contain two positive beta-shape parameters.", call. = FALSE)
  }

  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be one probability strictly between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(beta) || length(beta) != 1L || is.na(beta) ||
      beta <= 0 || beta >= 1) {
    stop("`beta` must be one probability strictly between 0 and 1.", call. = FALSE)
  }

  invisible(TRUE)
}