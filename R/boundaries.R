validate_tuning_parameters <- function(lambda1, lambda2, gamma, eta) {
  if (!is.numeric(lambda1) || length(lambda1) != 1L || is.na(lambda1) ||
      lambda1 <= 0 || lambda1 > 0.5) {
    stop("`lambda1` must be one value in (0, 0.5].", call. = FALSE)
  }

  if (!is.numeric(lambda2) || length(lambda2) != 1L || is.na(lambda2) ||
      lambda2 <= 0 || lambda2 >= 0.5) {
    stop("`lambda2` must be one value in (0, 0.5).", call. = FALSE)
  }

  if (!is.numeric(gamma) || length(gamma) != 1L || is.na(gamma) || gamma < 0) {
    stop("`gamma` must be one nonnegative value.", call. = FALSE)
  }

  if (!is.numeric(eta) || length(eta) != 1L || is.na(eta) || eta < 0) {
    stop("`eta` must be one nonnegative value.", call. = FALSE)
  }

  invisible(TRUE)
}

validate_boundary_sample_size <- function(sample_size, n_max) {
  if (!is.numeric(n_max) || length(n_max) != 1L || is.na(n_max) ||
      n_max < 1 || n_max != as.integer(n_max)) {
    stop("`n_max` must be one positive integer.", call. = FALSE)
  }

  if (!is.numeric(sample_size) || length(sample_size) != 1L || is.na(sample_size) ||
      sample_size < 1 || sample_size != as.integer(sample_size) ||
      sample_size > n_max) {
    stop("`sample_size` must be an integer from 1 through `n_max`.", call. = FALSE)
  }

  invisible(TRUE)
}

futility_boundary <- function(sample_size, n_max, lambda1, gamma) {
  validate_boundary_sample_size(sample_size, n_max)
  validate_tuning_parameters(lambda1, lambda2 = 0.25, gamma, eta = 0)

  lambda1 * (sample_size / n_max)^gamma
}

superiority_boundary <- function(sample_size, n_max, lambda2, eta) {
  validate_boundary_sample_size(sample_size, n_max)
  validate_tuning_parameters(lambda1 = 0.25, lambda2, gamma = 0, eta)

  1 - lambda2 * (sample_size / n_max)^eta
}