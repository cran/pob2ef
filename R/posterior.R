posterior_tail_probability <- function(
    responses,
    sample_size,
    target_rate,
    prior = c(1, 1)
) {
  if (!is.numeric(responses) || length(responses) != 1L || is.na(responses) ||
      responses < 0 || responses != as.integer(responses)) {
    stop("`responses` must be one nonnegative integer.", call. = FALSE)
  }

  if (!is.numeric(sample_size) || length(sample_size) != 1L || is.na(sample_size) ||
      sample_size < 0 || sample_size != as.integer(sample_size)) {
    stop("`sample_size` must be one nonnegative integer.", call. = FALSE)
  }

  if (responses > sample_size) {
    stop("`responses` cannot exceed `sample_size`.", call. = FALSE)
  }

  if (!is.numeric(target_rate) || length(target_rate) != 1L || is.na(target_rate) ||
      target_rate <= 0 || target_rate >= 1) {
    stop("`target_rate` must be one probability strictly between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(prior) || length(prior) != 2L || anyNA(prior) || any(prior <= 0)) {
    stop("`prior` must contain two positive beta-shape parameters.", call. = FALSE)
  }

  1 - stats::pbeta(
    q = target_rate,
    shape1 = prior[1] + responses,
    shape2 = prior[2] + sample_size - responses
  )
}