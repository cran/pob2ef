validate_true_rate <- function(true_rate) {
  if (!is.numeric(true_rate) || length(true_rate) != 1L || is.na(true_rate) ||
      true_rate <= 0 || true_rate >= 1) {
    stop("`true_rate` must be one probability strictly between 0 and 1.", call. = FALSE)
  }

  invisible(TRUE)
}

exact_operating_characteristics <- function(
    looks,
    n_max,
    p0,
    p1,
    lambda1,
    lambda2,
    gamma,
    eta,
    true_rate,
    prior = c(1, 1)
) {
  validate_true_rate(true_rate)
  actions <- response_action_table(
    looks = looks,
    n_max = n_max,
    p0 = p0,
    p1 = p1,
    lambda1 = lambda1,
    lambda2 = lambda2,
    gamma = gamma,
    eta = eta,
    prior = prior
  )

  active_paths <- data.frame(responses = 0L, probability = 1)
  previous_look <- 0L
  stopping_probabilities <- vector("list", length(looks))

  for (look_index in seq_along(looks)) {
    sample_size <- looks[look_index]
    increment_size <- sample_size - previous_look
    response_increments <- seq.int(0L, increment_size)

    path_probabilities <- do.call(
      rbind,
      lapply(seq_len(nrow(active_paths)), function(path_index) {
        data.frame(
          responses = active_paths$responses[path_index] + response_increments,
          probability = active_paths$probability[path_index] * stats::dbinom(
            response_increments,
            size = increment_size,
            prob = true_rate
          )
        )
      })
    )
    path_probabilities <- stats::aggregate(
      probability ~ responses,
      data = path_probabilities,
      FUN = sum
    )

    look_actions <- actions[actions$sample_size == sample_size, c("responses", "action")]
    path_probabilities <- merge(path_probabilities, look_actions, by = "responses", sort = TRUE)

    stopping_probabilities[[look_index]] <- stats::aggregate(
      probability ~ action,
      data = path_probabilities[path_probabilities$action != "continue", ],
      FUN = sum
    )
    stopping_probabilities[[look_index]]$sample_size <- sample_size

    active_paths <- path_probabilities[path_probabilities$action == "continue", c(
      "responses", "probability"
    )]
    previous_look <- sample_size
  }

  stopping_probabilities <- do.call(rbind, stopping_probabilities)
  rownames(stopping_probabilities) <- NULL

  action_probability <- function(action) {
    sum(stopping_probabilities$probability[stopping_probabilities$action == action])
  }

  list(
    true_rate = true_rate,
    superiority_probability = action_probability("superiority"),
    futility_probability = action_probability("futility"),
    grey_zone_probability = action_probability("grey_zone"),
    expected_sample_size = sum(
      stopping_probabilities$sample_size * stopping_probabilities$probability
    ),
    stopping_probabilities = stopping_probabilities
  )
}