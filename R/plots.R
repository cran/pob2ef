#' Plot decision boundaries
#'
#' Displays every response-count action at each planned analysis as a filled
#' grid. Interim continuation and final grey-zone outcomes are combined as the
#' grey-zone display category.
#'
#' @param design A `pob2ef_design` object returned by [select_design()].
#' @param ... Additional arguments passed to `graphics::plot()`.
#'
#' @return Invisibly returns the response-count decision table used for the
#'   figure.
#'
#' @examples
#' design <- select_design(
#'   n_max = 20, looks = c(10, 20), p0 = 0.20, p1 = 0.40,
#'   alpha = 0.10, beta = 0.20,
#'   lambda1_grid = 0.50, lambda2_grid = 0.20, gamma_grid = 0, eta_grid = 0
#' )
#' plot_decision_boundaries(design)
#' @export
plot_decision_boundaries <- function(design, ...) {
  table <- decision_table(design)
  display_action <- ifelse(
    table$action %in% c("continue", "grey_zone"),
    "grey_zone",
    table$action
  )
  looks <- sort(unique(table$sample_size))
  x_positions <- match(table$sample_size, looks)
  action_levels <- c("futility", "grey_zone", "superiority")
  action_labels <- c(
    futility = "Futility",
    grey_zone = "Grey zone",
    superiority = "Superiority"
  )
  action_colours <- c(
    futility = "firebrick",
    grey_zone = "goldenrod",
    superiority = "forestgreen"
  )

  graphics::plot(
    NA,
    type = "n",
    xlim = c(0.5, length(looks) + 0.5),
    ylim = c(-0.5, design$inputs$n_max + 0.5),
    xaxt = "n",
    yaxt = "n",
    xlab = "Cumulative sample size",
    ylab = "Cumulative responses",
    ...
  )
  graphics::axis(1, at = seq_along(looks), labels = looks)
  graphics::axis(2, at = 0:design$inputs$n_max, las = 1)
  graphics::abline(
    h = seq(-0.5, design$inputs$n_max + 0.5, by = 1),
    v = seq(0.5, length(looks) + 0.5, by = 1),
    col = "grey85",
    lwd = 0.8
  )
  graphics::rect(
    xleft = x_positions - 0.42,
    ybottom = table$responses - 0.42,
    xright = x_positions + 0.42,
    ytop = table$responses + 0.42,
    col = action_colours[display_action],
    border = "white"
  )
  graphics::legend(
    "topleft",
    legend = action_labels[action_levels[action_levels %in% unique(display_action)]],
    fill = action_colours[action_levels[action_levels %in% unique(display_action)]],
    bty = "n"
  )

  invisible(table)
}

#' Plot simulated operating characteristics
#'
#' @param simulation A `pob2ef_simulation` object returned by
#'   [simulate_design()].
#' @param metric Operating characteristic to display.
#' @param ... Additional arguments passed to `graphics::plot()`.
#'
#' @return Invisibly returns the simulation results used for the figure.
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
#' plot_operating_characteristics(
#'   simulation,
#'   metric = "superiority_probability"
#' )
#' @export
plot_operating_characteristics <- function(
    simulation,
    metric = "superiority_probability",
    ...
) {
  if (!inherits(simulation, "pob2ef_simulation")) {
    stop("`simulation` must be a `pob2ef_simulation` object returned by `simulate_design()`.", call. = FALSE)
  }

  metric <- match.arg(
    metric,
    c(
      "superiority_probability",
      "futility_probability",
      "grey_zone_probability",
      "expected_sample_size"
    )
  )
  results <- simulation$results[order(simulation$results$true_rate), , drop = FALSE]

  graphics::plot(
    results$true_rate,
    results[[metric]],
    type = "b",
    xlab = "True response rate",
    ylab = gsub("_", " ", metric),
    ...
  )

  invisible(results)
}