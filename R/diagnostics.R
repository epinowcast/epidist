#' Diagnostics for `epidist_fit` models
#'
#' This function computes diagnostics to assess the quality of a fitted model.
#' When the fitting algorithm used is `"sampling"` (HMC) then the output of
#' `epidist_diagnostics` is a `data.table` containing:
#'
#' * `time`: the total time taken to fit all chains
#' * `samples`: the total number of samples across all chains
#' * `max_rhat`: the highest value of the Gelman-Rubin statistic
#' * `divergent_transitions`: the total number of divergent transitions
#' * `per_divergent_transitions`: the proportion of samples which had divergent
#' transitions
#' * `max_treedepth`: the highest value of the treedepth HMC parameter
#' * `no_at_max_treedepth`: the number of samples which attained the
#' `max_treedepth`
#' * `per_at_max_treedepth`: the proportion of samples which attained the
#' `max_treedepth`
#'
#' When the fitting algorithm is not `"sampling"` (see `?brms::brm` for other
#' possible algorithms) then diagnostics are yet to be implemented.
#' @param fit A fitted model of class `epidist_fit`
#' @family diagnostics
#' @autoglobal
#' @export
epidist_diagnostics <- function(fit) {
  if (!inherits(fit, "epidist_fit")) {
    cli::cli_abort(c(
      "!" = "Diagnostics only supported for objects of class epidist_fit"
    ))
  }
  if (fit$algorithm %in% c("laplace", "meanfield", "fullrank", "pathfinder")) {
    cli::cli_abort(c(
      "!" = paste0(
        "Diagnostics not yet supported for the algorithm: ", fit$algorithm
      )
    ))
  }
  if (fit$algorithm == "sampling") {
    np <- brms::nuts_params(fit)
    divergent_indices <- np$Parameter == "divergent__"
    treedepth_indices <- np$Parameter == "treedepth__"
    diagnostics <- data.table(
      "time" = sum(rstan::get_elapsed_time(fit$fit)),
      "samples" = nrow(np) / length(unique(np$Parameter)),
      "max_rhat" = round(max(brms::rhat(fit)), 3),
      "divergent_transitions" = sum(np[divergent_indices, ]$Value),
      "per_divergent_transitions" = mean(np[divergent_indices, ]$Value),
      "max_treedepth" = max(np[treedepth_indices, ]$Value)
    )
    diagnostics[, no_at_max_treedepth :=
                  sum(np[treedepth_indices, ]$Value == max_treedepth)]
    diagnostics[, per_at_max_treedepth := no_at_max_treedepth / samples]
  } else {
    cli::cli_abort(c(
      "!" = paste0("Unrecognised algorithm: ", fit$algorithm)
    ))
  }
  return(diagnostics)
}
