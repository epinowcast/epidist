#' Diagnostics
#'
#' @param fit ...
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
