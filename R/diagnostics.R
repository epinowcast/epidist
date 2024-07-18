#' Diagnostics for fitted epidist model
#'
#' @family generics
#' @param fit ...
#' @export
epidist_diagnostics <- function(fit) {
  UseMethod("epidist_diagnostics")
}

#' Default method for returning diagnostics
#'
#' @param fit ...
#' @family defaults
#' @method epidist_diagnostics default
#' @export
epidist_diagnostics.default <- function(fit) {
  stop(
    "No epidist_diagnostics method implemented for the class ", class(data), "\n",
    "See methods(epidist_diagnostics) for available methods"
  )
}

#' Default method for returning diagnostics
#'
#' @param fit ...
#' @family defaults
#' @method epidist_diagnostics epidist_fit
#' @export
epidist_diagnostics.epidist_fit <- function(fit) {
  if (fit$algorithm %in% c("laplace", "meanfield", "fullrank", "pathfinder")) {
    cli::cli_abort(c(
      "!" = paste0(
        "Diagnostics not yet supported for the algorithm: ", fit$algorithm
      )
    ))
  } else if (!fit$algorithm == "sampling") {
    cli::cli_abort(c( 
      "!" = paste0(
        "Unrecognised algorithm: ", fit$algorithm
      )
    ))
  } else if (fit$algorithm == "sampling") {
    np <- brms::nuts_params(fit)
    divergent_indices <- np$Parameter == "divergent__"
    treedepth_indices <- np$Parameter == "treedepth__"
    diagnostics <- data.table(
      "samples" = nrow(np) / length(unique(np$Parameter)),
      "max_rhat" = round(max(brms::rhat(fit)), 3),
      "divergent_transitions" = sum(np[divergent_indices, ]$Value),
      "per_divergent_transitions" = mean(np[divergent_indices, ]$Value),
      "max_treedepth" = max(np[treedepth_indices, ]$Value)
    )
    diagnostics[, no_at_max_treedepth :=
                    sum(np[treedepth_indices, ]$Value == max_treedepth)]
    diagnostics[, per_at_max_treedepth := no_at_max_treedepth / samples]
  }
  
  # rstan::get_elapsed_time(fit$fit)
  
  return(diagnostics)
}
