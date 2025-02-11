#' Diagnostics for `epidist_fit` models
#'
#' This function computes diagnostics to assess the quality of a fitted model.
#' When the fitting algorithm used is `"sampling"` (HMC) then the output of
#' `epidist_diagnostics` is a `data.frame` containing:
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
#' When the fitting algorithm is not `"sampling"` (see [brms::brm()] for other
#' possible algorithms) then diagnostics are yet to be implemented.
#' @param fit A fitted model of class `epidist_fit`
#' @family diagnostics
#' @autoglobal
#' @export
#' @examples
#' fit <- sierra_leone_ebola_data |>
#'   as_epidist_linelist_data(
#'     pdate_lwr = "date_of_symptom_onset",
#'     sdate_lwr = "date_of_sample_tested"
#'   ) |>
#'   as_epidist_aggregate_data() |>
#'   as_epidist_naive_model() |>
#'   epidist(chains = 2, cores = 2, refresh = ifelse(interactive(), 250, 0))
#' epidist_diagnostics(fit)
epidist_diagnostics <- function(fit) {
  if (!inherits(fit, "epidist_fit")) {
    cli_abort(c(
      "!" = "Diagnostics only supported for objects of class epidist_fit"
    ))
  }
  if (fit$algorithm %in% c("laplace", "meanfield", "fullrank", "pathfinder")) {
    cli_abort(c(
      "!" = paste0(
        "Diagnostics not yet supported for the algorithm: ", fit$algorithm
      )
    ))
  }
  if (fit$algorithm == "sampling") {
    np <- brms::nuts_params(fit)
    divergent_ind <- np$Parameter == "divergent__"
    treedepth_ind <- np$Parameter == "treedepth__"
    diagnostics <- dplyr::tibble(
      time = sum(rstan::get_elapsed_time(fit$fit)),
      samples = nrow(np) / length(unique(np$Parameter)),
      max_rhat = round(max(brms::rhat(fit), na.rm = TRUE), 3),
      divergent_transitions = sum(np[divergent_ind, ]$Value),
      per_divergent_transitions = mean(np[divergent_ind, ]$Value),
      max_treedepth = max(np[treedepth_ind, ]$Value)
    ) |>
      mutate(
        no_at_max_treedepth =
          sum(np[treedepth_ind, ]$Value == .data$max_treedepth),
        per_at_max_treedepth = .data$no_at_max_treedepth / samples
      )
  } else {
    cli_abort(c(
      "!" = paste0("Unrecognised algorithm: ", fit$algorithm)
    ))
  }
  return(diagnostics)
}
