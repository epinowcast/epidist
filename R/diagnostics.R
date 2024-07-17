#' Diagnostics for fitted epidist model
#'
#' @family generics
#' @param fit ...
#' @export
epidist_diagnostics <- function(fit) {
  UseMethod("epidist")
}

#' Default method for returning diagnostics
#'
#' @param fit ...
#' @family defaults
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
#' @export
epidist_diagnostics <- function(fit) {
  # diag <- fit$sampler_diagnostics(format = "df")
  # diagnostics <- data.table(
  #   samples = nrow(diag),
  #   max_rhat = round(max(
  #     fit$summary(
  #       variables = NULL, posterior::rhat,
  #       .args = list(na.rm = TRUE)
  #     )$`posterior::rhat`,
  #     na.rm = TRUE
  #   ), 2),
  #   divergent_transitions = sum(diag$divergent__),
  #   per_divergent_transitions = sum(diag$divergent__) / nrow(diag),
  #   max_treedepth = max(diag$treedepth__)
  # )
  # diagnostics[, no_at_max_treedepth := sum(diag$treedepth__ == max_treedepth)]
  # diagnostics[, per_at_max_treedepth := no_at_max_treedepth / nrow(diag)]
  
  # timing <- round(fit$time()$total, 1)
}
