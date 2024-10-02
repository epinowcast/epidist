epidist_family.epidist_latent_individual <- function(data,
                                                     family = "lognormal",
                                                     ...) {
  epidist_validate(data)
  # allows use of stats::family and strings
  family <- brms:::validate_family(family)
  non_mu_links <- family[[paste0("link_", setdiff(family$dpars, "mu"))]]
  non_mu_bounds <- lapply(
    family$dpars[-1], brms:::dpar_bounds, family = family$family
  )
  custom_family <- brms::custom_family(
    paste0("latent_", family$family),
    dpars = family$dpars,
    links = c(family$link, non_mu_links),
    lb = c(NA, as.numeric(lapply(non_mu_bounds, "[[", "lb"))),
    ub = c(NA, as.numeric(lapply(non_mu_bounds, "[[", "ub"))),
    type = family$type,
    vars = c("pwindow", "swindow", "vreal1"),
    loop = FALSE
  )
  reparam <- family$dpars
  if (family$family == "gamma") {
    reparam <- c("shape", "shape ./ mu")
  }
  custom_family$reparam <- reparam
  return(custom_family)
}