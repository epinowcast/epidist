#' Fit epidemiological delay distributions using a `brms` interface
#'
#' @param data An object with class corresponding to an implemented model.
#' @param formula An object of class [stats::formula] or [brms::brmsformula]
#' (or one that can be coerced to those classes). A symbolic description of the
#' model to be fitted. A formula must be provided for the distributional
#' parameter `mu`, and may optionally be provided for other distributional
#' parameters.
#' @param family A description of the response distribution and link function to
#' be used in the model. Every family function has a link argument allowing
#' users to specify the link function to be applied on the response variable.
#' If not specified, default links are used. For details of all supported
#' families see [brmsfamily()]. Commonly used, such as [lognormal()], are also
#' reexported as part of `epidist`.
#' @param prior One or more `brmsprior` objects created by [brms::set_prior()]
#' or related functions. These priors are passed to [epidist_prior()] in the
#' `prior` argument. Some models have default priors that are automatically
#' added (see [epidist_model_prior()]). These can be merged with user-provided
#' priors using the `merge_priors` argument.
#' @param merge_priors If `TRUE` then merge user priors with default priors, if
#' `FALSE` only use user priors. Defaults to `TRUE`. This may be useful if
#' the built in approaches for merging priors are not flexible enough for a
#' particular use case.
#' @param fn The internal function to be called. By default this is
#' [brms::brm()] which performs inference for the specified model. Other options
#' are [brms::make_stancode()] which returns the Stan code for the specified
#' model, or [brms::make_standata()] which returns the data passed to Stan.
#' These two later options may be useful for model debugging and extensions.
#' @param ... Additional arguments passed to `fn` method.
#' @family fit
#' @export
epidist <- function(data, formula = mu ~ 1,
                    family = lognormal(), prior = NULL,
                    merge_priors = TRUE,
                    fn = brms::brm, ...) {
  assert_epidist(data)
  epidist_family <- epidist_family(data, family)
  epidist_formula <- epidist_formula(
    data = data, family = epidist_family, formula = formula
  )
  trans_data <- epidist_transform_data(data, epidist_family, epidist_formula)
  epidist_prior <- epidist_prior(
    data = trans_data, family = epidist_family,
    formula = epidist_formula, prior,
    merge = merge_priors
  )
  epidist_stancode <- epidist_stancode(
    data = trans_data, family = epidist_family, formula = epidist_formula
  )
  fit <- fn(
    formula = epidist_formula, family = epidist_family, prior = epidist_prior,
    stanvars = epidist_stancode, data = trans_data, ...
  )
  class(fit) <- c(class(fit), "epidist_fit")
  return(fit)
}
