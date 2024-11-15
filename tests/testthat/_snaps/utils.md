# .replace_prior errors when passed a new prior without a match in old_prior [plain]

    Code
      .replace_prior(old_prior, new_prior, warn = TRUE)
    Condition
      Warning:
      ! One or more priors have no match in existing parameters:
      Intercept_shape ~ normal(0, 5)
      i To remove this warning consider changing prior specification.
    Output
              prior     class coef group resp  dpar nlpar   lb   ub source
       normal(0, 5) Intercept                             <NA> <NA>   user
       normal(0, 5) Intercept                 sigma       <NA> <NA>   user

# .replace_prior errors when passed a new prior without a match in old_prior [ansi]

    Code
      .replace_prior(old_prior, new_prior, warn = TRUE)
    Condition
      [1m[33mWarning[39m:[22m
      [1m[22m[33m![39m One or more priors have no match in existing parameters:
      Intercept_shape ~ normal(0, 5)
      [36mi[39m To remove this warning consider changing prior specification.
    Output
              prior     class coef group resp  dpar nlpar   lb   ub source
       normal(0, 5) Intercept                             <NA> <NA>   user
       normal(0, 5) Intercept                 sigma       <NA> <NA>   user

# .replace_prior errors when passed a new prior without a match in old_prior [unicode]

    Code
      .replace_prior(old_prior, new_prior, warn = TRUE)
    Condition
      Warning:
      ! One or more priors have no match in existing parameters:
      Intercept_shape ~ normal(0, 5)
      ℹ To remove this warning consider changing prior specification.
    Output
              prior     class coef group resp  dpar nlpar   lb   ub source
       normal(0, 5) Intercept                             <NA> <NA>   user
       normal(0, 5) Intercept                 sigma       <NA> <NA>   user

# .replace_prior errors when passed a new prior without a match in old_prior [fancy]

    Code
      .replace_prior(old_prior, new_prior, warn = TRUE)
    Condition
      [1m[33mWarning[39m:[22m
      [1m[22m[33m![39m One or more priors have no match in existing parameters:
      Intercept_shape ~ normal(0, 5)
      [36mℹ[39m To remove this warning consider changing prior specification.
    Output
              prior     class coef group resp  dpar nlpar   lb   ub source
       normal(0, 5) Intercept                             <NA> <NA>   user
       normal(0, 5) Intercept                 sigma       <NA> <NA>   user
