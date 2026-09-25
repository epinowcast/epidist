# Helpers for evaluating the Stan meta model log likelihood outside a fit.
#
# Exposing the Stan functions to R cannot be relied on for the meta model, so
# the Stan log likelihood is evaluated through a fixed parameter generated
# quantities program that calls meta_lognormal_lpmf with the parameters
# passed as data, one row of the model data per column and one parameter
# draw per row.

meta_slot_names <- function() {
  return(c(paste0("vint", 1:10), paste0("vreal", 1:8)))
}

# Compile the generated quantities program for a meta model, lognormal by
# default. A model whose summary rows estimate their growth rate carries
# pgrowth as a further parameter, passed as data alongside the delay
# distribution parameters.
meta_log_lik_program <- function(meta, family = lognormal()) {
  meta_family <- epidist_family(meta, family = family)
  meta_formula <- epidist_formula(meta, meta_family, formula = bf(mu ~ 1))
  stanvars <- epidist_stancode(
    meta,
    family = meta_family, formula = meta_formula
  )
  standata <- suppressMessages(
    epidist(meta, family = family, fn = brms::make_standata)
  )
  slots <- meta_slot_names()
  growth <- "pgrowth" %in% meta_family$dpars
  delay_dpars <- setdiff(meta_family$dpars, "pgrowth")
  np_functions <- ""
  if (.is_nonparametric(meta_family)) {
    np_functions <- .np_stanvars(meta_family)[[1]]$scode
  }
  mod <- rstan::stan_model(model_code = paste0(
    "functions {\n", stanvars[[3]]$scode, "\n", stanvars[[2]]$scode, "\n",
    np_functions, "\n}\n",
    "data {\n  int N;\n  array[N] int Y;\n",
    paste0("  array[N] int ", slots[1:10], ";\n", collapse = ""),
    paste0("  array[N] real ", slots[11:18], ";\n", collapse = ""),
    "  int<lower=0> N_meta_group;\n",
    "  vector[N_meta_group] meta_group_value;\n",
    "  array[N_meta_group] int meta_group_count;\n",
    "  array[N_meta_group] int meta_group_lower;\n",
    "  array[N_meta_group] int meta_group_type;\n",
    "  vector[N_meta_group] meta_group_p;\n",
    "  int<lower=0> N_meta_chol;\n",
    "  vector[N_meta_chol] meta_group_chol;\n",
    "  int D;\n",
    paste0("  array[D] real ", delay_dpars, ";\n", collapse = ""),
    ifelse(growth, "  array[D] real pgrowth;\n", ""),
    "}\n",
    "generated quantities {\n  array[0] real primary_params;\n",
    "  matrix[D, N] log_lik;\n  for (d in 1:D) {\n    for (n in 1:N) {\n",
    "      log_lik[d, n] = ", meta_family$name, "_lpmf(Y[n] | ",
    paste0(delay_dpars, "[d], ", collapse = ""),
    ifelse(growth, "pgrowth[d], ", ""),
    paste0(slots, "[n]", collapse = ", "),
    ", meta_group_value, meta_group_count, meta_group_lower, meta_group_type",
    ", meta_group_p",
    ", meta_group_chol, primary_params);\n    }\n  }\n}\n"
  ))
  stan_data <- c(
    list(N = length(standata$Y), Y = as.integer(standata$Y)),
    lapply(standata[slots[1:10]], as.integer),
    lapply(standata[slots[11:18]], as.numeric),
    list(
      N_meta_group = standata$N_meta_group,
      meta_group_value = as.array(standata$meta_group_value),
      meta_group_count = as.array(standata$meta_group_count),
      meta_group_lower = as.array(standata$meta_group_lower),
      meta_group_type = as.array(standata$meta_group_type),
      meta_group_p = as.array(standata$meta_group_p),
      N_meta_chol = standata$N_meta_chol,
      meta_group_chol = as.array(standata$meta_group_chol)
    )
  )
  return(list(
    mod = mod, data = stan_data, standata = standata, growth = growth,
    family = meta_family
  ))
}

# The Stan log likelihood, one row per draw and one column per model row.
# pgrowth is the growth rate of the rows that estimate it, one per draw, and
# is only read where the program carries it. `dpars` holds the delay
# distribution parameters, one vector of draws each, and defaults to the
# lognormal `mu` and `sigma`.
meta_stan_log_lik <- function(program, mu, sigma, pgrowth = 0,
                              dpars = list(mu = mu, sigma = sigma)) {
  draws_n <- length(dpars[[1]])
  growth <- list()
  if (program$growth) {
    growth <- list(pgrowth = as.array(rep_len(pgrowth, draws_n)))
  }
  fit <- rstan::sampling(
    program$mod,
    data = c(
      program$data,
      list(D = draws_n),
      lapply(dpars, as.array),
      growth
    ),
    algorithm = "Fixed_param", chains = 1, iter = 1, warmup = 0, refresh = 0
  )
  draws <- posterior::as_draws_matrix(fit)
  n <- program$data$N
  return(t(vapply(
    seq_len(draws_n),
    function(d) {
      return(as.numeric(draws[1, paste0("log_lik[", d, ",", seq_len(n), "]")]))
    },
    numeric(n)
  )))
}

# Every element within a relative tolerance, scaled by at least one so that
# a log likelihood near zero is compared absolutely.
expect_rows_close <- function(actual, expected, tolerance) {
  gap <- abs(actual - expected) / pmax(abs(expected), 1)
  return(testthat::expect_lt(max(gap), tolerance))
}

# The R log likelihood on the same layout. The prep is a brmsprep in name
# only, holding pgrowth as a draw by row matrix so that .meta_row_slots()
# can read it through brms::get_dpar().
meta_r_log_lik <- function(program, mu, sigma, pgrowth = 0,
                           dpars = list(mu = mu, sigma = sigma)) {
  n <- program$data$N
  draws_n <- length(dpars[[1]])
  pgrowth <- rep_len(pgrowth, draws_n)
  np <- program$family$np
  return(t(vapply(
    seq_len(draws_n),
    function(d) {
      prep <- structure(
        list(
          data = program$data, ndraws = 1,
          dpars = list(pgrowth = matrix(pgrowth[d], nrow = 1, ncol = n))
        ),
        class = "brmsprep"
      )
      draw <- lapply(dpars, `[`, d)
      if (is.null(np)) {
        dist <- "plnorm"
        dist_args <- list(meanlog = draw$mu, sdlog = draw$sigma)
      } else {
        dist <- "pdiscretehazard"
        dist_args <- list(
          boundaries = np$boundaries,
          hazards = as.vector(.np_hazards(draw, np$hazard_model))
        )
      }
      return(vapply(
        seq_len(n),
        function(i) {
          return(.meta_row_log_lik(
            .meta_row_slots(i, prep), dist, dist_args
          ))
        },
        numeric(1)
      ))
    },
    numeric(n)
  )))
}
