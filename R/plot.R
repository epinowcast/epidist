#' Plot the relative difference between true values and posterior estimates
#' @export
plot_relative_recovery <- function(relative_data, alpha = 0.8,
                                   quantiles = c(0.05, 0.35, 0.65, 0.95), ...) {
  relative_data |>
    ggplot() +
    aes(x = rel_value, y = model, ...) +
    ggridges::geom_density_ridges(
      scale = 1.5, alpha = alpha, quantile_lines = TRUE,
      quantiles = quantiles
    ) +
    scale_x_continuous(
      trans = "log", limits = c(NA, NA),
      labels = ~ scales::comma(.x, accuracy = 0.1),
    ) +
    geom_vline(xintercept = 1, linetype = 2, size = 1.05, alpha = 0.8) +
    theme_bw() +
    labs(x = "Relative value")
}

#' Plot cases by observation window
#' @export
plot_cases_by_obs_window <- function(cases) {
  cases |>
    DT(case_type == "primary") |>
    ggplot() +
    aes(x = time, y = cases) +
    geom_col(aes(fill = factor(obs_at)), alpha = 1, col = "#696767b1") +
    geom_point(
      data = cases[case_type == "secondary"],
      aes(col = factor(obs_at))
    ) +
    geom_vline(
      aes(xintercept = as.numeric(as.character(obs_at))),
      linetype = 2, alpha = 0.9
    ) +
    scale_fill_brewer(palette = "Blues", direction = 1) +
    scale_color_brewer(palette = "Reds", direction = 1) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Days", y = "Cases") + 
    guides(fill = guide_legend(title = "Observation day", reverse = TRUE),
           color = guide_legend(title = "Observation day", reverse = TRUE))
}

#' Plot the empirical delay distribution
#' @export
plot_empirical_delay <- function(cases, meanlog, sdlog) {
  cases |>
    ggplot() +
    aes(x = delay_daily, fill = obs_at) +
    geom_histogram(
      aes(y = ..density..), binwidth = 1, position = "dodge",
      col = "#696767b1"
    ) +
    stat_function(
    fun = dlnorm, args = c(meanlog, sdlog), n = 100,
    col = "#696767b1", fill = NULL
    ) +
    scale_fill_brewer(palette = "Blues", direction = 1) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Days", y = "Density") +
    guides(fill = guide_legend(title = "Observation day", reverse = TRUE))
}

#' Plot the mean difference between continuous and discrete event time
#' @export
plot_censor_delay <- function(censor_delay) {
  ggplot(censor_delay) +
    geom_point(aes(cohort, mean))  +
    geom_errorbar(aes(cohort, ymin=lwr, ymax=upr), width=0) +
    facet_wrap(~type)  +
    theme_bw() +
    labs(
      x = "Cohort time (day)",
      y = "Mean different between continuous and discrete event time"
    )  
}

#' plot empirical cohort-based or cumulative mean vs posterior mean
#' @param fit fitted objects
#' @param data data used for object fitting
#' @param type type of mean to plot
#' @param truncate account for truncation?
#' @export
plot_posterior_pred_check <- function(fit,
                                      data,
                                      type = c("cohort", "cumulative"),
                                      truncate,
                                      obs_at) {
  type <- match.arg(type)
  
  gplot <- plot_cohort_mean(data, type)
  
  prange <- range(data$ptime_daily)
  pvec <- seq(prange[1], prange[2], by=1)
  
  ee <- extract_lognormal_draws(fit)
  
  if (truncate) {
    if (type == "cumulative") {
      stop("Don't use the truncate option with cumulative mean.")
    }
    
    if (missing(obs_at)) {
      ## FIXME: is this safe for more general usage? our data always have obs_at
      ## option 1: spit out a warning and take the maximum secondary event time
      ## the problem with option 1 is  that if someone's looking at old data..?
      obs_at <- unique(data$obs_at)
    }
    
    estmat <- matrix(NA, nrow=nrow(ee), ncol=length(pvec))
    
    ## calculate truncated mean
    message("Calculating truncated means...")
    for (j in 1:length(pvec)) {
      estvec <- rep(NA, nrow(ee))
      
      for (i in 1:nrow(ee)) {
          
        p <- pvec[j]
        
        numer <- integrate(
          function(x) {
            x * dlnorm(x, meanlog = ee$meanlog[i], sdlog = ee$sdlog[i])
          },
          lower=0, upper=obs_at - p
        )[[1]]
        
        denom <- integrate(
          function(x) {
            dlnorm(x, meanlog = ee$meanlog[i], sdlog = ee$sdlog[i])
          },
          lower=0, upper=obs_at - p
        )[[1]]
        
        estmat[i,j] <- numer / denom
      }
    }
  
    fitted <- data.table(
      pvec = pvec,
      mean = apply(estmat, 2, mean),
      lwr = apply(estmat, 2, quantile, 0.025),
      upr = apply(estmat, 2, quantile, 0.975)
    )
  } else {
    fitted <- data.table(
      pvec = pvec,
      mean = mean(ee$mean),
      lwr = quantile(ee$mean, 0.025),
      upr = quantile(ee$mean, 0.975)
    )  
  }
  
  gplot <- gplot +
    geom_ribbon(
      data = fitted, aes(pvec, ymin=lwr, ymax=upr), alpha=0.3,
      lty=2, col="black"
    ) +
    geom_line(data=fitted, aes(pvec, mean))

<<<<<<< HEAD
  return(gplot)
=======
  gplot
>>>>>>> 99d3fbca595c43943b32314f05ef78ff1f470650
}

#' Plot empirical cohort-based or cumulative mean 
#' @export
plot_cohort_mean <- function(data, type=c("cohort", "cumulative")) {
  
  out <- plot_cohort_mean_internal(data, type)
  
  gplot <- ggplot(out) +
    geom_point(aes(ptime_daily, mean, size=n), shape=1) +
    theme_bw() +
    scale_size("Number of samples") +
    labs(
      x = "Cohort time (day)",
      y = "Mean delay (days)"
    )  
  
<<<<<<< HEAD
  return(gplot)
=======
  gplot
>>>>>>> 99d3fbca595c43943b32314f05ef78ff1f470650
}

plot_cohort_mean_internal <- function(data, type=c("cohort", "cumulative")) {
  type <- match.arg(type)
  
  out <- data |>
    copy() |>
    DT(, .(mean = mean(delay_daily),
           n = .N), by = "ptime_daily") |>
    DT(order(rank(ptime_daily)))
  
  if (type=="cumulative") {
    out[, mean := cumsum(mean * n)/cumsum(n)]
    out[, n := cumsum(n)]
    
  }
  
  return(out[])
}
