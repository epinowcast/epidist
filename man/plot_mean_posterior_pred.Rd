% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_mean_posterior_pred}
\alias{plot_mean_posterior_pred}
\title{plot empirical cohort-based or cumulative mean vs posterior mean}
\usage{
plot_mean_posterior_pred(
  summarised_mean,
  obs_mean,
  alpha = 0.3,
  mean = FALSE,
  ribbon = TRUE,
  ribbon_bounds = c(0.05, 0.95),
  ...
)
}
\arguments{
\item{summarised_mean}{Summarised mean as produced by \code{\link[=summarise_variable]{summarise_variable()}}}

\item{obs_mean}{...}

\item{alpha}{...}

\item{mean}{Should the mean be plotted? Logical, defaults to \code{FALSE}.}

\item{ribbon}{Should the quantile ribbon be plotted? Logical, defaults to
\code{TRUE}.}

\item{ribbon_bounds}{Bounds of the quantile ribbon. Defaults to
\code{c(0.05, 0.95)} which corresponds to the 90\% credible interval.}

\item{...}{Additional arguments passed to \code{\link[ggplot2:aes]{ggplot2::aes()}}.}
}
\description{
plot empirical cohort-based or cumulative mean vs posterior mean
}
\seealso{
Other plot: 
\code{\link{calculate_cohort_mean}()},
\code{\link{calculate_truncated_means}()},
\code{\link{plot_cases_by_obs_window}()},
\code{\link{plot_censor_delay}()},
\code{\link{plot_cohort_mean}()},
\code{\link{plot_empirical_delay}()},
\code{\link{plot_recovery}()},
\code{\link{plot_relative_recovery}()}
}
\concept{plot}
