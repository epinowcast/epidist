% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marginal_model.R
\name{as_epidist_marginal_model.epidist_linelist_data}
\alias{as_epidist_marginal_model.epidist_linelist_data}
\title{The marginal model method for \code{epidist_linelist_data} objects}
\usage{
\method{as_epidist_marginal_model}{epidist_linelist_data}(data, obs_time_threshold = 2, ...)
}
\arguments{
\item{data}{An \code{epidist_linelist_data} object}

\item{obs_time_threshold}{Ratio used to determine threshold for setting
relative observation times to Inf. Observation times greater than
\code{obs_time_threshold} times the maximum delay will be set to Inf to improve
model efficiency by reducing the number of unique observation times.
Default is 2.}

\item{...}{Not used in this method.}
}
\description{
The marginal model method for \code{epidist_linelist_data} objects
}
\seealso{
Other marginal_model: 
\code{\link{as_epidist_marginal_model}()},
\code{\link{epidist_family_model.epidist_marginal_model}()},
\code{\link{epidist_formula_model.epidist_marginal_model}()},
\code{\link{is_epidist_marginal_model}()},
\code{\link{new_epidist_marginal_model}()}
}
\concept{marginal_model}
