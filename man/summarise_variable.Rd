% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postprocess.R
\name{summarise_variable}
\alias{summarise_variable}
\title{Summarise a variable}
\usage{
summarise_variable(draws, variable, sf = 6, by = c())
}
\arguments{
\item{draws}{A data.table of posterior draws}

\item{variable}{The variable to summarise}

\item{sf}{The number of significant figures to use}

\item{by}{A vector of columns to group by}
}
\description{
Summarise a variable
}
\seealso{
Other postprocess: 
\code{\link{add_mean_sd}()},
\code{\link{add_mean_sd.default}()},
\code{\link{add_mean_sd.gamma_samples}()},
\code{\link{add_mean_sd.lognormal_samples}()},
\code{\link{draws_to_long}()},
\code{\link{make_relative_to_truth}()},
\code{\link{predict_delay_parameters}()},
\code{\link{summarise_draws}()}
}
\concept{postprocess}
