% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latent_model.R
\name{as_epidist_latent_model.epidist_linelist_data}
\alias{as_epidist_latent_model.epidist_linelist_data}
\title{The latent model method for \code{epidist_linelist_data} objects}
\usage{
\method{as_epidist_latent_model}{epidist_linelist_data}(data, ...)
}
\arguments{
\item{data}{An \code{epidist_linelist_data} object containing individual-level
observations with primary and secondary event times. See
\code{\link[=as_epidist_linelist_data]{as_epidist_linelist_data()}} for details on creating this object.}

\item{...}{Not used in this method.}
}
\description{
This method takes an \code{epidist_linelist_data} object and converts it to a
format suitable for fitting latent variable models. It calculates key
variables needed for the latent variable method described in Park et al.
(2024) and Charniga et al. (2024). This approach adjusts for double
interval censoring and right truncation in the data.
}
\examples{
sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ) |>
  as_epidist_latent_model()
}
\references{
\itemize{
\item \href{https://doi.org/10.1101/2024.01.12.24301247}{Park et al. (2024)}
\item \href{https://doi.org/10.1371/journal.pcbi.1012520}{Charniga et al. (2024)}
}
}
\seealso{
Other latent_model: 
\code{\link{as_epidist_latent_model}()},
\code{\link{as_epidist_latent_model.epidist_aggregate_data}()},
\code{\link{epidist_family_model.epidist_latent_model}()},
\code{\link{epidist_formula_model.epidist_latent_model}()},
\code{\link{epidist_model_prior.epidist_latent_model}()},
\code{\link{is_epidist_latent_model}()},
\code{\link{new_epidist_latent_model}()}
}
\concept{latent_model}
