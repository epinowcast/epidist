% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/family.R
\name{epidist_family_model}
\alias{epidist_family_model}
\title{The model-specific parts of an \code{epidist_family()} call}
\usage{
epidist_family_model(data, family, ...)
}
\arguments{
\item{data}{A \code{data.frame} containing line list data}

\item{family}{Output of a call to \code{brms::brmsfamily()} with additional
information as provided by \code{.add_dpar_info()}}

\item{...}{Additional arguments passed to method.}
}
\description{
The model-specific parts of an \code{epidist_family()} call
}
\seealso{
Other family: 
\code{\link{epidist_family}()},
\code{\link{epidist_family_model.default}()},
\code{\link{epidist_family_reparam}()},
\code{\link{epidist_family_reparam.default}()},
\code{\link{epidist_family_reparam.gamma}()}
}
\concept{family}
