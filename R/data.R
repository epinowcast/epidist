#' Ebola linelist data from Fang et al. (2016)
#'
#' Linelist data for the Ebola virus collected in Sierra Leone. If you use this
#' data in your work, please cite the corresponding paper.
#'
#' @format A `tibble` with 8,358 rows and 8 columns:
#' \describe{
#'   \item{id}{Unique identification number for the case}
#'   \item{name}{Name as character, omitted}
#'   \item{age}{Age as numeric}
#'   \item{sex}{Sex as character, either "F", "M" or NA}
#'   \item{date_of_symptom_onset}{The date symptoms began}
#'   \item{date_of_sample_tested}{The date the sample was tested}
#'   \item{district}{The district (ADM2)}
#'   \item{chiefdom}{The chiefdom (ADM3)}
#' }
#' @family data
#' @source <https://www.pnas.org/doi/full/10.1073/pnas.1518587113>
"sierra_leone_ebola_outbreak_data"
