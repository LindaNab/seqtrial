#' Paxlovid data
#'
#' Follow-up time, event status indicator, treatment status indicator, time to
#' treatment and sex of individuals in a study investigating the effectiveness
#' of paxlovid use versus no-use.
#'
#' This is a dummy data set
#'
#' @format A data frame with 2 rows and 6 variables:
#' \describe{
#'   \item{id}{Unique patient identifier}
#'   \item{fup}{Days of follow-up}
#'   \item{status}{Event status indicator}
#'   \item{A}{Treatment indicator}
#'   \item{tt_A}{Time to treatment (day at which treatment is initiated)}
#'   \item{L}{Sex}
#' }
#' @examples
#' data("paxlovid", package = "seqtrial")
"paxlovid"
