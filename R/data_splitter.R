#' @title Split a Survival Data Set
#'
#' @description
#' This function reshapes data to a long format by splitting each record into
#' multiple subrecords at intervals of length 1. The resulting dataset will have
#' a new column 'tstart' and 'tend' with event status and treatment status for
#' each interval
#'
#' @param data a data frame
#' @param id column with unique patient identifier (defaults to id)
#' @param fup column with time of follow-up (defaults to fup)
#' @param status column with the status indicator (defaults to status)
#' @param treatment column with the treatment indicator
#' @param tt_treatment column with time to treatment
#'
#' @return \code{data_splitter} returns the splitted data in a tibble
#'
#' @author Linda Nab, \email{lindanab4@gmail.com}
#'
#' @examples
#' data(paxlovid)
#' data_splitter(data = paxlovid,
#'               treatment = A,
#'               tt_treatment = tt_A)
#'
#' data <- data_simulator(n = 500, n_visit = 5)
#' data_splitter(data = data,
#'               treatment = A,
#'               tt_treatment = tt_A)
#' @export
data_splitter <- function(
    data,
    id = id,
    fup = fup,
    status = status,
    treatment,
    tt_treatment
){
  data %>%
    dplyr::group_by({{ id }}) %>%
    tidyr::uncount({{ fup }}, .remove = FALSE) %>%
    dplyr::mutate(tend = dplyr::row_number(),
                  tstart = tend - 1L,
                  "{{ status }}" := dplyr::if_else(tend == {{ fup }}, {{ status }}, 0L),
                  "{{ treatment }}" := dplyr::if_else(tend == {{ tt_treatment }} | tend > {{ tt_treatment }}, {{ treatment }}, 0L)) %>%
    dplyr::select(- c({{ fup }}, {{ tt_treatment }})) %>%
    dplyr::relocate({{ id }}, tstart, tend) %>%
    dplyr::ungroup()
}
