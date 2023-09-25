#' @title Construct a Sequence of Trials
#'
#' @description
#' Construct a sequence of trials
#'
#' @param data_splitted a data frame that has been split using data_splitter()
#' @param n number of trials to construct, starting at baseline, n => 1
#' @param id column with unique patient identifier (defaults to id)
#' @param status column with the status indicator (defaults to status)
#' @param treatment column with the treatment indicator
#' @param censor boolean indicating if individuals are censored when they
#' deviate from their baseline treatment
#'
#' @return \code{trial_constructor} returns a data frame in which the sequence
#' of trials has been contructed
#'
#' @author Linda Nab, \email{lindanab4@gmail.com}
#'
#' @examples
#' # split data
#' data(paxlovid)
#' paxlovid_splitted <-
#'   data_splitter(data = paxlovid,
#'                 treatment = A,
#'                 tt_treatment = tt_A)
#' # construct trials
#' trial_constructor(data = paxlovid_splitted,
#'                   n = 5,
#'                   treatment = A)
#'
#' # same using simulated data
#' data <- data_simulator(n = 500, n_visit = 5)
#' data_splitted <-
#'   data_splitter(data = data,
#'                 treatment = A,
#'                 tt_treatment = tt_A)
#' data_splitted <-
#'   data_splitted %>%
#'   dplyr::mutate(
#'     L = dplyr::case_when(tstart == 0 ~ L0,
#'                          tstart == 1 ~ L1,
#'                          tstart == 2 ~ L2,
#'                          tstart == 3 ~ L3,
#'                          tstart == 4 ~ L4)) %>%
#'   dplyr::select(-dplyr::starts_with("L"))
#' trial_constructor(data = data_splitted,
#'                   n = 5,
#'                   treatment = A)
#' @export
trial_constructor <- function(
    data_splitted,
    n,
    id = id,
    status = status,
    treatment,
    censor = FALSE # artifical censoring if patient deviates from baseline treatment
){
  seq_trials <-
    purrr::map_dfr(
      .x = 0:{n - 1},
      .f = ~ construct_trial_n(data_splitted, .x, {{ id }}, {{ treatment }}, {{ status }})
      )
  if (censor == TRUE) {
    seq_trials <-
      seq_trials %>%
      dplyr::group_by({{ id }}, trial) %>%
      dplyr::filter(!(arm == 0 & {{ treatment }} == 1)) %>%
      dplyr::ungroup()
  }
  seq_trials
}
construct_trial_n <- function(data_splitted,
                              n, # day of start, 0 is the trial that starts at baseline, 1 is the trial that starts one day after baseline etc.
                              id, # id var
                              treatment, # treatment var
                              status # status var
){
  data_splitted %>%
    dplyr::filter(tstart >= n) %>%
    dplyr::mutate(trial = n + 1) %>%
    dplyr::group_by({{ id }}) %>%
    dplyr::mutate(
      arm = dplyr::first({{ treatment }}),
      treatment_lag1 = dplyr::lag({{ treatment }}, n = 1, default = 0),
      treatment_lag1_baseline = dplyr::first(treatment_lag1),
      status_baseline = dplyr::first({{ status }}),
      tstart = tstart - n,
      tend = tend - n) %>%
    dplyr::filter(treatment_lag1_baseline == 0,
                  status_baseline == 0) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(status_baseline, treatment_lag1_baseline, treatment_lag1)) %>%
    #dplyr::rename("{{ treatment }}_lag1" := treatment_lag1) %>%
    dplyr::relocate({{ id }}, trial, arm)
}
