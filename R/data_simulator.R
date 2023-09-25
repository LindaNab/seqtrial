#' @title Simulate Data
#'
#' @description
#' Simulate data following Keogh et al. [ref]
#'
#' @param n number of observations
#' @param n_visit number of visits, n >=1
#' @param gamma_0 parameter data generating mechanism
#' @param gamma_L parameter data generating mechanism
#' @param alpha_0 parameter data generating mechanism
#' @param alpha_A parameter data generating mechanism
#' @param alpha_L parameter data generating mechanism
#' @param alpha_U parameter data generating mechanism
#'
#' @return \code{data_simulator} returns a tibble with simulated data
#'
#' @author Linda Nab, \email{lindanab4@gmail.com}
#'
#' @examples
#' data_simulator(n = 500, n_visit = 5)
#' @importFrom rlang :=
#' @export
data_simulator <- function(
    n,
    n_visit,
    gamma_0 = -1,
    gamma_L = 0.5,
    alpha_0 = 0.2,
    alpha_A = -0.04,
    alpha_L = 0.015,
    alpha_U = 0.015
){
  # expit function
  expit <- function(x){exp(x) / (1 + exp(x))}
  # generate U, A, L
  A <- matrix(nrow = n, ncol = n_visit)
  L <- matrix(nrow = n, ncol = n_visit)
  U <- stats::rnorm(n, 0, 0.1)
  L[,1] <- stats::rnorm(n, U, 1)
  A[,1] <- stats::rbinom(n, 1, expit(gamma_0 + gamma_L * L[,1]))
  for (k in 2:n_visit){
    L[,k] <- stats::rnorm(n, 0.8*L[,k - 1] - A[,k - 1] + 0.1*(k - 1) + U, 1)
    A[,k] <- ifelse(A[,k - 1] == 1, 1, stats::rbinom(n, 1, expit(gamma_0 + gamma_L * L[,k])))
  }
  # generate event times fup, and event indicator 'status'
  fup <- rep(NA, n)
  for (k in 1:n_visit){
    u_t <- stats::runif(n, 0, 1)
    haz <- alpha_0 + alpha_A * A[,k] + alpha_L * L[,k] + alpha_U * U
    new_t <- -log(u_t) / haz
    fup <- ifelse(is.na(fup) & new_t < 1 & haz > 0, k - 1 + new_t, fup) # the haz>0 is just used to deal with tiny possibility (under this data generating mechanism) the hazard could go negative
  }
  status <- ifelse(is.na(fup), 0L, 1L)
  fup <- ifelse(is.na(fup), n_visit, fup) # max number of visits if NA
  fup <- ceiling(fup) %>% as.integer()
  A <- A %>% tibble::as_tibble() %>%
    dplyr::mutate(sum_A = rowSums(.),
                  A = dplyr::if_else(sum_A >= 1, 1L, 0L),
                  tt_A = dplyr::if_else(sum_A >= 1, as.integer(n_visit - sum_A), 5L)) %>%
    dplyr::select(A, tt_A)
  L <- tibble::as_tibble(L)
  colnames(L) <- paste0("L", 0:{n_visit - 1})
  data <-
    tibble::tibble(id = 1:n, fup, status, A, L)
}
