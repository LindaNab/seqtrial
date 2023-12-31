% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_simulator.R
\name{data_simulator}
\alias{data_simulator}
\title{Simulate Data}
\usage{
data_simulator(
  n,
  n_visit,
  gamma_0 = -1,
  gamma_L = 0.5,
  alpha_0 = 0.2,
  alpha_A = -0.04,
  alpha_L = 0.015,
  alpha_U = 0.015
)
}
\arguments{
\item{n}{number of observations}

\item{n_visit}{number of visits, n >=1}

\item{gamma_0}{parameter data generating mechanism}

\item{gamma_L}{parameter data generating mechanism}

\item{alpha_0}{parameter data generating mechanism}

\item{alpha_A}{parameter data generating mechanism}

\item{alpha_L}{parameter data generating mechanism}

\item{alpha_U}{parameter data generating mechanism}
}
\value{
\code{sim_data} returns a tibble with simulated data
}
\description{
Simulate data following Keogh et al. [ref]
}
\examples{
data_simulator(n = 500, n_visit = 5)
}
\author{
Linda Nab, \email{lindanab4@gmail.com}
}
