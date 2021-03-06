% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/robsurvey.R
\name{rht_control}
\alias{rht_control}
\title{Control function for M-estimation (tuning parameters etc.)}
\usage{
rht_control(acc = 1e-05, maxit = 100, psi = "Huber", ...)
}
\arguments{
\item{acc}{numeric tolerance, stoping rule in the iterative
updating scheme (default: \code{1e-5})}

\item{maxit}{maximum number of updating iterations}

\item{psi}{psi-function (\code{Huber} or \code{asymHuber})}

\item{...}{additional arguments}
}
\value{
List
}
\description{
This function is called internally.
}
\details{
Tuning parameters for \code{\link{weighted_mean_huber}},
\code{\link{weighted_total_huber}}, \code{\link{svymean_huber}},
\code{\link{svytotal_huber}}.
}
