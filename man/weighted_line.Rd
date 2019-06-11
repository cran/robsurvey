% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/robsurvey.R
\name{weighted_line}
\alias{weighted_line}
\title{Weighted robust line fitting}
\usage{
weighted_line(x, y = NULL, w, na.rm = FALSE, iter = 1)
}
\arguments{
\item{x}{a numeric vector (explanatory variable)}

\item{y}{a numeric vector (response variable)}

\item{w}{a numeric vector of weights}

\item{na.rm}{a logical value indicating whether rows with \code{NA}
values should be stripped before the computation proceeds}

\item{iter}{number of iterations for enhancing the slope}
}
\value{
intercept and slope of the fitted line
}
\description{
\code{weighted_line} fits a robust line and allows weights.
}
\details{
Uses different quantiles for splitting the sample than \code{line()}.
Is based on \code{weighted_median()}.
}
\examples{
data(cars)
weighted_line(cars$speed, cars$dist, w=rep(1, length(cars$speed)))
weighted_line(cars$speed, cars$dist, w=rep(1:10, each=5))
}
\seealso{
\code{\link[stats]{line}}
}