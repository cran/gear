% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evgram-omni_semivariogram.R
\name{omni_semivariogram}
\alias{omni_semivariogram}
\title{Compute an omnidirectional empirical semivariogram}
\usage{
omni_semivariogram(d, diff, cuts, npmin, type)
}
\arguments{
\item{d}{Vector of pairwise distances}

\item{diff}{Vector of pairwise response differences}

\item{cuts}{Cut points for bins}

\item{npmin}{Minimum pairs of differences}

\item{type}{Type of empirical semivariogram}
}
\value{
A data.frame with the computed omnidirectional semivariogram
}
\description{
Compute an omnidirectional empirical semivariogram
}
\keyword{internal}
