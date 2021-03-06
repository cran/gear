% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update.geolm.R, R/update.geolm_cmodMan.R,
%   R/update.geolm_cmodStd.R
\name{update.geolm}
\alias{update.geolm}
\alias{update.geolm_cmodMan}
\alias{update.geolm_cmodStd}
\title{Update linear model for geostatistical data}
\usage{
\method{update}{geolm}(object, mod, ...)

\method{update}{geolm_cmodMan}(object, mod, ...)

\method{update}{geolm_cmodStd}(object, mod, ...)
}
\arguments{
\item{object}{An object produced by the \code{geolm}
function.}

\item{mod}{A spatial dependence model object obtained
from one of the \code{cmod_*} functions or the
\code{estimate} function.}

\item{...}{Not implemented.}
}
\value{
Returns an object of the same class as
  \code{object}.
}
\description{
\code{update} updates a geostatistical linear model based
on the given model.
}
\examples{
# generate response
y = rnorm(10)
# generate coordinates
x1 = runif(10); x2 = runif(10)

# data frame for observed data
data = data.frame(y, x1, x2)
coords = cbind(x1, x2)
psill = 2 # partial sill
r = 4 # range parameter
evar = .1 # error variance
fvar = .1 # add finescale variance
# one can't generally distinguish between evar and fvar, but
# this is done for illustration purposes

cmod_std = cmod_std("exponential", psill = psill, r = r,
                    evar = evar, fvar = fvar)

cmod_std2 = cmod_std("exponential", psill = psill + 1,
                     r = r + .5, evar = evar + .01,
                     fvar = fvar)

# check geolm update for universal kriging
gear1 = geolm(y ~ x1 + x2, data = data, mod = cmod_std,
              coordnames = c("x1", "x2"))

gear2 = geolm(y ~ x1 + x2, data = data, mod = cmod_std2,
              coordnames = c("x1", "x2"))
gear2b = update(gear1, cmod_std2)
gear2$call = NULL
gear2b$call = NULL
identical(gear2, gear2b)
}
\seealso{
\code{\link[stats]{update}}
}
\author{
Joshua French
}
