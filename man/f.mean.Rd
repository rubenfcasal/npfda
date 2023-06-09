% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f.stats.R
\name{f.mean}
\alias{f.mean}
\alias{f.stats}
\alias{f.mean.npf.data}
\alias{f.var}
\alias{f.var.npf.data}
\title{Sample functional mean and variance}
\usage{
f.mean(x, ...)

\method{f.mean}{npf.data}(x, na.rm = TRUE, ...)

f.var(x, ...)

\method{f.var}{npf.data}(x, mean = f.mean(x, na.rm = na.rm), na.rm = TRUE, ...)
}
\arguments{
\item{x}{a functional data object.}

\item{...}{further arguments passed to or from other methods.}

\item{na.rm}{logical; indicating whether \code{NA} values should be ignored.}

\item{mean}{numeric; mean estimates at the discretization points.}
}
\value{
Return a vector with the corresponding estimates at the
discretization points.
}
\description{
\code{f.mean} computes the sample functional mean and \code{f.var} the sample
functional variance by default.
}
\details{
\code{f.var.npf.data()} computes the sample functional variance by default,
using the denominator \eqn{n - 1}
(alternatively, setting \code{mean = 0} may be appropriate for residuals or
\code{mean = predict(fit)}, for computing a residual functional variance).
}
\examples{
fd <- npf.data(ozone, dimnames = "day")
# Plot data + sample mean -+ sample std. dev.
plot(fd, col = "lightgray", legend = FALSE)
x <- coords(fd)
y <- f.mean(fd)
lines(x, y)
matlines(x, y + sqrt(f.var(fd, mean = y)) \%o\% c(-1, 1), col = 1, lty = 2)
}
