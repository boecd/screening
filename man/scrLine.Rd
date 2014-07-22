\name{scrLine}
\alias{scrLine}
\title{Screening: Lines}
\usage{
  scrLine(data = stop("'data' must be specified"),
    isic = 4, sources = NULL, by = "var", namevar = NULL,
    nameind = NULL, nameyear = NULL, display = TRUE)
}
\arguments{
  \item{cou}{a 3-digit country ISO code character string.}

  \item{isic}{an integer to specify the version of STAN.}

  \item{sources}{a list naming the sources to plot.}

  \item{namevar}{a character string naming the variables to
  plot.}

  \item{nameind}{a character string naming the industries
  to plot.}

  \item{nameyear}{a numeric vector with years to plot.}

  \item{display}{a logical to open the output file in a web
  browser.}
}
\description{
  Line plots for data screening
}
\details{
  Create line plots for data screening.
}
\examples{
data <- data.frame(sou=c(rep('UNSDSNA', 6)),
var=c(rep('EXPO', 6)),
ind=c(rep('CTOTAL', 6)),
year=c(1970:1975),
value=c(1.506e+11, 2.357e+11, 2.924e+11, 6.278e+11, 1.445e+12, 1.408e+12))
scrLine(data=data, isic=3, namevar='EXPO', nameind='CTOTAL', display=TRUE)
}
\author{
  OECD STAN
}
\seealso{
  \code{\link{detrendScale}}
}
\keyword{plot}

