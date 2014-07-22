\name{detrendScale}
\alias{detrendScale}
\title{Detrend Scale}
\usage{
  detrendScale(x, var, name = "newVar", vis = TRUE,
    nameind, plotcol = 6)
}
\arguments{
  \item{x}{a dataframe with columns "variable" and
  "ISICcombination".}

  \item{variable}{a character string specifying a member of
  "variable" to be checked.}

  \item{name}{a character string for the new variable
  name.}

  \item{vis}{logical to display plot.}

  \item{codesISIC}{a vector of industry codes in
  "ISICcombination".}
}
\description{
  Screening: detrend and scale
}
\details{
  Outlier detection using de-trending and scaling of the
  time series for all non-time dimension combination
  categories.
}
\examples{
x <- detrendScale(x, "VAEMP", "VAEMPdiff", nameind = unique(x$ind))
}
\author{
  OECD STAN
}
\seealso{
  nothing
}
\keyword{Screening}

