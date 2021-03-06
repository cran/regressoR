% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_generate.R
\name{default_calc_normal}
\alias{default_calc_normal}
\title{default_calc_normal}
\usage{
default_calc_normal(
  data = "datos",
  label.yes = "Positiva",
  label.no = "Negativa",
  label.without = "Sin Asimetría"
)
}
\arguments{
\item{data}{the name of the current data.}

\item{label.yes}{the label for when the asymmetry is positive.}

\item{label.no}{the label for when the asymmetry is negative.}

\item{label.without}{the label for when there is no asymmetry.}
}
\description{
generates the code that creates the asymmetry table.
}
\examples{
x <- default_calc_normal('iris')
exe(x)

}
