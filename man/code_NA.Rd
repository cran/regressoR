% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_generate.R
\name{code_NA}
\alias{code_NA}
\title{code_NA}
\usage{
code_NA(deleteNA = TRUE, d.o = "datos.originales")
}
\arguments{
\item{deleteNA}{a logical value indicating whether the NAs have to be eliminated or whether they have to be imputed. If TRUE then the NAs are eliminated, otherwise the data is imputed.}

\item{d.o}{the name of the original data.}
}
\description{
creates the code that imputes the NAs data or removes them.
}
\examples{
iris2 <- iris
x <- code_NA(TRUE, 'iris2')
exe(x)
x <- code_NA(FALSE, 'iris2')
exe(x)

}
