% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_penalized_Regression_utils.R
\name{rlr_model}
\alias{rlr_model}
\title{rlr_model}
\usage{
rlr_model(data, variable.pred, alpha = 0, standardize = TRUE)
}
\arguments{
\item{data}{dataframe}

\item{variable.pred}{the name of the variable to be predicted.}

\item{alpha}{the alpha parameter of the model.}

\item{standardize}{the standardize parameter of the model.}
}
\description{
generates a penalized regression model.
}
\seealso{
\code{\link[glmnet]{glmnet}}, \code{\link[glmnet]{cv.glmnet}}
}
