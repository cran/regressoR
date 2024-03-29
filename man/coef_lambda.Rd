% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_penalized_Regression_utils.R
\name{coef_lambda}
\alias{coef_lambda}
\title{coef_lambda}
\usage{
coef_lambda(data, variable.pred, model, log.lambda = NULL)
}
\arguments{
\item{data}{dataframe}

\item{variable.pred}{the name of the variable to be predicted.}

\item{model}{a penalized regression model(cv.glmnet).}

\item{log.lambda}{numerical. Logarithm of lambda in case you don't want to use the optimal lambda.}
}
\description{
get penalized regression coefficients.
}
