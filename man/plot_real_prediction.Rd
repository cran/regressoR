% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general_plots.R
\name{plot_real_prediction}
\alias{plot_real_prediction}
\title{plot_real_prediction}
\usage{
plot_real_prediction(
  real,
  prediction,
  model = "",
  titles = c("Predicciones vs Valores Reales", "Valor Real", "Prediccion")
)
}
\arguments{
\item{real}{the real values in traning-testing.}

\item{prediction}{the prediction values in traning-testing.}

\item{model}{the name of the model of the scatter plot.}

\item{titles}{Labels on the chart}
}
\value{
echarts4r plot
}
\description{
scatter plot between the actual value of the variable to be predicted and the prediction of the model.
}
\author{
Ariel Arroyo <luis.ariel.arroyo@promidat.com>
}
