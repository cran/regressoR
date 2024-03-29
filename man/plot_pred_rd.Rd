% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_dimension_reduction_utils.R
\name{plot_pred_rd}
\alias{plot_pred_rd}
\title{plot_pred_rd}
\usage{
plot_pred_rd(
  model,
  n.comp,
  titles = c("Varianza Explicada en Predictores", "Numero de Componentes",
    "Porcentaje de Varianza Explicada")
)
}
\arguments{
\item{model}{a dimension reduction model.}

\item{n.comp}{the optimum number of components.}

\item{titles}{labels on the chart}
}
\value{
echarts4r plot
}
\description{
graph of variance explained in the predictors according to components used.
}
\author{
Ariel Arroyo <luis.ariel.arroyo@promidat.com>
}
