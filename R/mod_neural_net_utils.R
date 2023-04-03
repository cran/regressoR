#' nn_plot
#'
#' @description graph of the neural network.
#'
#' @param model a neural network model(neuralnet)
#'
#' @export
#'
nn_plot <- function(model){
  plot(model, arrow.length = 0.1, rep = 'best', intercept = TRUE,x.entry = 0.1, x.out = 0.9,
       information=FALSE,intercept.factor = 0.8,col.entry.synapse='red',col.entry='red',
       col.out='green',col.out.synapse='green', dimension=15, radius = 0.2, fontsize = 10)
}

