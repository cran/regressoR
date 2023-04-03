
#' dt_plot
#' 
#' @description makes the graph of the tree.
#'
#' @param model a decision trees model(rpart).
#'
#' @export
dt_plot <- function(model){
  if(!is.null(model)){
    return(rpart.plot::prp(model, type = 2, extra = 100, nn = TRUE, varlen = 0, 
                           faclen = 0, fallen.leaves = TRUE, branch.lty = 6, 
                           shadow.col = '#dedede',box.col = '#c8b028'))
  }
  return(NULL)
}

