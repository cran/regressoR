#' dt_model
#' 
#' @description generates a decision trees model.
#'
#' @param data dataframe
#' @param variable.pred the name of the variable to be predicted.
#' @param minsplit the minsplit parameter of the model.
#' @param maxdepth the maxdepth parameter of the model.
#'
#' @seealso \code{\link[rpart]{rpart}}
#'
#' @export
#' 
dt_model <- function(data, variable.pred, minsplit =  20, maxdepth = 15){
  if(!is.null(variable.pred) && !is.null(data)){
    form <- formula(paste0(variable.pred,"~."))
    modelo.dt <- rpart(form, data, control = rpart.control(minsplit = minsplit, maxdepth = maxdepth),model = TRUE)
    return(modelo.dt)
  }
  return(NULL)
  
  # codigo <- paste0(model.var," <- rpart(`",variable.pred,"`~., data = ",data,",
  #                  control = rpart.control(minsplit = ",minsplit,", maxdepth = ", maxdepth,"))")
  # return(codigo)
}

#' dt_prediction
#'
#' @description generates the prediction of the decision trees model.
#'
#' @param model a decision trees model(rpart).
#' @param test.data dataframe.
#'
#' @export
dt_prediction <- function(model, test.data) {
  if(!is.null(model) && !is.null(test.data)){
    return(predict(model, test.data))
  }
  return(NULL)
  #return(paste0(pred.var," <- predict(",model.var,", ",data,")"))
}

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
  # return(paste0("rpart.plot::prp(",model.var,", type = 2, extra = 100, nn = TRUE, varlen = 0, faclen = 0,
  #               fallen.leaves = TRUE, branch.lty = 6, shadow.col = '#dedede',box.col = '#c8b028')"))
}


#------------------------------------CODE---------------------------------------
codeDt <- function(variable.predecir, minsplit, maxdepth){
  return(paste0("dt_model(data, '",variable.predecir,"', minsplit = ",minsplit, ", maxdepth = ",maxdepth,")"))
}


codeDtPred <- function(nombreModelo = "dt.model"){
  return(paste0("dt_prediction(model = ",nombreModelo,", test.data)"))
}

codeDtIG <- function(variable.predecir){
  return(paste0("general_indices(test.data[,'",variable.predecir,"'], prediccion.dt)"))
}

codeDtPlot <- function(nombreModelo){
  return(paste0("dt_plot(", nombreModelo,")"))
}