# SVM PAGE ----------------------------------------------------------------------------------------------------------------

#' svm_model
#' 
#' @description generates a support vector machines model.
#'
#' @param data dataframe
#' @param variable.pred the name of the variable to be predicted.
#' @param scale the scale parameter of the model.
#' @param kernel string. The kernel parameter of the model.
#'
#' @seealso \code{\link[e1071]{svm}}
#'
#' @export
#' 
svm_model <- function(data, variable.pred, scale = TRUE, kernel = "linear"){
  if(!is.null(variable.pred) && !is.null(data)){
    form <- formula(paste0(variable.pred,"~."))
    modelo.svm <- svm(form, data, scale = scale, kernel = kernel)
    #Cambiamos la forma en que va aparecer el call
    modelo.svm$call$formula <- paste0(variable.pred,"~.")
    modelo.svm$call$kernel <- kernel
    modelo.svm$call$scale <- scale
    return(modelo.svm)
  }
  return(NULL)
  
  #return(paste0(model.var," <- svm(`",variable.pred,"`~., data = ",data,", scale =",scale,", kernel = '",kernel,"')"))
}

#' svm_prediction
#' 
#' @description generates the prediction of the support vector machine model.
#' 
#' @param model a support vector machine model(svm).
#' @param test.data dataframe.
#'
#' @export
#' 
svm_prediction <- function(model, test.data){
  if(!is.null(test.data) && !is.null(model)){
    return(predict(model,test.data))
  }
  return(NULL)
  
  #return(paste0(pred.var," <- predict(",model.var," , ",data," %>% select(-`",variable.pred,"`))"))
}



#------------------------------------CODE---------------------------------------
codeSvm <- function(variable.predecir, scale, kernel){
  return(paste0("svm_model(data, '",variable.predecir,"', scale = ",scale, ", kernel = '",kernel,"')"))
}

codeSvmPred <- function(nombreModelo = "svm.model"){
  return(paste0("svm_prediction(model = ", nombreModelo, ", test.data)"))
}

codeSvmIG <- function(variable.predecir){
  return(paste0("general_indices(test.data[,'",variable.predecir,"'], prediccion.svm)"))
}