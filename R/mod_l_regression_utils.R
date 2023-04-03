
#' rl_coeff
#' 
#' @description get the information of the coefficients of the linear regression model
#'
#' @param modelo linear regression model
#'
#' @export
#'
rl_coeff <- function(modelo){
  if(!is.null(modelo)){
    summ <- summary(modelo)
    df.rl <- as.data.frame(summ$coefficients)
    df.rl <- cbind(df.rl,  Importance = symnum(summ$coefficients[,4], corr = FALSE, na = FALSE,
                                               cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                               symbols = c('***', '**', '*', '.', ' ')))
    df.rl <- as.data.frame(df.rl)
    r2 <- summ$r.square
    
    return(list(df.rl = df.rl, r2 = r2))
  }
  return(NULL)
  
}



#' disp_models
#' 
#' @description this function generates the call code of the scatter function.
#'
#' @param prediction the name of the prediction object.
#' @param model_name the name of the model.
#' @param var_pred the name of the variable to be predicted.
#'
#' @export
#'
#' @examples
#' disp_models("prediction.knn", "KNN", "Species")
#' 
disp_models <- function(prediction, model_name, var_pred){
  
  paste0("plot_real_prediction(datos.prueba['",var_pred,"'], ", prediction,", '",model_name,"')")
}