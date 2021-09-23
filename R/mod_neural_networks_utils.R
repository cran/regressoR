#' nn_model
#'
#' @description generates the code to create the neural network model.
#'
#' @param data dataframe
#' @param variable.pred the name of the variable to be predicted.
#' @param hidden a vector of integers specifying the number of hidden neurons (vertices) in each layer.
#' @param threshold a numeric value specifying the threshold for the partial derivatives of the error function as stopping criteria.
#' @param stepmax the maximum steps for the training of the neural network. Reaching this maximum leads to a stop of the neural network's training process.
#'
#' @seealso \code{\link[neuralnet]{neuralnet}}
#'
#' @export
#'
nn_model <- function(data, variable.pred, hidden = c(1), threshold = 0.1, stepmax = 2000){
  
  if(!is.null(variable.pred) && !is.null(data)){
    datos.dummies.apren <- dummy.data.frame(data)
    mean.vars <- sapply(datos.dummies.apren, mean)
    sd.vars <- sapply(datos.dummies.apren, sd)
    datos.dummies.apren <- as.data.frame(scale(datos.dummies.apren, center = mean.vars, scale = sd.vars))
    #nombres <- colnames(datos.dummies.apren)
    #form <- as.formula(paste(variable.pred,"~", paste0(nombres[!nombres %in% variable.pred], collapse = '+')))
    form <- formula(paste0(variable.pred,"~."))
    modelo.nn <- neuralnet(form, data = datos.dummies.apren, hidden = hidden, 
                          linear.output = TRUE, threshold = threshold, stepmax = stepmax)
    #Guardamos valores importante en nuestro modelo
    modelo.nn$mean.vars <- mean.vars
    modelo.nn$sd.vars <- sd.vars
    modelo.nn$variable.pred <- variable.pred
    #Cambiamos la forma en que va aparecer el call
    modelo.nn$call$formula <- form
    modelo.nn$call$hidden <- hidden
    modelo.nn$call$threshold <- threshold
    modelo.nn$call$stepmax <- stepmax
    
    return(modelo.nn)
  }
  return(NULL)
  
  # paste0("datos.dummies.apren <- dummy.data.frame(",data,")\n",
  #        mean.var," <- sapply(datos.dummies.apren, mean)\n",
  #        sd.var," <- sapply(datos.dummies.apren, sd)\n",
  #        "datos.dummies.apren <- as.data.frame(scale(datos.dummies.apren, center = ",mean.var,", scale = ",sd.var,"))\n",
  #        "nombres <- colnames(datos.dummies.apren)\n",
  #        "formula.nn <- as.formula(paste('",variable.pred,"~', paste0(nombres[!nombres %in% '",variable.pred,"'], collapse = '+')))\n",
  #        model.var," <- neuralnet(formula.nn, data = datos.dummies.apren, hidden = ",capas,",\n\t\t\tlinear.output = TRUE,",
  #        "threshold = ",threshold,", stepmax = ",stepmax,")\n")
}

#' nn_prediction
#' 
#' @description generates the prediction of a neural network model.
#'
#' @param model neural network model(neuralnet).
#' @param test.data dataframe.
#'
#' @seealso \code{\link[neuralnet]{compute}}
#'
#' @export
#'
nn_prediction <- function(model, test.data) {
  if(!is.null(test.data) && !is.null(model)){
    datos.dummies.prueb <- dummy.data.frame(test.data)
    datos.dummies.prueb[model$variable.pred] <- NULL
    #Para escalar los datos ya no podemos utilizar el promedio ni la sd de la variable a predecir
    means <- model$mean.vars[names(model$mean.vars) != model$variable.pred]
    sds <- model$sd.vars[names(model$sd.vars) != model$variable.pred]
    datos.dummies.prueb <- as.data.frame(scale(datos.dummies.prueb, center = means, scale = sds))
    prediction.nn <- neuralnet::compute(model, datos.dummies.prueb)$net.result
    #Como se escalaron los datos debemos volver a valor real.
    prediction.nn <- prediction.nn * model$sd.vars[[model$variable.pred]] + model$mean.vars[[model$variable.pred]]
    return(prediction.nn)
  }
  
  return(NULL)
  # paste0("datos.dummies.prueb <- as.data.frame(scale(dummy.data.frame(",data," %>% select(-`",variable.pred,"`))))\n",
  #        "datos.dummies.prueb['",variable.pred,"'] <- NULL\n",
  #        pred.var," <- neuralnet::compute(",model.var,", datos.dummies.prueb)$net.result\n",
  #        pred.var, " <- ",pred.var," * ",sd.var,"['",variable.pred,"'] + ",mean.var,"['",variable.pred,"']")
}

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
  
  # paste0("plot(",model.var,", arrow.length = 0.1, rep = 'best', intercept = TRUE,x.entry = 0.1, x.out = 0.9,\n\t",
  #        "information=FALSE,intercept.factor = 0.8,col.entry.synapse='red',col.entry='red',col.out='green',col.out.synapse='green',\n\t",
  #        "dimension=15, radius = 0.2, fontsize = 10)")
}


#------------------------------------CODE---------------------------------------
codeNn <- function(variable.predecir, hidden, threshold, stepmax){
  return(paste0("nn_model(data, '",variable.predecir,"', hidden = ",as_string_c(hidden,quote = FALSE), ", threshold = ", threshold, ", stepmax = ",stepmax, ")"))
}

codeNnPred <- function(nombreModelo = "nn.model"){
  return(paste0("nn_prediction(model = ", nombreModelo, ", test.data)"))
}

codeNnIG <- function(variable.predecir){
  return(paste0("general_indices(test.data[,'",variable.predecir,"'], prediccion.nn)"))
}