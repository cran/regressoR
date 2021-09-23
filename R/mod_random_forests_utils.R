# RF PAGE ------------------------------------------------------------------------------------------------------------

#' rf_model
#' 
#' @description generates a random forest model.
#'
#' @param data dataframe
#' @param variable.pred the name of the variable to be predicted.
#' @param ntree the ntree parameter of the model.
#' @param mtry the mtry parameter of the model.
#'
#' @seealso \code{\link[randomForest]{randomForest}}
#'
#' @export
#'
rf_model <- function(data, variable.pred, ntree = 500, mtry = 1){
  if(!is.null(variable.pred) && !is.null(data)){
    form <- formula(paste0(variable.pred,"~."))
    modelo.rf <- randomForest(form, data = data, ntree = ntree, mtry = mtry,
                              importance = TRUE)
    # Guardamos los datos dentro del modelo para utilizar 
    #la función printRandomForest() en Utilities
    modelo.rf$datos <- data
    #Cambiamos la forma en que va aparecer el call
    modelo.rf$call$formula <- form
    modelo.rf$call$ntree <- ntree
    modelo.rf$call$mtry <- mtry
    return(modelo.rf)
  }
  return(NULL)
  # codigo <- paste0(model.var," <- randomForest(`",variable.pred,"`~., data = ",data,",importance = TRUE,",
  #                  " ntree =",ntree,",mtry =",mtry,")")
  # return(codigo)
}

#' rf_prediction
#' 
#' @description generates the prediction of the random forest model.
#'
#' @param model Random Forest model(randomForest).
#' @param test.data dataframe.
#'
#' @export
#'
rf_prediction <- function(model, test.data){
  if(!is.null(test.data) && !is.null(model)){
    return(predict(model,test.data))
  }
  return(NULL)
  #return(paste0(pred.var," <- predict(",model.var,", ",data," %>% select(-`",variable.pred,"`))"))
}



#' importance_plot_rf
#' 
#' @description graphs the importance of variables for the random forest model according to the percentage increase in mean square error.
#'
#' @param model.rf a random forest model.
#' @param titles labels on the chart
#'
#' @seealso \code{\link[randomForest]{randomForest}}
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' 
#' @export
#'
importance_plot_rf <- function(model.rf, titles = c("Importancia de Variables Segun el Porcentaje de Incremento del MSE",
                                                    "Aumento porcentual del error cuadratico medio", "Variable")){
  #https://www.displayr.com/how-is-variable-importance-calculated-for-a-random-forest/
  df <- as.data.frame(model.rf$importance)
  df$variables <- as.factor(rownames(df))
  df <- df[order(df$`%IncMSE`, decreasing = T),]
  
  e_charts_(data = df, x = "variables") |>
    e_bar_(serie = "%IncMSE",legend = NULL) |>
    echarts4r::e_flip_coords() |>
    e_title(text = titles[1]) |>
    e_x_axis(name = titles[2], nameLocation = "center", 
             nameTextStyle = list(padding = c(10,0,0,0)),
             interval = 10,
             axisLabel = list(formatter = '{value} %')) |>
    e_y_axis(name = titles[3], nameLocation = "start", inverse = T) |>
    e_tooltip(formatter = e_JS("function(params){
    console.log(params)
    return('<b>' +  params.value[1] + ': </b>' + Number.parseFloat(params.value[0]).toFixed(4) + '%')
    }
    ")) |>
    e_datazoom(show = F) |>
    e_show_loading()
}


#------------------------------------CODE---------------------------------------
codeRf <- function(variable.predecir, ntree, mtry){
  return(paste0("rf_model(data, '",variable.predecir,"', ntree = ",ntree, ", mtry = ", mtry, ")"))
}

codeRfPred <- function(nombreModelo = "rf.model"){
  return(paste0("rf_prediction(model = ", nombreModelo, ", test.data)"))
}

codeRfIG <- function(variable.predecir){
  return(paste0("general_indices(test.data[,'",variable.predecir,"'], prediccion.rf)"))
}