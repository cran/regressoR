#' rd_model
#'
#' @description generates a dimension reduction model.
#'
#' @param data dataframe
#' @param variable.pred the name of the variable to be predicted.
#' @param mode the method of dimension reduction is defined as mode=1 is the MCP, and mode=0 the ACP.
#' @param scale the scale parameter of the model.
#'
#' @seealso \code{\link[pls]{pcr}}, \code{\link[pls]{plsr}}
#'
#' @export
#'
rd_model <- function(data, variable.pred, mode = 0, scale = TRUE){
  if(!is.null(variable.pred) && !is.null(data)){
    modelo.rd <- NULL
    form <- formula(paste0(variable.pred,"~."))
    if(mode == 0){ # ACP
      modelo.rd <- pcr(form, data = data, scale = scale, validation = 'CV')
    }
    else if(mode == 1){ # MCP
      modelo.rd <- plsr(form, data = data, scale = scale, validation = 'CV')
    }
    optimal.n <-  which.min(RMSEP(modelo.rd)$val[1, 1, ]) - 1
    names(optimal.n) <- NULL
    modelo.rd$optimal.n.comp <- optimal.n
    
    #Cambiamos el call
    modelo.rd$call$formula <- form
    modelo.rd$call$scale <- scale
    
    return(modelo.rd)
  }
  else{
    return(NULL)
  }
  
  # if(mode == 0){
  #   x <- paste0(model.var," <- pcr(`",variable.pred,"`~.,data = ",data,", scale = ",scale,", validation = 'CV')")
  # }else{
  #   x <- paste0(model.var," <- plsr(`",variable.pred,"`~.,data = ",data,", scale = ",scale,", validation = 'CV')")
  # }
  # paste0(x,"\n",n.comp, " <- which.min(RMSEP(",model.var,")$val[1, 1, ]) - 1")
}

#' rd_prediction
#' 
#' @description generates the prediction of a dimension reduction model.
#'
#' @param model dimension reduction model(pcr/plsr).
#' @param test.data dataframe.
#' @param ncomp a numerical value in case you don't want to use the optimum number of components.
#'
#' @export
#' 
rd_prediction <- function(model, test.data, ncomp = NULL) {
  if(!is.null(test.data) && !is.null(model)){
    ncomp <- ifelse(is.null(ncomp), model$optimal.n.comp, ncomp)
    return(predict(model,test.data, ncomp = ncomp))
  }
  return(NULL)
  #paste0(pred.var," <- predict(",model.var,", ",data,", ncomp = ",ncomp,")")
}

#' rd_type
#' 
#' @description returns the name of the method of dimension reduction.
#'
#' @param mode.rd the method of dimension reduction is defined as mode=1 is the MCP, and mode=0 the ACP.
#' 
#' @seealso \code{\link[pls]{pcr}}, \code{\link[pls]{plsr}}
#'
#' @export
#'
#' @examples
#' rd_type(1)
#' rd_type(0)
#' 
rd_type <- function(mode.rd = 0){
  mode.rd <- ifelse(is.null(mode.rd), 0, mode.rd)
  tipo <- "NA"
  if(mode.rd == 0){ tipo <- "ACP" }
  else if(mode.rd == 1){ tipo <- "MCP" }
  return(tipo)
}



#' plot_RMSE
#' 
#' @description graph the root mean square error of cross validation according to components used.
#'
#' @param model a dimension reduction model.
#' @param n.comp the optimum number of components.
#' @param titles labels on the chart
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' 
#' @export
#' 
plot_RMSE <- function(model, n.comp, titles = c("RMSE Segun Numero de Componentes",
                                                "Numero de Componente","RMSE")){
  
  RMSE.CV <- pls::RMSEP(model)$val[1, 1, ]
  df <- data.frame(Componentes = 0:(length(RMSE.CV) - 1), Error = RMSE.CV)
  
  #Coordenadas para los puntos
  x_y.RMSE <- list()
  for (i in 1:dim(df)[1]) {
    x_y.RMSE[[i]] <- list(value = c(df[i,1],df[i,2]))
  }
  
  #Coordenadas para la linea
  line.Values <- list()
  maximo <- ceiling(max(df[,2]))
  values <- 0:maximo
  for (i in 1:length(values)) {
    line.Values[[i]] <- list(value = c(n.comp,values[i]))
  }
  
  opts <- list(
    xAxis = list(
      type = "value",
      name = titles[2],
      nameTextStyle = list(fontSize = 13),
      max = max(df[,1]),
      interval = 2
    ),
    yAxis = list(
      type = "value",
      name = titles[3],
      nameTextStyle = list(fontSize = 13),
      max = maximo
    ),
    series = list(
      list(
        type = "line",
        symbolSize = 6,
        lineStyle = list(width = 2,type = 'solid'),
        color = "#4682B4",
        data = x_y.RMSE,
        tooltip = list(formatter = e_JS(paste0(
          "function(params){
          return('<b>",titles[2],": </b>' + params.value[0] + '<br /><b>",titles[3],": </b>' + params.value[1].toFixed(4))
      }
    ")))),
      list(
        type = "line",
        symbol = "none",
        lineStyle = list(width = 2, type = 'dashed'),
        tooltip = list(show = F),
        color = "blue",
        data = line.Values
      )
    )
  )
  
  e_charts() |>
    e_list(opts) |>
    e_title(text = titles[1]) |>
    e_tooltip() |>
    e_datazoom(show = F) |>
    e_show_loading()
}



#' plot_pred_rd
#' 
#' @description graph of variance explained in the predictors according to components used.
#'
#' @param model a dimension reduction model.
#' @param n.comp the optimum number of components.
#' @param titles labels on the chart
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' @export
#' 
plot_pred_rd <- function(model, n.comp, titles = c("Varianza Explicada en Predictores",
                                                   "Numero de Componentes","Porcentaje de Varianza Explicada")){
  
  
  var.explicada <- cumsum(pls::explvar(model)) / 100
  df <- data.frame(Componentes = 1:length(var.explicada), Varianza = var.explicada * 100)
  
  # Coordenadas x,y
  x_y.Varianza <- list()
  for (i in 1:dim(df)[1]) {
    x_y.Varianza[[i]] <- list(value = c(df[i,1],df[i,2]))
  }
  
  #Coordenadas para la linea
  line.Values <- list()
  maximo <- ceiling(max(df[,2]))
  values <- 0:maximo
  for (i in 1:length(values)) {
    line.Values[[i]] <- list(value = c(n.comp,values[i]))
  }
  
  opts <- list(
    xAxis = list(
      type = "value",
      name = titles[2],
      nameTextStyle = list(fontSize = 13),
      max = max(df[,1]),
      interval = 2
    ),
    yAxis = list(
      type = "value",
      name = titles[3],
      nameTextStyle = list(fontSize = 13),
      axisLabel = list(formatter = '{value} %'),
      max = maximo
    ),
    series = list(
      list(
        type = "line",
        symbolSize = 6,
        lineStyle = list(width = 2,type = 'solid'),
        color = "#4682B4",
        data = x_y.Varianza,
        tooltip = list(formatter = e_JS(paste0(
          "function(params){
          return('<b>",titles[2],": </b>' + params.value[0] + '<br /><b>",titles[3],": </b>' + params.value[1].toFixed(4))
      }
    ")))),
      list(
        type = "line",
        symbol = "none",
        lineStyle = list(width = 2, type = 'dashed'),
        tooltip = list(show = F),
        color = "blue",
        data = line.Values
      )
    )
  )
  
  e_charts() |>
    e_list(opts) |>
    e_title(text = titles[1]) |>
    e_tooltip() |>
    e_datazoom(show = F) |>
    e_show_loading()
}



#' plot_var_pred_rd
#' 
#' @description graph of the variance explained in the variable to predict according to the components used.
#'
#' @param model a dimension reduction model.
#' @param n.comp the optimum number of components.
#' @param titles labels on the chart
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' @export
#' 
plot_var_pred_rd <- function(model, n.comp, titles = c("Varianza Explicada en Variable a Predecir",
                                                       "Numero de Componente","Porcentaje de Varianza Explicada")){
  
  var.explicada <- drop(pls::R2(model, estimate = "train", intercept = FALSE)$val)
  df <- data.frame(Componentes = 1:length(var.explicada), Varianza = var.explicada * 100)
  
  # Coordenadas x,y
  x_y.Varianza <- list()
  for (i in 1:dim(df)[1]) {
    x_y.Varianza[[i]] <- list(value = c(df[i,1],df[i,2]))
  }
  
  #Coordenadas para la linea
  line.Values <- list()
  maximo <- ceiling(max(df[,2]))
  values <- 0:maximo
  for (i in 1:length(values)) {
    line.Values[[i]] <- list(value = c(n.comp,values[i]))
  }
  
  opts <- list(
    xAxis = list(
      type = "value",
      name = titles[2],
      nameTextStyle = list(fontSize = 13),
      max = max(df[,1]),
      interval = 2
    ),
    yAxis = list(
      type = "value",
      name = titles[3],
      nameTextStyle = list(fontSize = 13),
      axisLabel = list(formatter = '{value} %'),
      max = maximo
    ),
    series = list(
      list(
        type = "line",
        symbolSize = 6,
        lineStyle = list(width = 2,type = 'solid'),
        color = "#4682B4",
        data = x_y.Varianza,
        tooltip = list(formatter = e_JS(paste0(
          "function(params){
          return('<b>",titles[2],": </b>' + params.value[0] + '<br /><b>",titles[3],": </b>' + params.value[1].toFixed(4))
      }
    ")))),
      list(
        type = "line",
        symbol = "none",
        lineStyle = list(width = 2, type = 'dashed'),
        tooltip = list(show = F),
        color = "blue",
        data = line.Values
      )
    )
  )
  
  e_charts() |>
    e_list(opts) |>
    e_title(text = titles[1]) |>
    e_tooltip() |>
    e_datazoom(show = F) |>
    e_show_loading()
}


#------------------------------------CODE---------------------------------------
codeRd <- function(variable.predecir, mode, scale){
  return(paste0("rd_model(data, '",variable.predecir,"', mode = ",mode, ", scale = ", scale, ")"))
}

codeRdPred <- function(nombreModelo, ncomp){
  return(paste0("rd_prediction(model = ", nombreModelo, ", test.data, ncomp = ", ncomp, ")"))
}

codeRdIG <- function(variable.predecir){
  return(paste0("general_indices(test.data[,'",variable.predecir,"'], prediccion.rd)"))
}