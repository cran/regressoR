# KNN PAGE ----------------------------------------------------------------------------------------------------------------

# RMSE------------------------------------------------------------------------------
make_knn_pred = function(train, test, variable.pred, k = 7,  scale = TRUE, kernel = "optimal", distance = 2) {
  eval(parse(text = "library('traineR')"))
  if(!is.null(variable.pred) && !is.null(train)){
    form  <- formula(paste0(variable.pred,"~."))
    modelo.knn     <- train.knn(form, data = train, scale = scale, ks = k, kernel = kernel, distance = distance)
    prediccion.knn <- predict(modelo.knn, test)$prediction
    return(rmse(test[,variable.pred], prediccion.knn))
  }
  return(NULL)
}

rmse_k_values <- function(train, test, variable.pred, k = c(1:20),  scale = TRUE, kernel = "optimal", distance = 2) {
  knn_rmse = sapply(k, make_knn_pred, 
                    train = train, 
                    test = test, variable.pred = variable.pred, scale = scale, kernel = kernel, distance = distance)
  best_k = k[which.min(knn_rmse)]
  
  # find overfitting, underfitting, and "best"" k
  fit_status = ifelse(k < best_k, "Over", ifelse(k == best_k, "Best", "Under"))
  knn_results = data.frame(
    k,
    round(knn_rmse, 2),
    fit_status
  )
  colnames(knn_results) = c("k", "RMSE", "Fit?")
  return(knn_results)
}

get_title <- function(id = "KNN", idioma){
  if(id == "KNN")
    return(c("RMSE Segun Numero de Vecinos",
             "Numero de Vecinos","RMSE"))
  if(id == "BOOST" || id == "RF")
    return(c(paste0("RMSE Segun ", tr("numTree", idioma)),
             tr("numTree", idioma),"RMSE"))
  
}

plot_RMSEK <- function(datos , modelo.knn = NULL, titles = c("RMSE Segun Numero de Vecinos",
                                                             "Numero de Vecinos","RMSE")){
  modelo <- modelo.knn
  # df <- data.frame(k = 1:dim(modelo.knn$MEAN.SQU)[1],rmse = sqrt(modelo.knn$MEAN.SQU))
  # best_k <- modelo.knn$best.parameters$k
  # best_rmse <- df[best_k,2]
  df = datos
  best_k <- df[which(df$`Fit?` == "Best"),1]
  best_rmse <- df[which(df$`Fit?` == "Best"),2]
  #Coordenadas para los puntos
  x_y.RMSE <- list()
  for (i in 1:dim(df)[1]) {
    x_y.RMSE[[i]] <- list(value = c(df[i,1],df[i,2]))
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
      nameTextStyle = list(fontSize = 13)
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
          return('<b>",unlist(strsplit(titles[2], ":")),": </b>' + params.value[0] + '<br /><b>",titles[3],": </b>' + params.value[1].toFixed(8))
      }
    "))))
    )
  )
  
  e_charts() |>
    e_list(opts) |>
    e_title(text = titles[1]) |>
    e_tooltip() |>
    e_datazoom(show = F) |>
    e_show_loading()|> 
    e_mark_line(data = list(xAxis = best_k,name = best_rmse,
                            tooltip = list(formatter = e_JS(paste0("function(params){",
                                                                   "return('<b>K Value: </b>' + ",
                                                                   "Number.parseFloat(params.value) + ",
                                                                   "'</br><b>RMSE Value: </b>' + Number.parseFloat(params.name).toFixed(8))}")))))
}

