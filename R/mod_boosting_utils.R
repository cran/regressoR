#' boosting_model

boosting_model <- function(data, variable.pred, n.trees = 50, distribution = "gaussian", shrinkage = 0.1){
  if(!is.null(variable.pred) && !is.null(data)){
    extra.values <- calibrate_boosting(data)
    form <- formula(paste0(variable.pred,"~."))
    
    if(is.null(extra.values)){
      modelo.boost <- train.gbm(form, data = data, distribution = distribution, n.trees = n.trees, shrinkage = shrinkage)
    }else{
      modelo.boost <- train.gbm(form, data = data, distribution = distribution, n.trees = n.trees, shrinkage = shrinkage,
                          n.minobsinnode = extra.values[["n.minobsinnode"]], bag.fraction = extra.values[["bag.fraction"]])
      modelo.boost$call$n.minobsinnode <- extra.values[["n.minobsinnode"]]
      modelo.boost$call$bag.fraction <- extra.values[["bag.fraction"]]
    }
    modelo.boost$call$formula <- form
    modelo.boost$call$distribution <- distribution
    modelo.boost$call$n.trees <- n.trees
    modelo.boost$call$shrinkage <- shrinkage
    return(modelo.boost)
  }
  else{return(NULL)}
}

#' boosting_importance_plot
#' 
#' @description generates the graph of variable importance.
#'
#' @param model boosting model(gbm).
#' @param titles Labels on the chart
#' 
#' @export
#' 
boosting_importance_plot <- function(model, titles = c("Importancia de Variables segun Influencia Relativa",
                                                       "Influencia Relativa","Variable")){
  df <- summary.gbm(model,order = T, plotit = F)
  
  e_charts(data = df, x = var) |>
    e_bar_(serie = "rel.inf" ,legend = NULL) |>
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

