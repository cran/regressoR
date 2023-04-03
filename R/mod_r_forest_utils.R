# RF PAGE ------------------------------------------------------------------------------------------------------------

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

rf_ntree_values <- function(model) {
  ntree  = c(1:model$ntree)
  rf_rmse = sqrt(model$mse)
  best_ntree = ntree[which.min(rf_rmse)]
  # find overfitting, underfitting, and "best"" ntree
  fit_status = ifelse(ntree < best_ntree, "Over", ifelse(ntree == best_ntree, "Best", "Under"))
  rf_results = data.frame(
    ntree,
    round(rf_rmse, 10),
    fit_status
  )
  colnames(rf_results) = c("ntree", "RMSE", "Fit?")
  return(rf_results)
}
