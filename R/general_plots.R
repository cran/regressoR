#' gg_color_hue
#' 
#' @description create colors.
#'
#' @param n an integer specifying the number of colors to create.
#'
#' @return color-coded vector
#' @noRd
#'
#' @examples
#' col <- gg_color_hue(3)
#' plot(iris$Species, col = col)
#' 
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#' plot_real_prediction
#'
#' @description scatter plot between the actual value of the variable to be predicted and the prediction of the model.
#'
#' @param real the real values in traning-testing.
#' @param prediction the prediction values in traning-testing.
#' @param model the name of the model of the scatter plot.
#' @param titles Labels on the chart
#'
#' @author Ariel Arroyo <luis.ariel.arroyo@promidat.com>
#' @return echarts4r plot
#' @import echarts4r
#' @export
#' 
plot_real_prediction <- function(real, prediction, model = "", titles = c("Predicciones vs Valores Reales",
                                                                          "Valor Real","Prediccion")) {

  #Coordenadas para los puntos
  prediction <- unname(prediction)
  x_y.values <- list()
  for (i in 1:dim(real)[1]) {
    x_y.values[[i]] <- list(name = as.character(rownames(real)[i]),value = c(real[i,1],prediction[i]))
  }

  #Coordenadas para la linea
  line.Values <- list()
  minimo <- floor(min(real))
  maximo <- ceiling(max(real))
  values <- minimo:maximo
  for (i in 1:length(values)) {
    line.Values[[i]] <- list(value = c(values[i],values[i]))
  }

  opts <- list(
    xAxis = list(
      type = "value"
    ),
    yAxis = list(
      type = "value"
    ),
    series = list(
      list(
        type = "scatter",
        symbolSize = 10,
        color = "red",
        data = x_y.values,
        tooltip = list(formatter = e_JS(paste0(
        "function(params){
        return(params.marker + '<br/><b> ID: </b>' + params.name + '<br /><b>",
        titles[2],": </b>' + params.value[0].toFixed(4) + '<br /><b>",titles[3],": </b>' + params.value[1].toFixed(4))
      }
    ")))),
      list(
        type = "line",
        symbol = "none",
        lineStyle = list(width = 2),
        tooltip = list(show = F),
        color = "black",
        data = line.Values
      )
    )
  )

  e_charts() |>
    e_list(opts) |>
    e_title(text = paste0(titles[1],"  (",model,")")) |>
    e_axis_labels(x = titles[2], y = titles[3]) |>
    e_tooltip() |>
    e_datazoom(show = F) |>
    e_show_loading()
}