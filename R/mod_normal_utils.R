#' Normal plot
#'
#' @param data a numeric column of a data.frame.
#' @param colorbar a color for the bars.
#' @param colorline a color for the line.
#' @param nombres a character vector of length 2 specifying the titles to use on legend.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_histnormal
#' @import echarts4r
#' @importFrom graphics hist
#' @importFrom stats dnorm sd
#' @examples
#' e_histnormal(iris$Sepal.Length)
#' 
e_histnormal <- function(data, colorbar = "steelblue", colorline = "gray",
                         nombres = c("Histograma", "Curva Normal")) {
  h <- hist(data, plot = F)
  x <- seq(min(h$mids, na.rm = T), max(h$mids, na.rm = T), length = length(h$mids))
  promedio <- mean(data, na.rm = T)
  desviacion <- sd(data, na.rm = T)
  normalidad <- dnorm(x, promedio, desviacion)
  
  d <- diff(h$breaks)[1]
  
  distribu <- data.frame(
    x = h$mids, d = h$density, n = normalidad,
    name = paste0("(", h$mids - d / 2, " - ", h$mids + d / 2, ")")
  )
  
  distribu |> e_charts_("x") |> e_bar_("d", name = nombres[1]) |>
    e_line_("n", name = nombres[2]) |> e_x_axis(scale = T) |>
    e_axis_labels(x = "", y = "Densidad") |> 
    e_color(c(colorbar, colorline)) |> e_tooltip() |> 
    e_datazoom(show = F) |> e_show_loading()
}

#' Qplot + Qline
#'
#' @param data a numeric column of a data.frame.
#' @param colorpoint a color for the points.
#' @param colorline a color for the line.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return echarts4r plot
#' @export e_qq
#' @import echarts4r
#' @importFrom stats qnorm qqnorm quantile
#' @examples
#' e_qq(iris$Sepal.Length)
#' 
e_qq <- function(data, colorpoint = "steelblue", colorline = "gray") {
  data <- data.frame(qqnorm(data, plot = F))
  
  y <- quantile(data$y, c(0.25, 0.75), names = F, na.rm = T)
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  data$z <- data$x * slope + int
  data <- round(data, 3)
  
  data |> e_charts_("x") |> e_scatter_("y", name = "QQplot", symbol_size = 8) |>
    e_line_("z", name = "QQline", symbol = 'none') |> e_x_axis(scale = T) |>
    e_y_axis(scale = T) |> e_tooltip() |> e_datazoom(show = F) |> 
    e_color(c(colorpoint, colorline)) |> e_show_loading()
}

#' Data.frame with normal test
#'
#' @param data a data.frame object only with the numeric columns.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export dfnormal
#' @importFrom stats complete.cases pchisq pnorm shapiro.test
#' @examples
#' dfnormal(iris[, -5])
#' 
dfnormal <- function(data) {
  data    <- var.numericas(data)
  fisher  <- sapply(data, function(i) fisher.calc(i))
  pearson <- sapply(data, function(i) pearson.test(i))
  lillie  <- sapply(data, function(i) lillie.test(i))
  cvm     <- sapply(data, function(i) cvm.test(i))
  shapiro <- sapply(data, function(i) shapiro.test(i)$p.value)
  
  data.frame(cbind(fisher, pearson, lillie, cvm, shapiro))
}





########################## modeest ###########################################

fisher.calc <- function (x, na.rm = FALSE, ...) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  if (na.rm)
    x <- x[!is.na(x)]
  nx <- length(x)
  
  sk <- sum((x - mean(x))^3/stats::sd(x)^3)/nx
  
  return(sk)
}

########################## nortest ###########################################

pearson.test <- function (x, n.classes = ceiling(2 * (n^(2/5))), adjust = TRUE) {
  x <- x[complete.cases(x)]
  n <- length(x)
  if (adjust) {
    dfd <- 2
  }
  else {
    dfd <- 0
  }
  num <- floor(1 + n.classes * pnorm(x, mean(x), sd(x)))
  count <- tabulate(num, n.classes)
  prob <- rep(1/n.classes, n.classes)
  xpec <- n * prob
  h <- ((count - xpec)^2)/xpec
  P <- sum(h)
  pvalue <- pchisq(P, n.classes - dfd - 1, lower.tail = FALSE)
  
  return(pvalue)
}

lillie.test <- function (x) {
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if (n < 5) 
    stop("sample size must be greater than 4")
  p <- pnorm((x - mean(x))/sd(x))
  Dplus <- max(seq(1:n)/n - p)
  Dminus <- max(p - (seq(1:n) - 1)/n)
  K <- max(Dplus, Dminus)
  if (n <= 100) {
    Kd <- K
    nd <- n
  }
  else {
    Kd <- K * ((n/100)^0.49)
    nd <- 100
  }
  pvalue <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 * 
                  Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598/sqrt(nd) + 
                  1.67997/nd)
  
  return(pvalue)
}

cvm.test <- function (x) {
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if (n < 8) 
    stop("sample size must be greater than 7")
  p <- pnorm((x - mean(x))/sd(x))
  W <- (1/(12 * n) + sum((p - (2 * seq(1:n) - 1)/(2 * n))^2))
  WW <- (1 + 0.5/n) * W
  if (WW < 0.0275) {
    pval <- 1 - exp(-13.953 + 775.5 * WW - 12542.61 * WW^2)
  }
  else if (WW < 0.051) {
    pval <- 1 - exp(-5.903 + 179.546 * WW - 1515.29 * WW^2)
  }
  else if (WW < 0.092) {
    pval <- exp(0.886 - 31.62 * WW + 10.897 * WW^2)
  }
  else if (WW < 1.1) {
    pval <- exp(1.111 - 34.242 * WW + 12.832 * WW^2)
  }
  else {
    pval <- 7.37e-10
  }
  return(pval)
}