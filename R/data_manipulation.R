
my_options <- new.env()
my_options$ops <- list(language  = "es",
                       rd.mode   = 0,
                       rlr.alpha = 1,
                       exe.envir = NULL)

#' options_regressor
#'
#' @param ... any options can be defined, using name = value or a character string holding an option name.
#' 
#' @export
#' 
#' @examples
#' options_regressor("language")
#' options_regressor(language = "en")
#' options_regressor("language")
#' 
options_regressor <- function(...){
  if(missing(...)){
    return(my_options$ops)
  }else{
    .args <- list(...)
    .names <- names(.args)
    if(suppressWarnings(all(!is.na(as.numeric(.names))))){
      my_options$ops[unlist(.args)]
    }else{
      my_options$ops[.names] <- unlist(.args)
    }
  }
}


#' general_indices
#'
#' @description calculates indices to measure accuracy of a model.
#'
#' @param real the real values in traning-testing.
#' @param prediccion the prediction values in traning-testing.
#'
#' @return a list with the Correlation, Relative Error, Mean Absolute Error and Root Mean Square Error.
#' @export
#'
#' @examples
#' real <- rnorm(45)
#' prediction <- rnorm(45)
#' model <- "KNN"
#' general_indices(real, prediction)
#' 
general_indices <- function(real, prediccion) {
  RMSE <- sqrt(sum((real - prediccion) ^ 2) / length(prediccion))
  MAE  <- sum(abs(real - prediccion)) / length(prediccion)
  RE   <- sum(abs(real - prediccion)) / sum(abs(real))
  COR  <- as.numeric(cor(real, prediccion))
  COR  <- ifelse(is.na(COR), 0 , COR)
  return(list(Raiz.Error.Cuadratico = RMSE,
              Error.Absoluto = MAE,
              Error.Relativo = RE,
              Correlacion = COR))
}

#' combine_names
#' 
#' @description combine two string vector by grouping according to the first vector.
#'
#' @param x a vector to combine with y. The combination is grouped by this parameter.
#' @param y a vector to combine with x.
#' @param sep a string with the separator characters.
#'
#' @return a vector with the combination of x and y.
#' @export
#'
#' @examples
#' x = c("A", "B", "C")
#' y = c("1", "2", "3")
#' combine_names(x, y)
#' 
combine_names <- function(x, y, sep = "."){
  unlist(lapply(y, function(y1)lapply(x, function(x1)paste0(x1,sep,y1))))
}

#' colnames_empty
#' 
#' @description gets names of the columns or an empty string if the data is NULL.
#'
#' @param data a data.frame with the data.
#'
#' @export
#'
#' @examples
#' colnames_empty(iris)
#' colnames_empty(NULL)
#' 
colnames_empty <- function(data){
  res <- colnames(data)
  if(is.null(res))
    return("")
  return(res)
}

#' var_numerical
#'
#' @description gets only the numerical columns.
#'
#' @param data the dataset.
#'
#' @return a vector with the names of the numerical columns.
#' @export
#'
#' @examples
#' var_numerical(iris)
#' 
var_numerical <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

#' var_categorical
#'
#' @description gets only the categorical columns.
#'
#' @param data the dataset.
#'
#' @return a vector with the names of the categorical columns.
#' @export
#'
#' @examples
#' var_categorical(iris)
#' 
var_categorical <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = !sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

#' summary_indices
#'
#' @description summarizes a variable by returning the minimum, first quartile, third quartile and maximum value.
#'
#' @param data a numeric vector. 
#'
#' @export
#'
#' @examples
#' summary_indices(iris$Sepal.Length)
#' 
summary_indices <- function(data){
  list("Min" = min(data),
       "1Q"  = quantile(data, prob=c(0.25)),
       "3Q"  = quantile(data, prob=c(0.75)),
       "Max" = max(data))
}

#' disjunctive_data
#' 
#' @description convert the columns selected to disjunctive.
#'
#' @param data the dataset to be converted.
#' @param vars a vector with the name of columns.
#'
#' @export
#'
#' @examples
#' disjunctive_data(iris, "Species")
#' 
disjunctive_data <- function(data, vars){
  if(is.null(data)) return(NULL)
  cualitativas <- base::subset(data, select = colnames(data) %in% c(vars))
  data <- data[, !colnames(data) %in% vars]
  for (variable in colnames(cualitativas)) {
    for (categoria in unique(cualitativas[, variable])) {
      nueva.var <- as.numeric(cualitativas[, variable] == categoria)
      data <- cbind(data, nueva.var)
      colnames(data)[length(colnames(data))] <- paste0(variable, '.', categoria)
    }
  }
  return(data)
}

#' comparative_table
#'
#' @description creates the comparison table.
#'
#' @param sel the selection of the models to be shown.
#' @param indices the values to be shown.
#' 
#' @export
#'
#' @examples
#' models <- list('knnl-mode1' = list(0.11,0.22,0.33,0.44),
#'                'dtl-mode2'  = list(0.12,0.23,0.34,0.45),
#'                'rfl-mode1'  = list(0.51,0.42,0.13,0.24))
#' sel <- c("K Vecinos Más Cercanos-mode1", "Bosques Aleatorios-mode1")
#' comparative_table(sel, models)
#' 
comparative_table <- function(sel, indices) {
  tryCatch({
    nombres <- models_mode(indices)
    
    if(nombres[1] == "---X---") {
      return(data.frame())
    }
    resp <- do.call(rbind, indices)
    rownames(resp) <- nombres
    colnames(resp) <- c(translate("RMSE"), translate("MAE"),
                        translate("ER")  , translate("correlacion"))
    resp <- as.data.frame(resp)
    resp[] <- lapply(resp, as.numeric)
    resp <- round(resp, 4)
    resp <- resp[nombres %in% sel,]
    resp <- resp[ order(row.names(resp)), ]
    return(resp)
    
  }, error = function(e){
    return(data.frame())
  })
}

#' validate_pn_data
#' 
#' @description verify that a data.frame has the same columns with the same types.
#'
#' @param x a data.frame with criteria to compare.
#' @param y a data.frame to be comprared.
#' @param var.pred a vector with the names of variables to be excluded from the comparison.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' validate_pn_data(iris, cars)
#' validate_pn_data(iris, iris)
#' x <- iris
#' x$Species <- as.numeric(x$Species)
#' validate_pn_data(iris, x)
#' }
validate_pn_data <- function(x, y, var.pred = ""){
  nombres <- colnames(x)
  selec <- -which(nombres == var.pred)
  if(length(selec) > 0){
    nombres <- nombres[selec]
  }
  nombres.prueba <- colnames(y)
  
  if(any(!(nombres.prueba %in% nombres))){
    stop(translate("NoTamColum"), call. = FALSE) 
  }
  
  tipos <- unlist(lapply(x[,nombres, drop = FALSE], class))
  tipos.prueba <- unlist(lapply(y[,nombres, drop = FALSE], class))
  
  if(any(tipos != tipos.prueba)){
    stop(translate("NoTamColum"),call. = FALSE)
  }
}

#' new_col
#' 
#' @description creates a new column.
#'
#' @param data the data.frame to join with the new column.
#' @param name the name of the new column.
#' @param values the values of the new column.
#'
#' @export
#'
#' @examples
#' new_col(iris)
#' new_col(iris, "var1", c(1,2,3))
#' 
new_col <- function(data, name = "new_", values = NA){
  data[,name] <- values
  return(data)
}

#' fisher_calc
#' 
#' @description calculate the fisher skewness.
#'
#' @param x a vector with the data to make the calculation.
#' @param na.rm a logical value indicating whether the NAs have to be eliminated.
#'
#' @export
#'
#' @examples
#' fisher_calc(iris$Petal.Length)
#' 
fisher_calc <- function (x, na.rm = FALSE) {
  if (na.rm){
    x <- x[!is.na(x)]
  }
  return(sum((x - mean(x))^3/sd(x)^3)/length(x))
}

#' calibrate_boosting
#' 
#' @description helps to get the maximum of n.minobsinnode and bag.fraction values with which no error is generated in the model.
#'
#' @param data the name of the learning data.
#' 
#' @seealso \code{\link[gbm]{gbm}}
#'
#' @export
#'
#' @examples
#' calibrate_boosting(iris)
#' 
calibrate_boosting <- function(data){
  nr <- nrow(data)
  for(i in 10:1){
    for (j in seq(0.5, 1, 0.1)) {
      if(nr * j > i*2 + 1){
        return(list(n.minobsinnode = i, bag.fraction = j))
      }
    }
  }
  return(NULL)
}
