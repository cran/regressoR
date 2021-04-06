
# In this file are the functions that generate the code as text
# These texts are shown to the user through the 'shiny' application, 
# but most of them can work with the help of the 'exe' function.

# LOAD AND TRANSFORMATION PAGE---------------------------------------------------------------------------------------------

#' code_load
#' 
#' @description generates data reading code.
#'
#' @param row.names a logical value indicating whether the data has row names.
#' @param path the path of the file.
#' @param sep the column separator in the file.
#' @param sep.dec the decimal separator in the file.
#' @param header a logical value indicating whether the file has a header.
#' @param d.o the name of the original data.
#' @param d the name of the current data.
#'
#' @export
#'
#' @examples
#' code_load(TRUE, "MY/PATH/FILE.csv")
#' 
code_load <- function(row.names = TRUE, path = NULL, sep = ";", sep.dec = ",", header = TRUE, d.o = "datos.originales", d = "datos"){
  if(!is.null(path)){
    path <-  gsub("\\", "/", path, fixed=TRUE)
  }
  if(row.names){
    return(paste0(d.o, " <- read.table(stringsAsFactors = T, '", path, "', header=", header, ", sep='", sep,
                   "', dec = '", sep.dec, "', row.names = 1) \n",d," <- ",d.o))
  } else {
    return(paste0(d.o, "<- read.table(stringsAsFactors = T, '", path, "', header=", header, ", sep='", sep,
                  "', dec = '", sep.dec,"') \n",d," <- ",d.o))
  }
}

#' code_NA
#' 
#' @description creates the code that imputes the NAs data or removes them.
#'
#' @param deleteNA a logical value indicating whether the NAs have to be eliminated or whether they have to be imputed. If TRUE then the NAs are eliminated, otherwise the data is imputed.
#' @param d.o the name of the original data.
#'
#' @export
#'
#' @examples 
#' iris2 <- iris
#' x <- code_NA(TRUE, 'iris2')
#' exe(x)
#' x <- code_NA(FALSE, 'iris2')
#' exe(x)
#' 
code_NA <- function(deleteNA = TRUE, d.o = "datos.originales") {
  ifelse(deleteNA, paste0(d.o, " <- na.omit(",d.o,")\n"),
         paste0("Mode <- function(x) {\n  x[which.max(summary(x))]\n}\n",
                "for (variable in colnames(",d.o,")) {\n",
                "  if(any(is.na(",d.o,"[, variable]))){\n",
                "   ifelse(class(",d.o,"[, variable]) %in% c('numeric', 'integer'),\n",
                "           ",d.o,"[, variable][is.na(",d.o,"[, variable])] <- \n",
                "                                              mean(",d.o,"[, variable], na.rm = TRUE),\n",
                "           ",d.o,"[, variable][is.na(",d.o,"[, variable])] <- \n",
                "                                     Mode(",d.o,"[, variable]))",
                "\n   }\n}"))
}

#' code_transf
#' 
#' @description generate code to transform data.
#'
#' @param variable the name of the variable to be converted.
#' @param new.type the new type of the variable. Can be categorical, numerical or disjunctive. ('categorico', 'numerico', 'disyuntivo')
#' @param d.o the name of the original data.
#' @param d the name of the current data.
#'
#' @export
#' 
#' @examples 
#' iris2 <- iris
#' x <-code_transf('Species', 'disyuntivo', 'iris', 'iris2')
#' exe(x)
#' head(iris2)
#' 
code_transf <- function(variable, new.type, d.o = "datos.originales", d="datos"){
  if(new.type == "categorico"){
    return(paste0(d,"[, '", variable, "'] <- as.factor(",d,"[, '", variable, "'])"))
  } else if(new.type == "numerico") {
    v <- as.character(exe(d,"[, '", variable, "']"))
    if(all(grepl("^[[:digit:]]+((\\.|,)[[:digit:]]+)*$", v))){
      return(paste0(d,"[, '", variable, "'] <- as.numeric(sub(',', '.', ",d,"[, '", variable, "'], fixed = TRUE))"))
    }else{
      return(paste0(d,"[, '", variable, "'] <- as.numeric(",d,"[, '", variable, "'])"))
    }
  } else {
    es.factor <- ifelse(exe("class(",d.o,"[, '",variable,"']) %in% c('numeric', 'integer')"),
                         paste0(d,"[, '", variable, "'] <- as.factor(",d,"[, '", variable, "']) \n"), "")
    return(paste0(es.factor, d, " <- disjunctive_data(",d,", '", variable,"')"))
  }
}

#' code_deactivate
#' 
#' @description creates the code that deactivates the selected variables of the data.
#'
#' @param variables the name of the variables to be deactivated.
#' @param d the name of the current data.
#'
#' @export
#'
#' @examples
#' iris2 <- iris
#' x <- code_deactivate('Species', 'iris2')
#' exe(x)
#' head(iris2)
#' 
code_deactivate <- function(variables, d = "datos"){
  return(paste0(d, " <- subset(",d,", select = -c(", paste(variables, collapse = ","), "))"))
}

#' partition_code
#'
#' @description creates the partition code for testing and learning data.
#'
#' @param data the name of the current data.
#' @param p the percentage of data for the learning data.
#' @param variable  the name of the variable to be predicted.
#' @param semilla a number with the random seed.
#' @param perm.semilla a logical value indicating whether the random seed should be used.
#'
#' @export
#'
#' @examples
#' x <- partition_code('iris', 75, 'Species', 555, TRUE)
#' exe(x)
#' head(datos.aprendizaje)
#' head(datos.prueba)
#' 
partition_code <- function(data = "datos", p = 50, variable = NULL, semilla = 5, perm.semilla = FALSE){
  semilla <- ifelse(is.numeric(semilla), semilla, 5)
  codigo <- ifelse(perm.semilla, paste0("set.seed(",semilla,")"), paste0("set.seed(NULL)"))
  codigo <- paste0(codigo,"\nvariable.predecir <- '",variable,"'\nparticion <- sample(1:nrow(",data,"),size = nrow(",data,")*",p/100,", replace = FALSE)\n",
                  "datos.prueba <- ",data,"[-particion,]\ndatos.aprendizaje <- ",data,"[particion,]\nreal.val <- datos.prueba[, '",variable,"', drop = FALSE]")
  codigo <- ifelse(perm.semilla, paste0(codigo, "\nset.seed(",semilla,")"),codigo)
  return(codigo)
}

# NUMERICAL SUMMARY PAGE --------------------------------------------------------------------------------------------------

#' code_summary
#'
#' @description creates the code for the basic summary of variables.
#'
#' @param data the name of the current data.
#'
#' @export
#'
#' @examples
#' x <- code_summary('iris')
#' exe(x)
#' 
code_summary <- function(data = "datos"){
  return(paste0("summary(", data, ")"))
}

# NORMALITY TEST PAGE -----------------------------------------------------------------------------------------------------

#' normal_default
#' 
#' @description generates the code of the normality test.
#'
#' @param data the name of the current data.
#' @param vars the variable for analysis. It has to be numeric.
#' @param color the color of the histogram.
#' @param labelcurva label for the curve.
#'
#' @export
#'
#' @examples
#' x <- normal_default('iris', 'Sepal.Length')
#' exe(x)
#' 
normal_default <- function(data = "datos", vars = NULL, color = "#00FF22AA", labelcurva = "Curva Normal"){
  if(is.null(vars)){
    return(NULL)
  } else {
    return(paste0(
      "promedio <- mean(", data, "[, '", vars, "']) \n",
      "desviacion <- sd(", data, "[, '", vars, "']) \n",
      "values <- dnorm(", data, "[, '", vars, "'], mean = promedio, sd = desviacion) \n",
      "values <- c(values, hist(", data, "[, '", vars, "'],  plot = FALSE)$density) \n",
      "hist(", data, "[, '", vars, "'], col = '", color, "', border=FALSE, axes=FALSE,\n",
      "  freq = FALSE, ylim = range(0, max(values)), ylab = '',xlab = '",vars,"', \n",
      "  main = '", vars, "') \n",
      "axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "curve(dnorm(x, mean = promedio, sd = desviacion), add=TRUE, col='blue', lwd=2)\n",
      "legend('bottom', legend = '", labelcurva, "', col = 'blue', lty=1, cex=1.5)"))
  }
}

#' default_calc_normal
#'
#' @description generates the code that creates the asymmetry table.
#'
#' @param data the name of the current data.
#' @param label.yes the label for when the asymmetry is positive.
#' @param label.no the label for when the asymmetry is negative.
#' @param label.without the label for when there is no asymmetry.
#'
#' @export
#'
#' @examples
#' x <- default_calc_normal('iris')
#' exe(x)
#' 
default_calc_normal <- function(data = "datos", label.yes = "Positiva", label.no = "Negativa", label.without = "Sin Asimetr\u00EDa") {
  return(paste0(
    "calc <- lapply(var_numerical(", data,"), function(i) fisher_calc(i)[1]) \n",
    "calc <- as.data.frame(calc) \n",
    "calc <- rbind(calc, lapply(calc, function(i) ifelse(i > 0, '", label.yes,
    "',\n  ifelse(i < 0, '", label.no, "', '", label.without, "')))) \n",
    "calc <- t(calc)\ncalc"))
}

# DISPERSION PAGE ---------------------------------------------------------------------------------------------------------

#' default_disp
#'
#' @param data the name of the current data.
#' @param vars a vector of length 2 or 3 with the names of the variables for the graph.
#' @param color the color of the dots on the chart.
#'
#' @export
#'
#' @examples
#' library(scatterplot3d)
#' 
#' x <- default_disp('iris', c('Sepal.Length', 'Sepal.Width'))
#' exe(x)
#' 
#' x <- default_disp('iris', c('Sepal.Length', 'Sepal.Width', 'Petal.Length'))
#' exe(x)
#' 
default_disp <- function(data = "datos", vars = NULL, color = "#FF0000AA"){
  if(length(vars) < 2) {
    return(NULL)
  } else if(length(vars) == 2) {
    return(paste0("ggplot2::ggplot(data = ", data, ", ggplot2::aes(x = ", vars[1], ", y = ", vars[2], ", label = rownames(", data, "))) +",
                  "ggplot2::geom_point(color = '", color, "', size = 3) + ggplot2::geom_text(vjust = -0.7) + ggplot2::theme_minimal()"))
  } else{
    return(paste0("scatterplot3d(", data, "[, '", vars[1], "'], ", data, "[, '",
                  vars[2], "'], ", data, "[, '", vars[3], "'], pch = 16, color = '", color, "')"))
  }
}

# DISTRIBUTIONS PAGE ------------------------------------------------------------------------------------------------------

#' def_code_num
#'
#' @param data the name of the current data.
#' @param variable the name of the variable for the numerical distribution chart.
#' @param color the color of the chart.
#'
#' @export
#'
#' @examples
#' x <- def_code_num('iris', 'Petal.Length')
#' exe(x)
#' 
def_code_num <- function(data = "datos", variable, color = 'red'){
  return(paste0("numerical_distribution(",data,"[, '",variable,"' ],'", variable,"',color = '", color,"')"))
}

#' def_code_cat
#'
#' @param data the name of the current data.
#' @param variable the name of the variable for the categorical distribution chart.
#'
#' @export
#'
#' @examples
#' x <- def_code_cat('iris', 'Species')
#' exe(x)
#' 
def_code_cat <- function(data = "datos", variable) {
  xlab = translate("cantidadcasos")
  ylab = translate("categorias")
  paste0("categorical_distribution(", data, "[, '", variable,"']) + ",
         "ggplot2::labs(title = '", variable, "', x = '",xlab, "', y = '", ylab, "')")
}

# CORRELATIONS PAGE -------------------------------------------------------------------------------------------------------

#' cor_model
#' 
#' @description generates the code to calculate the correlation matrix.
#'
#' @param data the name of the current data.
#'
#' @export
#'
#' @examples
#' x <- cor_model('iris')
#' exe(x)
#' correlacion
#' 
cor_model <- function(data = "datos"){
  return(paste0("correlacion <- cor(var_numerical(", data, "))"))
}

#' correlations_plot
#' 
#' @description generates the code of the correlation chart.
#'
#' @param method the visualization method of correlation matrix to be used. 
#' @param type display full matrix, lower triangular or upper triangular matrix.
#'
#' @seealso \code{\link[corrplot]{corrplot}}
#'
#' @export
#'
#' @examples
#' x <- cor_model('iris')
#' exe(x)
#' print(correlacion)
#' 
#' x <- correlations_plot()
#' exe(x)
#' 
correlations_plot <- function(method = 'circle', type = "lower"){
  return(paste0("corrplot::corrplot(correlacion, method='", method,"', shade.col=NA, tl.col='black',
                tl.srt=20, addCoef.col='black', order='AOE', type = '", type, "')"))
}

# PREDICTIVE POWER PAGE ---------------------------------------------------------------------------------------------------

#' pairs_power
#' 
#' @param data the name of the current data.
#'
#' @seealso \code{\link[psych]{pairs.panels}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(psych)
#' x <- pairs_power('iris')
#' exe(x)
#' }
#' 
pairs_power <- function(data = "datos"){ 
  paste0("pairs.panels(var_numerical(",data,"), bg='black', ellipses=FALSE, smooth=FALSE,",
         "lm=TRUE, cex=0.5, cex.main=0.1, pch=20, main='',",
         "hist.col = gg_color_hue(3)[3], oma=c(1,1,1,1) )")
}

# RL PAGE -----------------------------------------------------------------------------------------------------------------

#' rl_model
#' 
#' @description generates the code to create the linear regression model.
#'
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' 
#' @seealso \code{\link[stats]{lm}}
#'
#' @export
#'
#' @examples
#' x <- rl_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.rl)
#' 
rl_model <- function(data = "datos.aprendizaje", variable.pred = NULL, model.var = "modelo.rl"){
  return(paste0(model.var," <- lm(`",variable.pred,"`~., data = ",data,")"))
}

#' rl_prediction
#' 
#' @description generates the code to create the prediction of the linear regression model.
#'
#' @param data the name of the test data.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#'
#' @seealso \code{\link[stats]{predict}}
#'
#' @export
#'
#' @examples
#' x <- rl_model('iris', 'Petal.Length', 'model_rl')
#' exe(x)
#' print(model_rl)
#' 
#' x <- rl_prediction('iris', 'model_rl', 'my_prediction')
#' exe(x)
#' print(my_prediction)
#' 
rl_prediction <- function(data = "datos.prueba", model.var = "modelo.rl" , pred.var = "prediccion.rl") {
  return(paste0(pred.var, " <- predict(",model.var,", ",data,")"))
}

#' rl_coeff
#' 
#' @description generates the code to get the information of the coefficients of the linear regression model
#'
#' @param model.var  the name of the variable that stores the resulting model.
#'
#' @export
#'
#' @examples
#' x <- rl_model('iris', 'Petal.Length', 'model_rl')
#' exe(x)
#' print(model_rl)
#' 
#' x <- rl_coeff('model_rl')
#' exe(x)
#' 
#' print(df.rl)
#' print(r2)
#' 
rl_coeff <- function(model.var = "modelo.rl"){
  paste0("summ <- summary(",model.var,")\n",
         "df.rl <- as.data.frame(summ$coefficients)\n",
         "df.rl <- cbind(df.rl,  Importance = symnum(summ$coefficients[,4], corr = FALSE, na = FALSE, 
                                           cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                           symbols = c('***', '**', '*', '.', ' ')))\n",
         "df.rl <- as.data.frame(df.rl)\n",
         "r2    <- summ$r.square\n")
}

# RLR PAGE ----------------------------------------------------------------------------------------------------------------

#' rlr_model
#' 
#' @description generates the code to create the penalized regression model.
#'
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param cv.var the variable that stores the optimal lambda.
#' @param alpha the alpha parameter of the model.
#' @param standardize the standardize parameter of the model.
#'
#' @seealso \code{\link[glmnet]{glmnet}}, \code{\link[glmnet]{cv.glmnet}}
#'
#' @export
#'
#' @examples
#' library(glmnet)
#' x <- rlr_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.rlr)
#' 
rlr_model <- function(data = "datos.aprendizaje", variable.pred = NULL, model.var = "modelo.rlr", cv.var = "cv.glm", alpha = 0, standardize = TRUE){
  return(paste0("x <- model.matrix(`",variable.pred,"`~., ",data,")[, -1]\n",
                "y <- ",data,"[, '",variable.pred,"']\n",
                cv.var," <- cv.glmnet(x, y, standardize = ",standardize,", alpha = ",alpha,")\n",
                model.var," <- glmnet(x, y, standardize = ",standardize,", alpha = ",alpha,")"))
}

#' coef_lambda
#' 
#' @description generates the code to print the penalized regression coefficients.
#'
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param lambda a numerical value in case you don't want to use the optimal lambda.
#' @param cv.var the variable that stores the optimal lambda.
#'
#' @export
#'
#' @examples
#' library(glmnet)
#' x <- rlr_model('iris', 'Petal.Length')
#' exe(x)
#' 
#' x <- coef_lambda('iris','Petal.Length', 'modelo.rlr')
#' exe(x)
#' 
coef_lambda <- function(data = "datos.aprendizaje", variable.pred = NULL, model.var = "modelo.rlr", lambda = NULL,  cv.var = "cv.glm"){
  lambda <- ifelse(is.null(lambda), paste0(cv.var,"$lambda.min"), paste0("exp(",lambda,")"))
  paste0("x <- model.matrix(`",variable.pred,"`~., ",data,")[, -1]\n",
         "y <- ",data,"[, '",variable.pred,"']\n",
         "predict(",model.var,", s = ",lambda,", type = 'coefficients', exact = TRUE, x = x, y = y)")
}

#' plot_coef_lambda
#' 
#' @description generates the code to plot the penalized regression coefficients.
#'
#' @param model.var the name of the variable that stores the resulting model.
#' @param lambda a numerical value in case you don't want to use the optimal lambda.
#' @param cv.var the variable that stores the optimal lambda.
#'
#' @export
#'
#' @examples
#' library(glmnet)
#' x <- rlr_model('iris', 'Petal.Length')
#' exe(x)
#' 
#' x <- plot_coef_lambda('modelo.rlr')
#' exe(x)
#' 
plot_coef_lambda <- function(model.var = "modelo.rlr", lambda = NULL,  cv.var = "cv.glm"){
  lambda <- ifelse(is.null(lambda), paste0("log(",cv.var,"$lambda.min)"), lambda)
  paste0("plot(",model.var,", 'lambda', label = TRUE)\n",
         "abline(v = ",lambda,", col = 'blue', lwd = 2, lty = 3)")
}

#' rlr_prediction
#' 
#' @description generates the code to create the prediction of the penalized regression model.
#'
#' @param data.a the name of the learning data.
#' @param data.p the name of the test data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#' @param lambda a numerical value in case you don't want to use the optimal lambda.
#' @param cv.var the variable that stores the optimal lambda.
#'
#' @export
#'
#' @examples
#' library(glmnet)
#' x <- rlr_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.rlr)
#' 
#' x <- rlr_prediction('iris', 'iris', 'Petal.Length', pred.var = 'my_prediction')
#' exe(x)
#' print(my_prediction)
#' 
rlr_prediction <- function(data.a = "datos.aprendizaje", data.p = "datos.prueba",variable.pred = NULL, model.var = "modelo.rlr", 
                           pred.var = "prediccion.rlr", lambda = NULL,  cv.var = "cv.glm") {
  lambda <- ifelse(is.null(lambda),paste0(cv.var,"$lambda.min"), paste0("exp(",lambda,")") )
  paste0("x <- model.matrix(`",variable.pred,"`~., ",data.a,")[, -1]\n",
         "y <- ",data.a,"[, '",variable.pred,"']\n",
         "prueba <- ",data.p,"\n",
         "prueba[, '",variable.pred,"'] <- 0\n",
         "prueba <- model.matrix(`",variable.pred,"`~., prueba)[, -1]\n",
         pred.var," <- predict(",model.var,",newx = prueba,",
         "s = ",lambda,", exact = TRUE, x = x, y = y)")
}

#' rlr_type
#' 
#' @description returns the name of the penalty according to the alpha.
#'
#' @param alpha_rlr the penalty is defined as alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
#' 
#' @seealso \code{\link[glmnet]{glmnet}}
#'
#' @export
#'
#' @examples
#' rlr_type(1)
#' rlr_type(0)
#' 
rlr_type <- function(alpha_rlr = options_regressor("rlr.alpha")){
  alpha_rlr <- ifelse(is.null(unlist(alpha_rlr)), 0, alpha_rlr)
  ifelse(alpha_rlr == 0, "ridge", "lasso")
}

# KNN PAGE ----------------------------------------------------------------------------------------------------------------

#' kkn_model
#' 
#' @description generates the code to create the k nearest neighbors model.
#' 
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param scale the scale parameter of the model.
#' @param kmax the kmax parameter of the model.
#' @param kernel the kernel parameter of the model.
#' @param model.var the name of the variable that stores the resulting model.
#' @param distance the distance parameter of the model.
#' 
#' @seealso \code{\link[kknn]{train.kknn}}
#' 
#' @export
#'
#' @examples
#' library(kknn)
#' x <- kkn_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.knn)
#' 
kkn_model <- function(data = "datos.aprendizaje", variable.pred = NULL, scale = TRUE, kmax = 7, kernel = "optimal", model.var = "modelo.knn", distance = 2){
  kmax <- ifelse(!is.numeric(kmax), exe("round(sqrt(nrow(",data,"))"), kmax)
  return(paste0(model.var," <- train.kknn(`",variable.pred,"`~., data = ",data,", scale =",scale,", kmax=",kmax,", kernel = '",kernel,"', distance = ",distance,")"))
}

#' kkn_prediction
#'
#' @description generates the code to create the prediction of the k nearest neighbors model.
#'
#' @param data the name of the test data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#'
#'
#' @export
#'
#' @examples
#' library(kknn)
#' library(dplyr)
#' 
#' x <- kkn_model('iris', 'Petal.Length', model.var = 'model_knn')
#' exe(x)
#' print(model_knn)
#' 
#' x <- kkn_prediction('iris', 'Petal.Length', 'model_knn', 'my_prediction')
#' exe(x)
#' print(my_prediction)
#' 
kkn_prediction <- function(data = "datos.prueba", variable.pred = NULL, model.var = "modelo.knn", pred.var = "prediccion.knn") {
  return(paste0(pred.var," <- predict(",model.var,", ",data," %>% select(-`",variable.pred,"`))"))
}

# SVM PAGE ----------------------------------------------------------------------------------------------------------------

#' svm_model
#' 
#' @description generates the code to create the support vector machines model.
#'
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param scale the scale parameter of the model.
#' @param kernel the kernel parameter of the model.
#'
#' @seealso \code{\link[e1071]{svm}}
#'
#' @export
#'
#' @examples
#' library(e1071)
#' x <- svm_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.svm)
#' 
svm_model <- function(data = "datos.aprendizaje", variable.pred = NULL, model.var = "modelo.svm", scale = TRUE, kernel = "linear"){
  return(paste0(model.var," <- svm(`",variable.pred,"`~., data = ",data,", scale =",scale,", kernel = '",kernel,"')"))
}

#' svm_prediction
#' 
#' @description generates the code to create the prediction of the support vector machines model.
#'
#' @param data the name of the test data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#'
#' @export
#'
#' @examples
#' library(e1071)
#' library(dplyr)
#' 
#' x <- svm_model('iris', 'Petal.Length', model.var = 'model_svm')
#' exe(x)
#' print(model_svm)
#' 
#' x <- svm_prediction('iris', 'Petal.Length', 'model_svm', 'my_prediction')
#' exe(x)
#' print(my_prediction)
#' 
svm_prediction <- function(data = "datos.prueba", variable.pred = NULL, model.var = "modelo.svm", pred.var = "prediccion.svm"){
  return(paste0(pred.var," <- predict(",model.var," , ",data," %>% select(-`",variable.pred,"`))"))
}

# RD PAGE -----------------------------------------------------------------------------------------------------------------

#' rd_model
#'
#' @description generates the code to create the dimension reduction model.
#'
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param n.comp the name of the variable that stores the optimum number of components.
#' @param mode the method of dimension reduction is defined as mode=1 is the MCP, and mode=0 the ACP.
#' @param scale the scale parameter of the model.
#'
#' @seealso \code{\link[pls]{pcr}}, \code{\link[pls]{plsr}}
#'
#' @export
#'
#' @examples
#' library(pls)
#' 
#' x <- rd_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.rd)
#' 
rd_model <- function(data = "datos.aprendizaje", variable.pred = NULL, model.var = "modelo.rd",
                      n.comp = "n.comp.rd", mode = options_regressor("rd.mode"), scale = TRUE){
  mode <- ifelse(is.null(unlist(mode)), 0, mode)
  if(mode == 0){
    x <- paste0(model.var," <- pcr(`",variable.pred,"`~.,data = ",data,", scale = ",scale,", validation = 'CV')")
  }else{
    x <- paste0(model.var," <- plsr(`",variable.pred,"`~.,data = ",data,", scale = ",scale,", validation = 'CV')")
  }
  paste0(x,"\n",n.comp, " <- which.min(RMSEP(",model.var,")$val[1, 1, ]) - 1")
}

#' rd_prediction
#' 
#' @description generates the code to create the prediction of the dimension reduction model.
#'
#' @param data the name of the test data.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#' @param n.comp the name of the variable that stores the optimum number of components.
#' @param ncomp a numerical value in case you don't want to use the optimum number of components.
#'
#' @export
#'
#' @examples
#' library(pls)
#' 
#' x <- rd_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.rd)
#' 
#' x <- rd_prediction('iris', 'modelo.rd', 'my_prediction')
#' exe(x)
#' print(my_prediction)
#' 
rd_prediction <- function(data = "datos.prueba", model.var = "modelo.svm", pred.var = "prediccion.rd", 
                          n.comp = "n.comp.rd", ncomp = NULL) {
  ncomp <- ifelse(is.null(ncomp), n.comp, ncomp)
  paste0(pred.var," <- predict(",model.var,", ",data,", ncomp = ",ncomp,")")
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
rd_type <- function(mode.rd = options_regressor("rd.mode")){
  mode.rd <- ifelse(is.null(unlist(mode.rd)), 0, mode.rd)
  ifelse(mode.rd == 0, "ACP", "MCP")
}

# DT PAGE ------------------------------------------------------------------------------------------------------------

#' dt_model
#' 
#' @description generates the code to create the decision trees model.
#'
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param minsplit the minsplit parameter of the model.
#' @param maxdepth the maxdepth parameter of the model.
#'
#' @seealso \code{\link[rpart]{rpart}}
#'
#' @export
#'
#' @examples
#' library(rpart)
#' 
#' x <- dt_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.dt)
#' 
dt_model <- function(data = "datos.aprendizaje", variable.pred = NULL, model.var = "modelo.dt", minsplit =  20, maxdepth = 15){
  minsplit <- ifelse(!is.numeric(minsplit), 1, minsplit)
  maxdepth <- ifelse(!is.numeric(maxdepth) || maxdepth > 30, 15, maxdepth)
  codigo <- paste0(model.var," <- rpart(`",variable.pred,"`~., data = ",data,",
                   control = rpart.control(minsplit = ",minsplit,", maxdepth = ", maxdepth,"))")
  return(codigo)
}

#' dt_prediction
#'
#' @description generates the code to create the prediction of the decision trees model.
#'
#' @param data the name of the test data.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#'
#' @export
#'
#' @examples
#' library(rpart)
#' 
#' x <- dt_model('iris', 'Petal.Length', model.var = 'model_dt')
#' exe(x)
#' print(model_dt)
#' 
#' x <- dt_prediction('iris', 'model_dt', 'my_prediction')
#' exe(x)
#' print(my_prediction)
#' 
dt_prediction <- function(data = "datos.prueba", model.var = "modelo.dt", pred.var = "prediccion.dt") {
  return(paste0(pred.var," <- predict(",model.var,", ",data,")"))
}

#' dt_plot
#' 
#' @description makes the graph of the tree.
#'
#' @param model.var the name of the variable that stores the resulting prediction.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(rpart)
#' 
#' x <- dt_model('iris', 'Petal.Length', model.var = 'model_dt')
#' exe(x)
#' print(model_dt)
#' 
#' x <- dt_plot('model_dt')
#' exe(x)
#' }
dt_plot <- function(model.var = "modelo.dt"){
  return(paste0("rpart.plot::prp(",model.var,", type = 2, extra = 100, nn = TRUE, varlen = 0, faclen = 0,
                fallen.leaves = TRUE, branch.lty = 6, shadow.col = '#dedede',box.col = '#c8b028')"))
}

# RF PAGE ------------------------------------------------------------------------------------------------------------

#' rf_model
#' 
#' @description generates the code to create the random forest model.
#'
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param ntree the ntree parameter of the model.
#' @param mtry the mtry parameter of the model.
#'
#' @seealso \code{\link[randomForest]{randomForest}}
#'
#' @export
#'
#' @examples
#' library(randomForest)
#' x <- rf_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.rf)
#' 
rf_model <- function(data = "datos.aprendizaje", variable.pred = NULL, model.var = "modelo.rf", ntree = 500, mtry = 1){
  ntree <- ifelse(!is.numeric(ntree), 500, ntree)
  codigo <- paste0(model.var," <- randomForest(`",variable.pred,"`~., data = ",data,",importance = TRUE,",
                   " ntree =",ntree,",mtry =",mtry,")")
  return(codigo)
}

#' rf_prediction
#' 
#' @description generates the code to create the prediction of the random forest model.
#'
#' @param data the name of the test data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#'
#' @export
#'
#' @examples
#' library(randomForest)
#' library(dplyr)
#' 
#' x <- rf_model('iris', 'Petal.Length', model.var = 'model_rf')
#' exe(x)
#' print(model_rf)
#' 
#' x <- rf_prediction('iris', 'Petal.Length', 'model_rf', 'my_prediction')
#' exe(x)
#' print(my_prediction)
#'
rf_prediction <- function(data = "datos.prueba", variable.pred = NULL, model.var = "modelo.rf", pred.var = "prediccion.rf"){
  return(paste0(pred.var," <- predict(",model.var,", ",data," %>% select(-`",variable.pred,"`))"))
}

# BOOSTING PAGE ---------------------------------------------------------------------------------------------------------

#' boosting_model
#' 
#' @description generates the code to create the boosting model.
#'
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param n.trees the n.trees parameter of the model.
#' @param distribution the distribution parameter of the model.
#' @param shrinkage the shrinkage parameter of the model.
#'
#' @seealso \code{\link[gbm]{gbm}}
#'
#' @export
#' 
#' @examples
#' library(gbm)
#' library(dplyr)
#' 
#' x <- boosting_model('iris', 'Petal.Length')
#' exe(x)
#' print(modelo.boosting)
#' 
boosting_model <- function(data = "datos.aprendizaje", variable.pred = NULL, model.var = "modelo.boosting", n.trees = 50, distribution = "gaussian", shrinkage = 0.1){
  n.trees <- ifelse(!is.numeric(n.trees), 50, n.trees)
  shrinkage <- ifelse(!is.numeric(shrinkage), 0.1, shrinkage)
  extra.values <- calibrate_boosting(exe(data))
  
  if(is.null(extra.values)){
    codigo <- paste0(model.var,"<- gbm(`",variable.pred,
                     "`~ ., data = ",data,", distribution = '",
                     distribution,"', n.trees = ",n.trees,", shrinkage = ",shrinkage,")")
  }else{
    codigo <- paste0(model.var," <- gbm(`",variable.pred,
                     "`~ ., data = ",data,", distribution = '",
                     distribution,"', n.trees = ",n.trees,", shrinkage = ",shrinkage,",n.minobsinnode = ",extra.values[["n.minobsinnode"]],
                     ",bag.fraction = ",extra.values[["bag.fraction"]],")")
  }
  return(codigo)
}

#' boosting_prediction
#' 
#' @description generates the code to create the prediction of the boosting model.
#'
#' @param data the name of the test data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#' @param n.trees the n.trees parameter of the model.
#'
#' @seealso \code{\link[gbm]{gbm}}
#' 
#' @export
#'
#' @examples
#' library(gbm)
#' library(dplyr)
#' x <- boosting_model('iris', 'Petal.Length', "model_boosting")
#' exe(x)
#' print(model_boosting)
#' 
#' x <- boosting_prediction('iris', 'Petal.Length', 'model_boosting', 'my_prediction')
#' exe(x)
#' print(my_prediction)
#' 
boosting_prediction <- function(data = "datos.prueba", variable.pred = NULL, model.var = "modelo.boosting", pred.var = "prediccion.boosting", n.trees = 50) {
  n.trees <- ifelse(!is.numeric(n.trees), 50, n.trees)
  return(paste0(pred.var," <- predict(",model.var,
                ", ",data," %>% select(-`",variable.pred,"`), n.trees = ",n.trees,")"))
}

#' boosting_importance_plot
#' 
#' @description generates the code to make the graph of variable importance.
#'
#' @param model.var the name of the variable that stores the resulting model.
#' @param data the name of the learning data.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' library(gbm)
#' library(ggplot2)
#' library(forcats)
#' library(dplyr)
#' 
#' x <- boosting_model('iris', 'Petal.Length', "model_boosting")
#' exe(x)
#' 
#' x <- boosting_importance_plot('model_boosting', 'iris')
#' exe(x)
#' }
boosting_importance_plot <- function(model.var = "modelo.boosting", data = "datos.aprendizaje"){
  data <- exe(data)
  size.y <- ifelse(ncol(data) <= 25, 1.5, 1 - (ncol(data) - 25)/4 * 0.01 )
  size.y <- ifelse(size.y <= 0, 0.1, size.y)
  paste0("ggplot(summary(",model.var,"), aes(x = fct_reorder(var, rel.inf), y = rel.inf, fill = fct_reorder(var, rel.inf))) +\n",
         "geom_bar(stat = 'identity', position = 'identity', width = 0.1) +\n",
         "labs(title = '",translate("impVarRI"),"', y = '",translate("RI"),"', x = '') +\n",
         "scale_y_continuous(labels = scales::comma) + coord_flip() +\n",
         "theme(axis.text.x = element_text(angle = 45, hjust = 1),",
         "axis.text.y=element_text(size=rel(",size.y,")),legend.position='none')\n")
}

# NN PAGE ------------------------------------------------------------------------------------------------------------

#' nn_model
#'
#' @description generates the code to create the neural network model.
#'
#' @param data the name of the learning data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param mean.var the name of the variable that stores the mean of the columns.
#' @param sd.var the name of the variable that stores the standard deviation of the columns.
#' @param threshold the threshold parameter of the model.
#' @param stepmax the stepmax parameter of the model.
#' @param cant.hidden the quantity of hidden layers that are going to be used.
#' @param ... a vector with the number of nodes in each hidden layer.
#'
#' @seealso \code{\link[neuralnet]{neuralnet}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(neuralnet)
#' library(dummies)
#' 
#' x <- nn_model('iris', 'Petal.Length','modelo.nn', 'mean.nn', 'sd.nn', 0.05, 2000, 3, 30, 50, 80)
#' exe(x)
#' 
#' print(modelo.nn)
#' print(mean.nn)
#' print(sd.nn)
#' }
nn_model <- function(data = "datos.aprendizaje", variable.pred = NULL, model.var = "modelo.nn", mean.var = "mean.nn",
                     sd.var = "sd.nn", threshold = 0.01, stepmax = 1000, cant.hidden = 2, ...){
  threshold <- ifelse(threshold == 0, 0.01, threshold)
  stepmax <- ifelse(stepmax < 100, 100, stepmax)
  capas <- as_string_c(as.numeric(list(...)[1:cant.hidden]), quote = FALSE)
  
  paste0("datos.dummies.apren <- dummy.data.frame(",data,")\n",
         mean.var," <- sapply(datos.dummies.apren, mean)\n",
         sd.var," <- sapply(datos.dummies.apren, sd)\n",
         "datos.dummies.apren <- as.data.frame(scale(datos.dummies.apren, center = ",mean.var,", scale = ",sd.var,"))\n",
         "nombres <- colnames(datos.dummies.apren)\n",
         "formula.nn <- as.formula(paste('",variable.pred,"~', paste0(nombres[!nombres %in% '",variable.pred,"'], collapse = '+')))\n",
         model.var," <- neuralnet(formula.nn, data = datos.dummies.apren, hidden = ",capas,",\n\t\t\tlinear.output = TRUE,",
         "threshold = ",threshold,", stepmax = ",stepmax,")\n")
}

#' nn_prediction
#' 
#' @description generates the code to create the prediction of the neural network model.
#'
#' @param data the name of the test data.
#' @param variable.pred the name of the variable to be predicted.
#' @param model.var the name of the variable that stores the resulting model.
#' @param pred.var the name of the variable that stores the resulting prediction.
#' @param mean.var the name of the variable that stores the mean of the columns.
#' @param sd.var the name of the variable that stores the standard deviation of the columns.
#'
#' @seealso \code{\link[neuralnet]{compute}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(neuralnet)
#' library(dummies)
#' library(dplyr)
#' 
#' x <- nn_model('iris', 'Petal.Length','modelo.nn', 'mean.nn', 'sd.nn', 0.05, 2000, 3, 30, 50, 30)
#' exe(x)
#' 
#' x <- nn_prediction('iris', 'Petal.Length')
#' exe(x)
#' print(prediccion.nn)
#' }
nn_prediction <- function(data = "datos.prueba", variable.pred = NULL, model.var = "modelo.nn", pred.var = "prediccion.nn", mean.var = "mean.nn", sd.var = "sd.nn") {
  paste0("datos.dummies.prueb <- as.data.frame(scale(dummy.data.frame(",data," %>% select(-`",variable.pred,"`))))\n",
         "datos.dummies.prueb['",variable.pred,"'] <- NULL\n",
         pred.var," <- neuralnet::compute(",model.var,", datos.dummies.prueb)$net.result\n",
         pred.var, " <- ",pred.var," * ",sd.var,"['",variable.pred,"'] + ",mean.var,"['",variable.pred,"']")
}

#' nn_plot
#'
#' @description generates the code to create the graph of the neural network.
#'
#' @param model.var the name of the variable that stores the resulting model.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(neuralnet)
#' library(dummies)
#' library(dplyr)
#' 
#' x <- nn_model('iris', 'Petal.Length','modelo.nn', 'mean.nn', 'sd.nn', 0.05, 2000, 3, 10, 10, 10)
#' exe(x)
#' 
#' x <- nn_plot('modelo.nn')
#' exe(x)
#' }
nn_plot <- function(model.var = "modelo.nn"){
  paste0("plot(",model.var,", arrow.length = 0.1, rep = 'best', intercept = TRUE,x.entry = 0.1, x.out = 0.9,\n\t",
         "information=FALSE,intercept.factor = 0.8,col.entry.synapse='red',col.entry='red',col.out='green',col.out.synapse='green',\n\t",
         "dimension=15, radius = 0.2, fontsize = 10)")
}

# MODELS UTILITY FUNCTIONS ------------------------------------------------------------------------------------------------

#' disp_models
#' 
#' @description this function generates the call code of the scatter function.
#'
#' @param prediction the name of the prediction object.
#' @param model_name the name of the model.
#' @param var_pred the name of the variable to be predicted.
#' @param data the name of the current data.
#'
#' @export
#'
#' @examples
#' disp_models("prediction.knn", "KNN", "Species")
#' 
disp_models <- function(prediction, model_name, var_pred, data = "datos.prueba"){
  paste0("plot_real_prediction(",data,"[,'",var_pred,"'], ", prediction,", '",model_name,"')")
}