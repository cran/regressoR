
env_report <- new.env()
env_report$codigo.reporte <- list()

#' get_env_report
#'
#' @description gets the environment where the list is found with the report.
#'
#' @export
#'
#' @examples
#' e <- get_env_report()
#' e$codigo.reporte
#' 
get_env_report <- function(){
  return(env_report)
}

#' clean_report
#'
#' @description clean the full report.
#'
#' @export
#'
#' @examples
#' new_report(iris, 'iris')
#' get_report()
#' clean_report()
#' get_report()
#' 
clean_report <- function(){
  env_report$codigo.reporte <- list()
}

#' get_report
#'
#' @description gets the list of report values.
#'
#' @export
#'
#' @examples
#' get_report()
#' 
get_report <- function(){
  return(env_report$codigo.reporte)
}

#' len_report
#'
#' @description gets the size of the current report.
#'
#' @keywords internal
#' 
len_report <- function(){
  length(env_report$codigo.reporte)
}

#' chunk
#' 
#' @description locks the data into a rmarkdown chunk.
#'
#' @param content the content to be inserted.
#'
#' @keywords internal
#' 
chunk <- function(content = ""){
  paste0("```{r}\n", content, "\n```")
}

#' new_report
#' 
#' @description creates a new report section within the list. All new reports section store data and data names as headers.
#'
#' @param data the data that is stored in the report list
#' @param name the name of the stored data
#'
#' @export
#'
#' @examples
#' new_report(iris, "iris")
#' get_report()
#' clean_report()
#' 
new_report <- function(data, name = ""){
  n <- len_report() + 1
  env_report$codigo.reporte[[n]] <- list(datos.originales = data)
  env_report$codigo.reporte[[n]][["carga.datos"]] <- paste0("\n## Carga de Datos (",name,")",
                                                             "\n```{r}\ndatos.originales <- codigo.reporte[[",n,"]]$datos.originales\n",
                                                             "datos <- datos.originales\n```\n```{r}\nhead(datos)\n```\n```{r}\nstr(datos)\n```\n",
                                                             "```{r}\nIndicesM <- list()\n```\n")
}

#' insert_report
#' 
#' @description inserts an element in the report in the current section.
#'
#' @param id a string with the key of what is inserted in the report.
#' @param title the title of the content, if there is no title is NA.
#' @param ... the content to be inserted.
#' @param interpretation a logical value indicating whether a label has to be inserted for interpretation.
#' @param is.chunk a logical value indicating whether the content has to be enclosed in a chunk.
#' @param add a logical value indicating if the content has to be added to what is a before.
#' 
#' @export
#'
#' @examples
#' new_report(iris, "iris")
#' insert_report("1_part", 'Title 1', 'head(iris)\n', 'summary(iris)')
#' get_report()
#' clean_report()
#' 
insert_report <- function(id, title = NA, ... ,  interpretation =  TRUE, is.chunk = TRUE, add = FALSE){
  n <- len_report()
  content <- paste0(...)
  title <- ifelse(is.na(title), "\n\n", paste0("\n##  ", title, "\n"))
  content <- ifelse(is.chunk, chunk(content), content)
  inter <- ifelse(interpretation, "\n\n#### Interpretaci\u00F3n:\n", "\n")
  if(!add){
    env_report$codigo.reporte[[n]][[id]] <- ifelse(interpretation, paste0(title, content, inter), paste0(title, content,"\n"))
  }else{
    aux <- env_report$codigo.reporte[[n]][[id]]
    env_report$codigo.reporte[[n]][[id]] <- paste0(aux , "\n", title, content, inter)
  }
}


#' remove_report_elem
#' 
#' @description removes an element from the report according to its key in the current section.
#'
#' @param id a string with the key of what is removed in the report.
#'
#' @export
#'
#' @examples
#' new_report(iris, 'iris')
#' insert_report('1_part', 'Title 1', 'head(iris)\n', 'summary(iris)')
#' get_report()
#' remove_report_elem('1_part')
#' get_report()
#' clean_report()
#' 
remove_report_elem <- function(id){
  n <- len_report()
  env_report$codigo.reporte[[n]][[id]] <- NULL
}

#' new_section_report
#'
#' @description creates a new section in the report, this way you can overwrite keys and delete an element only affects the current section.
#'
#' @export
#'
#' @examples
#' new_report(iris, 'iris')
#' insert_report('1_part', 'Title 1', 'head(iris)\n', 'summary(iris)')
#' get_report()
#' 
#' remove_report_elem('1_part')
#' get_report()
#' 
#' new_section_report()
#' insert_report('1_part', 'Title 1', 'head(iris)\n', 'summary(iris)')
#' get_report()
#' 
#' new_section_report()
#' insert_report('1_part', 'Title 1', 'head(iris)\n', 'summary(iris)')
#' get_report()
#' 
#' remove_report_elem('1_part')
#' get_report()
#' 
#' clean_report()
#' 
new_section_report <- function(){
  n <- len_report() + 1
  env_report$codigo.reporte[[n]] <- list()
}


#' order_report
#' 
#' @description sort the report list according to the 'regressoR' logic.
#'
#' @param list_report la lista del con los elementos del reporte.
#'
#' @keywords internal
#'
order_report <- function(list_report){
  nombres <- names(list_report)
  order <- c("carga.datos","na.delete","transformar.datos","segmentar.datos","resumen",
             nombres[grepl("normalidad.", nombres)],
             nombres[grepl("dispersion.", nombres)],
             nombres[grepl("dya.num.", nombres)],
             nombres[grepl("dya.cat.", nombres)],
             "correlacion","poder.pred",
             nombres[grepl("poder.cat.", nombres)],
             "poder.num",nombres[grepl("poder.den.", nombres)],
             "modelo.rl","coeff.rl","pred.rl","disp.rl","ind.rl",
             combine_names(c("modelo.rlr","posib.landa.rlr","coeff.landa.rlr","gcoeff.landa.rlr","pred.rlr","disp.rlr","ind.rlr"),
                           c("ridge", "lasso")),
             combine_names(c("modelo.knn","pred.knn","disp.knn","ind.knn"),
                           c("optimal", "rectangular", "triangular","epanechnikov",
                             "biweight","triweight", "cos","inv","gaussian")),
             combine_names(c("modelo.svm","pred.svm","disp.svm","ind.svm"),
                           c("linear", "polynomial", "radial","sigmoid")),
             combine_names(c("modelo.rd","rmse.rd","plot.pred.rd","plot.var.pred.rd","pred.rd","disp.rd","ind.rd"),
                              c("ACP", "MCP")),
             "modelo.dt","modelo.dt.graf","pred.dt",
             "disp.dt","ind.dt","modelo.dt.rules",
             "modelo.rf","modelo.rf.graf",
             "pred.rf","disp.rf","ind.rf",
             nombres[grepl("modelo.rf.rules.", nombres)],
             combine_names(c("modelo.b","modelo.b.imp","pred.b","disp.boosting","ind.b"),
                           c("gaussian", "laplace", "tdist")),
             "modelo.nn", "modelo.nn.graf", "pred.nn", "disp.nn", "ind.nn",
             "tabla.comparativa")
  
  order <- c(order, nombres[!(nombres %in% order)])
  
  list_report <- list_report[order]
  list_report <- list_report[!as.logical(lapply(list_report, is.null))]
  return(list_report)
}


#' word_report
#' 
#' @description creates a header for the report that allows you to generate a word file.
#'
#' @param title report title.
#' @param name name of the author of the report.
#' @param order_by_regressor it's the order for the default "regressoR" report.
#' @param extra a string with any extra code you want to add to the configuration chunk.
#'
#' @export
#' 
#' @examples
#' new_report(iris, 'iris')
#' 
#' new_section_report()
#' insert_report('1_part', 'Title 1', 'head(iris)\n', 'summary(iris)')
#' 
#' new_section_report()
#' insert_report('1_part', 'Title 1', 'head(iris)\n', 'summary(iris)')
#' 
#' word_report(order_by_regressor = FALSE)
#' 
word_report <- function(title = "Sin Titulo", name = "PROMiDAT", order_by_regressor = TRUE, extra = "") {
  codigo.usuario <- ""
  codigos <- env_report$codigo.reporte
  
  for (list_r in codigos) {
    if(order_by_regressor){
      list_r <- order_report(list_r)
    }
    for (codigo in list_r) {
      if(!is.data.frame(codigo)){
        codigo.usuario <- paste0(codigo.usuario, codigo)
      }
    }
  }
  
  paste0(
    "---\n", "title: '", title, "'\n", "author: '", name, "'\n",
    "date: ", Sys.Date(), "\n", "output:\n  word_document:\n",
    "    df_print: paged\n---\n\n",
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE,  fig.height = 10, fig.width = 15)\n",
    "```\n\n",
    "```{r message=FALSE, warning=FALSE}\n",
    "library(promises)\nlibrary(ggplot2)\nlibrary(neuralnet)\n",
    "library(corrplot)\nlibrary(rattle)\n",
    "library(stringr)\nlibrary(gbm)\nlibrary(DT)\nlibrary(glmnet)\n",
    "library(kknn)\nlibrary(e1071)\nlibrary(rpart)\n",
    "library(rpart.plot)\nlibrary(randomForest)\nlibrary(ada)\nlibrary(xgboost)\n",
    "library(dplyr)\nlibrary(forcats)\nlibrary(knitr)\n",
    "library(xtable)\n",
    "kt<-function(x)\n{\nif(class(x)!=\"table\")\nreturn(kable(as.data.frame(x),align = \"l\"))\nelse\nreturn(kable(x,align =\"l\"))\n}\n",
    extra,"\n",
    "```\n\n",
    codigo.usuario)
}



