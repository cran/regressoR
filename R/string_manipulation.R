
load("inst/extdata/translation.bin") # Load translation.bin (dictionary to change language)
enc <- ifelse(toupper(.Platform$OS.type) != "WINDOWS", "utf8", "UTF-8")
options_regressor(language = "es")

#' exe
#' 
#' @description concat and execute a text in R.
#'
#' @param ... one or more texts to be concatenated and executed.
#' @param envir the environment in which expr is to be evaluated.
#' 
#' @return the result of the execute.
#' @export
#'
#' @examples
#' exe("5+5")
#' exe("5","+","5")
#' exe("plot(iris$Species)")
#' 
exe <- function(..., envir = options_regressor()$exe.envir){
  envir <- if(is.null(envir) || !is.environment(envir)) parent.frame() else envir
  eval(parse(text = paste0(...)), envir = envir)
}

#' extract_code
#' 
#' @description gets the code of a function in text form.
#'
#' @param funcion the name of the function to be extracted.
#' @param envir the environment in which expr is to be evaluated.
#'
#' @export
#'
#' @examples
#' extract_code("cat")
#' extract_code("plot")
#' 
#' parse(text = extract_code("plot"))
#' 
extract_code <- function(funcion, envir = parent.frame()) {
  code <- paste(deparse(exe(funcion, envir = envir)), collapse = "\n")
  code <- paste(funcion, "<-", code)
  return(code)
}

#' as_string_c
#' 
#' @description creates a string representative of a vector
#'
#' @param vect a vector with values
#' @param quote a logical value. If TRUE, the values on the vector will be surrounded by quotes.
#' 
#' @export
#' 
#' @examples
#' as_string_c(c("A", "B", "C"))
#' as_string_c(c(5, 6, 7))
#' as_string_c(c(5, 6, 7), quote = FALSE)
#' as_string_c(iris$Species)
#'
as_string_c <- function(vect, quote = TRUE){
  if(quote){
    return(paste0("c('",paste0(vect, collapse = "','"),"')"))
  }
  else{
    return(paste0("c(",paste0(vect, collapse = ","),")"))
  }
}

#' translate
#' 
#' @description translates text id into current language.
#' 
#' @param text the id for the text.
#' @param language the language to choose. It can be "es" or "en".
#' 
#' @export
#' @examples
#' translate("knnl")
#' translate("knnl", "en")
#' 
translate <- function(text, language = options_regressor("language")) {
  if(is.null(language) || !any(language %in% c("es", "en"))){
    language <- "es"
  }
  language <- as.character(language)
  sapply(text, function(s) {
    elem <- ifelse(is.null(translation[[s]][[language]]), s, translation[[s]][[language]])
    Encoding(elem) <- enc
    elem
  }, USE.NAMES = F)
}

#' models_mode
#' 
#' @description transforms the names of a list from key-mode form to value-mode form.
#'
#' @param list.names a list whose names function as keys for \code{\link[regressoR]{translate}}. The names have to have the key-mode form.
#' 
#' @export
#'
#' @examples
#' x <- list('knnl-mode1' = 1, 'knnl-mode2' = 2, 'knnl-mode2' = 5)
#' models_mode(x)
#' 
models_mode <- function(list.names = list()){
  if(length(list.names) == 0) {
    return("---X---")
  }
  nombres <- c()
  for (nom in names(list.names)){
    nom.aux <- unlist(strsplit(nom, "-"))
    nombres <- c(nombres,ifelse(length(nom.aux) == 1,
                                translate(nom.aux),
                                paste0(translate(nom.aux[1]),"-",nom.aux[2])))
  }
  return(nombres)
}
