load("inst/app/lang/translation.bin") # Load translation.bin (dictionary to change language)
enc <- "utf8"

cambiar.labels <- function(){
  c("confg","ndec","ID","idioma","selidioma","data","basico","resumen","normalidad",
    "dispersion","distribucion","correlacion","coeff","poderpred","reporte",
    "aprendizaje","acercade","comparacion","predicnuevos","knn","dt",
    "rf","boost","svm","cargar","header","Rownames","eliminana","si","no",
    "cargarchivo","subir","trans","aplicar","separador","coma","puntocoma",
    "tab","separadordec","punto","subir","configuraciones","semilla",
    "habilitada","deshabilitada","seleccionarPredecir","propA","propP",
    "generar","descargar","dataA","dataP","numerico","categorico","disyuntivo",
    "resumenvar","selvar","plotnormal","opciones", "selcolor","selvars",
    "selcolores","codigo","codedist","numericas","categoricas","ejecutar",
    "selmetodo","seltipo","resultados","distpred","distpredcat","pares",
    "denspred","generatem","predm","mc","indices","gclasificacion","garbol",
    "reglas","evolerror","varImp","selkernel","kv","escal","minsplit",
    "maxdepth","splitIndex","numTree","numVars","ruleNumTree","selectAlg",
    "rocCurva","tablaComp","selectMod","selectCat", "reporte","titulo",
    "nombre","codreporte","salida","copyright","info","version","cargarNuev",
    "cargarDatos","transDatos","seleParModel","generarM","variables","tipo",
    "activa","nn","xgb","selbooster","selnrounds","selectCapas","threshold",
    "stepmax","redPlot","rl","rlr","posibLanda","coeff","gcoeff","manual",
    "automatico","log_landa","shrinkage","resumenVarPre", "R2", "distknn",
    "ncomp", "rd", "RdPred", "RdVarPred", "errRDnCom", "RMSE","eliminar", "imputar",
    "selcolbar","selcolline","selcolpoint","selcolline","tasim","pvalue","asimetria","sigue",
    "atipicos")
}


tr <- function(text, idioma = "es") {
  sapply(text, function(s) {
    elem <- ifelse(is.null(translation[[s]][[idioma]]), s,
                   translation[[s]][[idioma]])
    Encoding(elem) <- "utf8"
    
    elem
  }, USE.NAMES = F)
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
#' translate("knn")
#' translate("knn", "en")
#' 
translate <- function(text, language = "es") {
  if(is.null(language) || !any(language %in% c("es", "en"))){
    language <- "es"
  }
  language <- as.character(language)
  sapply(text, function(s) {
    elem <- ifelse(is.null(translation[[s]][[language]]), s, translation[[s]][[language]])
    Encoding(elem) <- "utf8"
    elem
  }, USE.NAMES = F)
}



dropNulls <- function (x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}




updateLabelInput <- function (session, labelid, value = NULL) {
  message <- dropNulls(list(labelid = labelid))
  if(length(labelid) == 1) {
    labelid <- list(labelid)
  }
  ifelse(
    is.null(value), sentvalue <- labelid,
    ifelse(length(value) == 1, sentvalue <- list(value),
           sentvalue <- value))
  session$sendCustomMessage(
    type = 'updateLabel',
    message = list(ids = labelid, values = sentvalue))
}



# crear.traslation <- function() {
#   library(plyr)
#   archivo <- read.table("diccionario.csv", header = TRUE, sep = ";", as.is = TRUE)
#   translation <- dlply(archivo , .(key), function(s) key = as.list(s))
# 
#   save(translation, file = "translation.bin")
# }