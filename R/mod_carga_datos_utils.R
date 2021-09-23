# -------------------  Datos
datos <<- NULL

var.numericas <- function(data) {
  if(is.null(data)) return(NULL)
  subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
}

var.categoricas <- function(data) {
  if(is.null(data)) return(NULL)
  subset(data, select = !sapply(data, class) %in% c('numeric', 'integer'))
}


selectInputTrans <- function(datos, var, session, idioma = "es") {
  tags$select(
    id = session$ns(paste0("sel", var)),
    tags$option(value = "categorico", tr("categorico", idioma)),
    if(class(datos[, var]) %in% c("numeric", "integer")) {
      tags$option(value = "numerico", tr("numerico", idioma), 
                  selected = 'selected')
    } else {
      tags$option(value = "numerico", tr("numerico", idioma))
    },
    tags$option(value = "disyuntivo", tr("disyuntivo", idioma))
  )
}

#' Create disjunctive columns to a data.frame.
#'
#' @param data a data.frame object.
#' @param var the column name to apply disjunctive code.
#'
#' @author Diego Jimenez <diego.jimenez@promidat.com>
#' @return data.frame
#' @export datos.disyuntivos
#' @examples
#' datos.disyuntivos(iris, "Species")
#' 
datos.disyuntivos <- function(data, var) {
  if(is.null(data)) {
    return(NULL)
  }
  
  for (categoria in unique(data[, var])) {
    nueva.var <- as.numeric(data[, var] == categoria)
    data[, paste0(var, '.', categoria)] <- nueva.var
  }
  
  return(data)
}

#Obtiene los nombres de columnas o regresa un string vacio
colnames.empty <- function(res){
  res <- colnames(res)
  if(is.null(res))
    return("")
  return(res)
}

#Obtiene los nombres de columnas o regresa un string vacio
borrar.modelos <- function(updateData){
  updateData$datos.prueba         <- NULL
  updateData$datos.aprendizaje    <- NULL
  updateData$variable.predecir    <- NULL
}

# Borra los datos de los modelos
borrar.datos.modelos <- function(flag.datos = TRUE) {
  if (flag.datos) {

  }
}

accion.NAs <- function(datos, deleteNA = T) {
  if(deleteNA) {
    return(na.omit(datos))
  } else {
    moda <- function(x) x[which.max(summary(x))]
    
    for (var in colnames(datos)) {
      if(any(is.na(datos[, var]))) {
        if(class(datos[, var]) %in% c('numeric', 'integer')) {
          datos[, var][is.na(datos[, var])] <- mean(datos[, var], na.rm = T)
        } else {
          datos[, var][is.na(datos[, var])] <- moda(datos[, var])
        }
      }
    }
    return(datos)
  }
}

carga.datos <- function(nombre.filas = T, ruta = NULL, separador = ";",
                        sep.decimal = ",", encabezado = T, deleteNA = T) {
  if(!is.null(ruta)) {
    ruta <- gsub("\\", "/", ruta, fixed = T)
  }
  if(nombre.filas) {
    res <- read.table(
      ruta, header = encabezado, sep = separador, dec = sep.decimal,
      stringsAsFactors = T, row.names = 1)
  } else {
    res <- read.table(
      ruta, header = encabezado, sep = separador, dec = sep.decimal,
      stringsAsFactors = T)
  }
  return(accion.NAs(res, deleteNA))
}

carga.datos.np <- function(nombre.filas = T, ruta = NULL, separador = ";",
                        sep.decimal = ",", encabezado = T) {
  if(!is.null(ruta)) {
    ruta <- gsub("\\", "/", ruta, fixed = T)
  }
  if(nombre.filas) {
    res <- read.table(
      ruta, header = encabezado, sep = separador, dec = sep.decimal,
      stringsAsFactors = T, row.names = 1)
  } else {
    res <- read.table(
      ruta, header = encabezado, sep = separador, dec = sep.decimal,
      stringsAsFactors = T)
  }
  return(res)
}

segmentar.datos <- function(datos,porcentaje = 30, semilla = 5, perm.semilla = FALSE){
  semilla <- ifelse(is.numeric(semilla), semilla, 5)
  
  if (perm.semilla) {
    set.seed(semilla)
  }else{
    rm(.Random.seed, envir = globalenv())
  }
  particion <- sample(1:nrow(datos),size = nrow(datos)*porcentaje/100, replace = FALSE)
  test <- datos[-particion,]
  train <- datos[particion,]
  if (perm.semilla) 
    set.seed(semilla)
  
  return(list(test = test, train = train))
}

############################### Generar Código ################################
code.carga <- function(nombre.filas = T, ruta = NULL, separador = ";",
                       sep.decimal = ",", encabezado = T, incluir.NA = F) {
  res <- paste0(
    "datos <- read.table(stringsAsFactors = T, '", ruta, "', header=", encabezado, 
    ", sep='", separador, "', dec = '", sep.decimal, "'", 
    ifelse(nombre.filas, ", row.names = 1", ""), ")")
  res <- paste0(res, "\n", code.NA(incluir.NA))
  return(res)
}

code.NA <- function(deleteNA = T) {
  res <- ifelse(
    deleteNA, "datos <- na.omit(datos)\n",
    paste0(
      "Mode <- function(x) {\n  x[which.max(summary(x))]\n}\n",
      "for (var in colnames(datos)) {\n",
      "  if(any(is.na(datos[, var]))){\n",
      "    if(class(datos[, var]) %in% c('numeric', 'integer')) {\n",
      "      datos[, var][is.na(datos[, var])] <- mean(datos[, var], na.rm = T)\n",
      "    } else {\n",
      "      datos[, var][is.na(datos[, var])] <- Mode(datos[, var])\n",
      "    }\n  }\n}"))
  return(res)
}

code.trans <- function(var, nuevo.tipo) {
  if(nuevo.tipo == "categorico"){
    return(paste0(
      "datos[['", var, "']] <- as.factor(datos[['", var, "']])\n"))
  } else if(nuevo.tipo == "numerico") {
    return(paste0(
      "datos[['", var, "']] <- as.numeric(sub(',', '.', datos[['",
      var, "']], fixed = TRUE))\n"))
  } else {
    return(paste0(
      "datos <- datos.disyuntivos(datos, '", var,"')\n", 
      "datos[['", var, "']] <- NULL\n"))
  }
}
 
# Genera el codigo para las particiones para training- testing
code.segment <- function(p = 30, variable = NULL, semilla = 5, perm.semilla = FALSE){
  semilla <- ifelse(is.numeric(semilla), semilla, 5)
  codigo <- ifelse(perm.semilla, paste0("set.seed(",semilla,")"), "rm(.Random.seed, envir = globalenv())")
  
  codigo <- paste0(codigo,
                   "\nvariable.predecir <- '",variable,
                   "'\nparticion <- sample(1:nrow(datos),size = nrow(datos)*",p/100,
                   ", replace = FALSE)\n",
                   "datos.prueba <- datos[-particion,]\ndatos.aprendizaje <- datos[particion,]")
  
  codigo <- ifelse(perm.semilla, paste0(codigo, "\nset.seed(",semilla,")"),codigo)
  return(codigo)
}

