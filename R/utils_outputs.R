#' infoBoxPROMiDAT
#' 
#' @description create a box for the information tab
#' 
#' @details this function only work correctly on the server side because they need the css and js file.
#'
#' @param title title of the box
#' @param value the content of the box
#' @param icon icon of the box
#'
#' @return shiny.tag object
#' @noRd
#' 
infoBoxPROMiDAT <- function(title, value, icon) {
  tags$div(class = "info-box bg-promidat",
           tags$span(class = "info-box-icon", icon),
           tags$div(class="info-box-content",
                    tags$span(class = "info-box-text", title),
                    tags$span(class = "info-box-number", value)))
}



tabBoxPrmdt <- function (..., id = NULL, title = NULL, opciones = NULL) {
  content <- shiny::tabsetPanel(..., id = id, selected = NULL)
  content$attribs$class <- "nav-tabs-custom"
  
  if (!is.null(title)) {
    content$children[[1]] <- htmltools::tagAppendChild(
      content$children[[1]], tags$li(class = "header pull-right", title))
  }
  if (!is.null(opciones)) {
    pos <- length(content$children[[2]]$children[[1]]) + 1
    content$children[[2]]$children[[1]][[pos]] <- opciones
  }
  
  content
}

#' tabsOptions
#'
#' @description create tabs options on panels.
#' 
#' @details this function only work correctly on the server side because they need the css and js file.
#'
#' @param buttons vector or list with icons of each tab.
#' @param widths vector or list with widths of each tab.
#' @param heights vector or list with heights of each tab.
#' @param tabs.content list with the content of each tab.
#'
#' @return shiny.tag
#' @noRd
#' 
tabsOptions <- function(buttons = list(icon("gear"), icon("terminal")), widths = c(50, 100),
                        heights = c(100, 50), tabs.content = list("", "")){
  res <- ""
  codeButtons <- ""
  cant <- length(buttons)
  if(cant == 1) {widgets <- c("center")}
  if(cant == 2) {widgets <- c("left", "right")}
  if(cant == 3) {widgets <- c("left", "center", "right")}
  if(cant == 4) {widgets <- c("left", "centerleft", "centeright", "right")}
  if(cant == 5) {widgets <- c("left", "centerleft", "center", "centeright", "right")}
  for(i in 1:cant){
    res <- paste0(res, tags$div(class = paste0("box-option box-option-", widgets[i]),
                                style = paste0("width:", widths[i], "%;height:", heights[i], "%;"),
                                tabs.content[[i]]), "\n")
    codeButtons <- paste0(codeButtons, "<button style='width:", 100/cant, "%' data-widget='",
                          widgets[i], "'>", buttons[[i]], "</button>\n")
  }
  res <- paste0(res, tags$div(class = "btn-options", style = "position:relative;",width = "100%", HTML(codeButtons)))
  return(tags$div(HTML(res)))
}

#' render_index_table
#' 
#' @description creates a reactive table for indices panels.
#'
#' @param table the data.frame to be converted
#'
#' @noRd
#' 
render_index_table <- function(table){
  renderTable({table}, striped = TRUE, bordered = TRUE,  
              spacing = 'l', width = '100%',  digits = 5,
              align = 'c')
}

#' render_table_data
#' 
#' @description create a table for the shiny application and render it.
#'
#' @param data a data.frame to create a the table.
#' @param editable whether to make an editable table. The default value is TRUE.
#' @param dom define the table control elements to appear on the page and in what order.
#' @param pageLength the number of rows to show. The default value is 10.
#' @param scrollY the heigth of the table.
#' @param server whether to use server-side processing. If TRUE, then the data is kept on the server and the browser requests a page at a time; if FALSE, then the entire data frame is sent to the browser at once.
#' @param languaje string. ("es" for Spanish, "en" for English)
#'
#' @seealso  \code{\link[DT]{datatable}}, \code{\link[DT]{renderDT}}
#'
#' @return a shiny.render.function
#' @noRd
#'
render_table_data <- function(data, editable = TRUE, dom = "frtip", pageLength = 20, scrollY = "27vh", server = T, language = "es") {
  labelsNC <- ifelse(language == c("es", "es"), c("Num\u00E9rico","Categ\u00F3rico"), c("Numerical","Categorical"))
  data <- head(data, n = 100)
  nombre.columnas <- c("ID", colnames(data))
  tipo.columnas <- sapply(colnames(data), function(i) ifelse(class(data[,i]) %in% c("numeric", "integer"),
                                                             paste0("<span data-id='numerico'>", labelsNC[1], "</span>"),
                                                             paste0("<span data-id='categorico'>", labelsNC[2], "</span>")))
  tipo.columnas <- lapply(tipo.columnas, function(i)tags$th(HTML(i)))
  sketch <- withTags(table(DT::tableHeader(nombre.columnas),
                           tags$tfoot(tags$tr(tags$th(), tipo.columnas))))
  
  return(DT::renderDT(DT::datatable(data, selection = 'none', editable = editable,  container = sketch,
                                    options = list(dom = dom, pageLength = pageLength, scrollY = scrollY)), server = server))
}


#El parámetro idioma se utiliza para cuando se llama directamente la función
#Cuando se actualiza el idioma simplemente se cambian los nombres del encabezado a traves de los span data-id
dttable.custom <- function(df, decimals = NULL, translatable = FALSE, language = "es"){
  
  columnas.nombres <- c("ID",colnames(df))
  
  #Redondeos
  if(!is.null(decimals)){
    df <- data.frame(lapply(df, function(x) if(is.numeric(x)) round(x, decimals) else x),
                     row.names = row.names(df))
  }
  
  #Traducciones
  if(translatable){
    columnas.nombres <- sapply(columnas.nombres, function(c) return(HTML(paste0("<span data-id='",c,"'>",tr(c,language),"</span>"))))
  }
  
  #Bosquejo
  sketch <- withTags(table(DT::tableHeader(columnas.nombres, escape = FALSE)))
  
  return(DT::datatable(df,
                       rownames = TRUE,
                       selection = "none",
                       editable = FALSE,
                       escape  = FALSE,
                       container = sketch,
                       options = list(dom = "frtip", pageLength = 10, 
                                      columnDefs = list(
                                        list(orderable=TRUE, targets=0),
                                        list(className = 'dt-center', targets = "_all"))
                       )))
}

#' tb_predic
#' 
#' @description creates comparison table between prediction and real data (test data).
#' 
#' @param real a data.frame with the real values.
#' @param predic.var a vector with the prediction value.
#' @param languaje string. ("es" for Spanish, "en" for English)
#'
#' @noRd
#'
tb_predic <- function(real, predic.var, decimals = NULL, languaje = "es"){
  df   <- cbind(real, predic.var,  abs(real - predic.var))
  colns <- c(tr("reald",languaje), tr("pred",languaje), tr("dif",languaje))
  colnames(df) <- colns
  sketch <- htmltools::withTags(table(DT::tableHeader(c("ID",colns))))
  
  #Redondeo
  if(!is.null(decimals)){
    df <- round(df,decimals)
  }
  
  return(DT::datatable(df,
                       selection = "none",
                       editable = FALSE,
                       escape  = FALSE,
                       container = sketch,
                       options = list(dom = "frtip", pageLength = 10,
                                      columnDefs = list(
                                        list(orderable=TRUE, targets=0),
                                        list(className = 'dt-center', targets = "_all"))
                                      )
                       ))
}