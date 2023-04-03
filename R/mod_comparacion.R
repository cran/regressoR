#' comparacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_comparacion_ui <- function(id){
  ns <- NS(id)

  tagList(
    tabItem(tabName = "comparar",
            tabBox(id = ns("BoxCom"), width = NULL, height ="80%",
                   tabPanel(title = labelInput("tablaComp"),
                            withLoader(DT::dataTableOutput(ns("TablaComp"), height="70vh"), type = "html", loader = "loader4"))))
  )
}

#' comparacion Server Function
#'
#' @noRd 
mod_comparacion_server <- function(input, output, session, updateData, modelos, codedioma){
  ns <- session$ns
  
  #Muestra la tabla comparativa.
  output$TablaComp <- DT::renderDataTable({
    tryCatch({
      idioma <- codedioma$idioma
      df     <- data.frame()
      for(modelName in names(modelos)){
        if(!is.null(modelos[[modelName]])){
          for (subModelName in names(modelos[[modelName]])) {
            modelo     <- modelos[[modelName]][[subModelName]]
            nombreFila <- paste0(tr(modelName, idioma),
                                 ifelse(is.null(modelo$id),"",paste0("-",modelo$id)))
            df.aux <- data.frame(modelo$indices,row.names = nombreFila)
            df     <- rbind.data.frame(df,df.aux)
          }
        }
      }
      
      if(dim(df)[1] > 0){
        colnames(df) <- c(tr("RMSE", idioma), tr("MAE", idioma), tr("ER", idioma), tr("correlacion", idioma))
        #Ordenamos por la primera columna(menor a mayor)
        df <- df[order(df[,1]),]
        
        dttable.custom(df, decimals = updateData$decimals)
      }
      else{NULL}
    }, 
    error = function(e){
      showNotification(paste0("Error: ", e), duration = 10, type = "error")
      NULL
    })
  },server = FALSE)
  
}
    
## To be copied in the UI
# mod_comparacion_ui("comparacion_ui_1")
    
## To be copied in the server
# callModule(mod_comparacion_server, "comparacion_ui_1")
 
