#' Predictive_Power UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Predictive_Power_ui <- function(id){
  ns <- NS(id)
  
  # PREDICTIVE POWER PAGE ---------------------------------------------------------------------------------------------------
  
  code.power.num <- list(h3(labelInput("codigo")), hr(),
                         aceEditor(ns("fieldCodePoderNum"), mode = "r", theme = "monokai",
                                   value = "", height = "7vh", readOnly = F, autoComplete = "enabled"))
  
  
  tabs.power.num <- tabsOptions(buttons = list(icon("terminal")), widths = 100, heights = 55,
                                tabs.content = list(code.power.num))
  
  power.plot.pairs <- tabPanel(title = labelInput("pares"), value = "predpares",
                               withLoader(plotOutput(ns('plot_pairs_poder'), height = "75vh"),
                                          type = "html", loader = "loader4"))
  
  pagina.poder <- tabItem(tabName = "poderPred",
                          tabBox(id = ns("BoxPodPred"), width = NULL,
                                 power.plot.pairs,
                                 tabs.power.num))
  
  
  
  tagList(
    pagina.poder
  )
}
    
#' Predictive_Power Server Function
#'
#' @noRd 
mod_Predictive_Power_server <- function(input, output, session, updateData){
  ns <- session$ns
  # Show the graph of numerical predictive power
  output$plot_pairs_poder <- renderPlot({
    tryCatch({
      updateData$datos.aprendizaje
      updateAceEditor(session, "fieldCodePoderNum", value = "pairs_power(datos)")
      if (ncol(var.numericas(updateData$datos)) >= 2) {
        if(ncol(var.numericas(updateData$datos)) <= 25){
          pairs_power(updateData$datos, decimals = updateData$decimals)
        }else{
          showNotification(tr("bigPlot",updateData$idioma), duration = 10, type = "message")
          NULL
        }
      }else{
        NULL
      }
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e),duration = 10,type = "error")
      NULL
    })
  })
}
    
## To be copied in the UI
# mod_Predictive_Power_ui("Predictive_Power_ui_1")
    
## To be copied in the server
# callModule(mod_Predictive_Power_server, "Predictive_Power_ui_1")
 
