#' correlacion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList 
#' 
mod_correlacion_ui <- function(id){
  ns <- NS(id)
  
  correlation.plot <- tabPanel(title = labelInput("correlacion"), value = "correlacion",
                               echarts4rOutput(ns('plot_cor'), height = "70vh"))
  
  results.table.correlations <- tabPanel(title = labelInput("resultados"), value = "cor.salida",
                                         div(style = "height: 60vh;overflow-y: scroll;",
                                             withLoader(verbatimTextOutput(ns("txt_cor")),
                                                        type = "html", loader = "loader4")))
  
  corr.plot.opc <- list(options.run(ns("run_cor")), tags$hr(style = "margin-top: 0px;"),
                        colourInput(ns("col_max"), labelInput("selcolor"), "#2E86C1",allowTransparent = T),
                        colourInput(ns("col_med"), labelInput("selcolor"), "#F8F5F5",allowTransparent = T),
                        colourInput(ns("col_min"), labelInput("selcolor"), "#FF5733",allowTransparent = T))
  
  corr.plot.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                           codigo.monokai(ns("fieldCodeCor"),  height = "24vh"))
  
  corr.table.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                          codigo.monokai(ns("fieldCodeCor2"),  height = "10vh"))
  
  tabs.plot <- tabsOptions(buttons = list(icon("cog"), icon("code")),heights = c(70, 50),
                           tabs.content = list(corr.plot.opc,corr.plot.code))
  
  tabs.table <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(40),
                            tabs.content = list(corr.table.code))
  
  tabs.options <- list(conditionalPanel("input.tabCor == 'correlacion'",tabs.plot,ns = ns),
                       conditionalPanel("input.tabCor != 'correlacion'",tabs.table,ns = ns))

  
  page.correlations <- tabItem(tabName = "correlacion",
                               tabBoxPrmdt(id = ns("tabCor"), opciones = tabs.options,
                                      correlation.plot,
                                      results.table.correlations))
  
  tagList(
    page.correlations
  )
}
    
#' correlacion Server Function
#' @keywords internal
#' 
mod_correlacion_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  # Gráfico de Correlaciones
  output$plot_cor <- renderEcharts4r({
    input$run_cor
    datos <- var.numericas(updateData$datos)
    col_min <- isolate(input$col_min)
    col_med <- isolate(input$col_med)
    col_max <- isolate(input$col_max)
    colores <- list(col_min, col_med, col_max)
    
    tryCatch({
      cod <- code.cor(colores)
      updateAceEditor(session, "fieldCodeCor", value = cod)
      
      datos.plot <- round(cor(datos), updateData$decimals)
      e_cor(datos.plot, colores)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  # Resultados numéricos de Correlaciones
  output$txt_cor <- renderPrint({
    cod <- "cor(var.numericas(datos))"
    updateAceEditor(session, "fieldCodeCor2", value = cod)
    print(cor(var.numericas(updateData$datos)))
  })
}
    
## To be copied in the UI
# mod_correlacion_ui("correlacion_ui_1")
    
## To be copied in the server
# callModule(mod_correlacion_server, "correlacion_ui_1")
 
