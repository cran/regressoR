#' normal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @importFrom shiny NS tagList
#' @noRd
#'
mod_normal_ui <- function(id){
  ns <- NS(id)
  
  normal.opc <- list(options.run(ns("run.normal")), tags$hr(style = "margin-top: 0px;"),
                     conditionalPanel("input.BoxNormal == 'tabNormalPlot'",
                                      colourInput(ns("col_hist_bar"), labelInput("selcolbar"),value = "steelblue", allowTransparent = T),
                                      colourInput(ns("col_hist_line"), labelInput("selcolline"),value = "#555555", allowTransparent = T),
                                      ns = ns
                     ),
                     conditionalPanel("input.BoxNormal == 'tabQPlot'",
                                      colourInput(ns("col_qq_point"), labelInput("selcolpoint"),value = "steelblue", allowTransparent = T),
                                      colourInput(ns("col_qq_line"), labelInput("selcolline"),value = "#555555", allowTransparent = T),
                                      ns = ns
                     ),
                     conditionalPanel("input.BoxNormal == 'tabNormalCalc'",
                                      sliderInput(ns("slide_inter"), labelInput("alfa"), min = 0, max = 0.2, step = 0.01, value = 0.05),
                                      ns = ns
                     ))
  
  normal.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                      conditionalPanel("input.BoxNormal == 'tabNormalPlot'",codigo.monokai(ns("fieldCodeNormal"), height = "10vh"),ns = ns),
                      conditionalPanel("input.BoxNormal == 'tabQPlot'",codigo.monokai(ns("fieldCodeQplot"), height = "10vh"),ns = ns),
                      conditionalPanel("input.BoxNormal == 'tabNormalCalc'",codigo.monokai(ns("fieldCalcNormal"), height = "10vh"),ns = ns))
  
  
  
  num.normal.plot.panel <- tabPanel(title = labelInput("plotnormal"), value = "tabNormalPlot", echarts4rOutput(ns('plot.normal'), height = "70vh"))
  
  qplot <- tabPanel(title = "Qplot + Qline", value = "tabQPlot",echarts4rOutput(ns('plot_qq'), height = "70vh"))
  
  resumen.test <- tabPanel(title = labelInput("normalidad"), value = "tabNormalCalc",
           withLoader(DT::DTOutput(ns('calc_normal')),type = "html", loader = "loader4"))
  

  
  tabs.normal <- tabsOptions(heights = c(40, 30), tabs.content = list(normal.opc, normal.code))
  
  normal.options <-  tags$div(class = "multiple-select-var", 
                              conditionalPanel("input.BoxNormal == 'tabNormalPlot' || input.BoxNormal == 'tabQPlot'",
                                               selectInput(inputId = ns("sel_normal"), label = NULL, choices =  ""),ns = ns))
  
  page.test.normality <- tabItem(tabName = "normalidad",
                                 tabBoxPrmdt(id = ns("BoxNormal"), opciones = tabs.normal,
                                        title = normal.options,
                                        num.normal.plot.panel,
                                        qplot,
                                        resumen.test))
  
  tagList(
    page.test.normality
  )
}
    
#' normal Server Function
#' @keywords internal
#' 
mod_normal_server <- function(input, output, session, updateData){
  ns <- session$ns
 
  #Update on load data
  observeEvent(updateData$datos, {
    numericos   <- var.numericas(updateData$datos)
    
    updateSelectInput(session, "sel_normal", choices = colnames(numericos))
  })
  
  # Grafico Test de normalidad
  output$plot.normal <- renderEcharts4r({
    tryCatch({
      input$run.normal
      var       <- input$sel_normal
      datos     <- updateData$datos[, var]
      colorBar  <- isolate(input$col_hist_bar)
      colorLine <- isolate(input$col_hist_line)
      nombres   <- c(tr("histograma", updateData$idioma), 
                     tr("curvanormal", updateData$idioma))
      
      cod <- paste0("e_histnormal(datos[['", var, "']], '", colorBar,
                    "', '", colorLine, "', c('", nombres[1], "', '", 
                    nombres[2], "'))")
      updateAceEditor(session, "fieldCodeNormal", value = cod)
      return(e_histnormal(datos, colorBar, colorLine, nombres))
    }, error = function(e){
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  
  # Grafico qqplot + qqline
  output$plot_qq <- renderEcharts4r({
    input$run.normal
    var        <- input$sel_normal
    datos      <- updateData$datos[, var]
    colorPoint <- isolate(input$col_qq_point)
    colorLine  <- isolate(input$col_qq_line)
    
    tryCatch({
      cod <- paste0("e_qq(datos[['", var, "']], '", colorPoint,
                    "', '", colorLine, "')")
      updateAceEditor(session, "fieldCodeQplot", value = cod)
      return(e_qq(datos, colorPoint, colorLine))
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  
  # Resumen Test de normalidad
  output$calc_normal <- DT::renderDT({
    input$run.normal
    datos <- updateData$datos
    alfa <- isolate(as.numeric(input$slide_inter))
    noms  <- c(tr('asimetria', isolate(updateData$idioma)),
               tr('normalidad', isolate(updateData$idioma)),
               tr('sigue', isolate(updateData$idioma)),
               tr('pvalue', isolate(updateData$idioma)),
               tr('tasim', isolate(updateData$idioma)))
    
    tryCatch({
      updateAceEditor(session, "fieldCalcNormal", value = "dfnormal(datos)")
      res <- dfnormal(datos)
      
      res <- res[, c(1, 5)]
      res <- round(res, 3)
      res$asimetria <- res$fisher > 0
      res$asimetria <- ifelse(res$asimetria, '<i class="fa fa-plus" style="color: green;"></i>', 
                              '<i class="fa fa-minus" style="color: red;"></i>')
      res$normal <- res$shapiro > alfa
      res$normal <- ifelse(res$normal, '<i class="fa fa-check" style="color: green;"></i>', 
                           '<i class="fa fa-times" style="color: red;"></i>')
      res$shapiro <- paste0(res$shapiro, " > ", alfa)
      res <- res[, c(1, 3, 2, 4)]
      
      sketch <- htmltools::withTags(table(
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Variables'), 
            tags$th(colspan = 2, style = "text-align: center;", 
                    labelInput('asimetria', noms[1])),
            tags$th(colspan = 2, style = "text-align: center;", 
                    labelInput('normalidad', noms[2]))
          ),
          tags$tr(
            tags$th(labelInput('tasim', noms[5])), tags$th(labelInput('asimetria', noms[1])),
            tags$th(labelInput('pvalue', noms[4])), tags$th(labelInput('sigue', noms[3]))
          )
        )
      ))
      DT::datatable(
        res, selection = 'none', container = sketch, escape = F,
        options = list(dom = 'frtip', scrollY = "50vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
}
    
## To be copied in the UI
# mod_normal_ui("normal_ui_1")
    
## To be copied in the server
# callModule(mod_normal_server, "normal_ui_1")
 
