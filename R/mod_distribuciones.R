#' distribuciones UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_distribuciones_ui <- function(id){
  ns <- NS(id)
  
  titulo_dist <- tags$div(class = "multiple-select-var", 
                          conditionalPanel(condition = "input.tabDyA == 'numericas'",
                                           selectInput(inputId = ns("sel_dya_num"),label = NULL, choices = ""),ns = ns),
                          conditionalPanel(condition = "input.tabDyA == 'categoricas'",
                                           selectInput(inputId = ns("sel_dya_cat"), label = NULL, choices = ""),ns = ns))
  
  distr.numericas.opc <- list(options.run(ns("run_dist")), tags$hr(style = "margin-top: 0px;"),
                             colourInput(ns("col_dist_bar"),labelInput("selcolbar"),
                                                       value = "steelblue",allowTransparent = T),
                             colourInput(ns("col_dist_point"), labelInput("selcolline"),
                                                       value = "red",allowTransparent = T))
  
  distr.numericas.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                               codigo.monokai(ns("fieldCodeNum"), height = "10vh"))
  
  distr.numericas.atipicos <- list(h3(labelInput("atipicos")), hr(style = "margin-top: 0px;"),
                                  DT::dataTableOutput(ns("mostrar.atipicos")))
  
  
  distr.categoricas.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                              codigo.monokai(ns("fieldCodeCat"), height = "20vh"))
  

  tabs_dist_numericas <- tabsOptions(buttons = list(icon("gear"), icon("info"), icon("code")),
                           widths = c(50, 100, 100), heights = c(50, 50, 35),
                           tabs.content = list(distr.numericas.opc,distr.numericas.atipicos,distr.numericas.code))
  
  tabs_dist_categoricas <- tabsOptions(buttons = list(icon("code")),widths = c(100), heights = c(35),
                                       tabs.content = list(distr.categoricas.code))
  
  distribuciones.numericas <- tabPanel(title = labelInput("numericas"), value = "numericas",
                                       echarts4rOutput(ns('plot_num'), height = "75vh"))
  
  distribuciones.categoricas <- tabPanel(title = labelInput("categoricas"), value = "categoricas",
                                         echarts4rOutput(ns('plot_cat'), height = "75vh"))
  
  
  page.distributions <- tabItem(tabName = "distribucion",
                                tabBox(id = ns("tabDyA"), width = NULL,
                                       title =  titulo_dist,
                                       distribuciones.numericas,
                                       distribuciones.categoricas,
                                       conditionalPanel("input.tabDyA == 'numericas'",tabs_dist_numericas,ns = ns),
                                       conditionalPanel("input.tabDyA == 'categoricas'",tabs_dist_categoricas,ns = ns)))
  
  tagList(
    page.distributions
  )
}
    

#' distribuciones Server Function
#' @keywords internal
#' 
mod_distribuciones_server <- function(input, output, session, updateData){
  ns <- session$ns
  
  # Update on load data
  observeEvent(updateData$datos, {
    datos       <- updateData$datos
    numericos   <- var.numericas(datos)
    categoricos <- var.categoricas(datos)
    
    updateSelectInput(session, "sel_dya_num", choices = colnames(numericos))
    updateSelectInput(session, "sel_dya_cat", choices = colnames(categoricos))
  })
  
  # Gráfico de Distribuciones (Númericas)
  output$plot_num <- renderEcharts4r({
    input$run_dist
    datos      <- updateData$datos
    var        <- input$sel_dya_num
    colorBar   <- isolate(input$col_dist_bar)
    colorPoint <- isolate(input$col_dist_point)
    titulos <- c(
      tr("minimo", updateData$idioma),
      tr("q1", updateData$idioma),
      tr("mediana", updateData$idioma),
      tr("q3", updateData$idioma),
      tr("maximo", updateData$idioma)
    )
    
    tryCatch({
      cod <- paste0("e_histboxplot(datos[['", var, "']], '", var, "', '", 
                    colorBar, "', '", colorPoint, "', c('", 
                    paste(titulos, collapse = "', '"), "'))\n")
      updateAceEditor(session, "fieldCodeNum", value = cod)
      e_histboxplot(datos[[var]], var, colorBar, colorPoint, titulos)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  output$mostrar.atipicos <- DT::renderDataTable({
    datos <- updateData$datos
    var   <- input$sel_dya_num
    atipicos <- boxplot.stats(datos[, var])
    datos <- datos[datos[, var] %in% atipicos$out, var, drop = F]
    datos <- datos[order(datos[, var]), , drop = F]
    DT::datatable(datos, options = list(
      dom = 't', scrollX = TRUE, scrollY = "28vh", pageLength = nrow(datos))) |>
      formatStyle(1, color = "white", backgroundColor = "#CBB051", target = "row")
  })
  
  # Gráfico de Distribuciones (Categóricas)
  output$plot_cat <- renderEcharts4r({
    var  <- input$sel_dya_cat
    validate(need(var != "", tr("errorcat", isolate(updateData$idioma))))
    
    tryCatch({
      datos.plot <- updateData$datos[, var]
      
      cod <- code.dist.cat(var)
      updateAceEditor(session, "fieldCodeCat", value = cod)
      
      datos.plot <- data.frame (
        label = levels(datos.plot),
        value = summary(datos.plot, maxsum = length(levels(datos.plot)))
      )
      
      datos.plot |> e_charts_("label") |> e_bar_("value", name = var) |>
        e_tooltip() |> e_datazoom(show = F) |> e_show_loading()
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
 
}
    
## To be copied in the UI
# mod_distribuciones_ui("distribuciones_ui_1")
    
## To be copied in the server
# callModule(mod_distribuciones_server, "distribuciones_ui_1")
 
