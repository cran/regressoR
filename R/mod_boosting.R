#' boosting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_boosting_ui <- function(id){
  
  ns <- NS(id)
  
  b.options <- list(options.run(ns("runBoosting")), tags$hr(style = "margin-top: 0px;"),
                    fluidRow(column(numericInput(ns("iter.boosting"), labelInput("numTree"), 20, width = "100%",min = 1), width = 5),
                             column(numericInput(ns("shrinkage.boosting"), labelInput("shrinkage"), 0.1, width = "100%",min = 0.01, step = 0.01), width=5)),
                    fluidRow(column(selectInput(inputId = ns("tipo.boosting"), label = labelInput("selectAlg"),selected = "gaussian",
                                                choices =  c("gaussian", "laplace", "tdist")), width = 5)))
  
  b.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                        codigo.monokai(ns("fieldCodeBoosting"), height = "7vh"))
  
  
  b.code  <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                  conditionalPanel("input.BoxB == 'tabBImp'",
                                   codigo.monokai(ns("fieldCodeBoostingPlotImport"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxB == 'tabBPred'",
                                   codigo.monokai(ns("fieldCodeBoostingPred"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxB == 'tabBDisp'",
                                   codigo.monokai(ns("fieldCodeBoostingDisp"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxB == 'tabBIndex'",
                                   codigo.monokai(ns("fieldCodeBoostingIG"), height = "7vh"),ns = ns))
  
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("gear"), icon("code")), widths = c(50,100), heights = c(80,70),
                                       tabs.content = list(b.options,b.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(70),
                                         tabs.content = list(b.code))
  
  generate.b.panel <- tabPanel(title = labelInput("generatem"), value = "tabBModelo",
                               withLoader(verbatimTextOutput(ns("txtBoosting")),type = "html", loader = "loader4"))
  
  plot.boosting.import <- tabPanel(title = labelInput("varImp"), value = "tabBImp",
                                   echarts4rOutput(ns('plot_boosting_import'), height = "75vh"))
  
  prediction.b.panel <- tabPanel(title = labelInput("predm"), value = "tabBPred",
                                 withLoader(DT::dataTableOutput(ns("boostingPrediTable")),type = "html", loader = "loader4"))
  
  disp.boosting.panel <- tabPanel(title = labelInput("dispersion"), value = "tabBDisp",
                                  echarts4rOutput(ns('plot_boosting_disp'), height = "75vh"))
  
  general.index.b.panel <- tabPanel(title = labelInput("indices"),value = "tabBIndex",
                                    br(),
                                    fluidRow(withLoader(tableOutput(ns('indexdfb')),type = "html", loader = "loader4")),
                                    br(),
                                    fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                    br(),
                                    fluidRow(withLoader(tableOutput(ns('indexdfb2')),type = "html", loader = "loader4")))
  
  pagina.boosting <- tabItem(tabName = "boosting",
                             tabBox(id = ns("BoxB"), width = NULL, height ="80%",
                                    generate.b.panel,
                                    plot.boosting.import,
                                    prediction.b.panel,
                                    disp.boosting.panel,
                                    general.index.b.panel,
                                    conditionalPanel("input.BoxB == 'tabBModelo'",tabs.options.generate,ns = ns),
                                    conditionalPanel("input.BoxB != 'tabBModelo'",tabs.options.Nogenerate,ns = ns)))
  
  tagList(
    pagina.boosting
  )
}

#' boosting Server Function
#'
#' @noRd 
mod_boosting_server <- function(input, output, session,updateData, modelos){
  ns <- session$ns
  
  nombreBase <- "modelo.boost."
  nombreModelo <- "modelo.boost."
  
  return.boosting.default.values <- function(){
    updateSelectInput(session,inputId = "tipo.boosting", selected = "gaussian")
    updateNumericInput(session, inputId = "iter.boosting", value = 20)
    updateNumericInput(session, inputId = "shrinkage.boosting", value = 0.1)
    
    nombreModelo <- "modelo.boost."
  }
  
  
  observeEvent(updateData$datos.aprendizaje,{
    return.boosting.default.values()
  })
  
  # When the boosting model is generated
  observeEvent(input$runBoosting, {
    if (validate_data(updateData, idioma = updateData$idioma)){ # Si se tiene los datos entonces :
      boosting_full()
    }
  })
  

  # Execute model, prediction and indices
  boosting_full <- function() {
    tryCatch({
      shinyjs::runjs(code = "generating_model = true")
      
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        n.trees <- input$iter.boosting
        distribution <- input$tipo.boosting
        shrinkage <- input$shrinkage.boosting
      })
      
      if(!is.null(calibrate_boosting(datos.aprendizaje))){
        nombreModelo <<- paste0(nombreBase, distribution)
        
        n.trees <- ifelse(!is.numeric(n.trees), 50, n.trees)
        shrinkage <- ifelse(!is.numeric(shrinkage), 0.1, shrinkage)
        
        #Model generate
        modelo.boost <- boosting_model(datos.aprendizaje,variable.predecir, n.trees, distribution, shrinkage)
        updateAceEditor(session, "fieldCodeBoosting", value = codeBoost(variable.predecir, n.trees, distribution, shrinkage))
        
        #Prediccion
        prediccion.boost <- boosting_prediction(modelo.boost, datos.prueba, n.trees)
        updateAceEditor(session, "fieldCodeBoostingPred", value = codeBoostPred(nombreModelo, n.trees))
        
        #Indices
        indices.boost <- general_indices(datos.prueba[,variable.predecir], prediccion.boost)
        updateAceEditor(session, "fieldCodeBoostingIG", value = codeBoostIG(variable.predecir))
        
        #isolamos para que no entre en un ciclo en el primer renderPrint
        isolate(modelos$boost[[nombreModelo]] <- list(modelo = modelo.boost, prediccion = prediccion.boost, indices = indices.boost,
                                                      id = distribution))
      }
      else{
        isolate(modelos$boost[[nombreModelo]] <- NULL)
        showNotification(tr("ErrorBsize"), duration = 10, type = "error")
      }
      
    }, error = function(e){
      isolate(modelos$boost[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (Boost-00) : ",e), duration = 10, type = "error")
    },
    finally = {shinyjs::runjs(code = "generating_model = false")})
  }
  
  
  #Update model tab
  output$txtBoosting <- renderPrint({
    tryCatch({
      if(!is.null(modelos$boost[[nombreModelo]])){
        modelo.boost <- modelos$boost[[nombreModelo]]$modelo
        print(summary(modelo.boost, plotit = FALSE))
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (Boost-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  # Update importance plot
  output$plot_boosting_import <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$boost[[nombreModelo]])){
        
        modelo.boost <- modelos$boost[[nombreModelo]]$modelo
        
        # Cambia el codigo del grafico de importancia
        codigo <- paste0("boosting_importance_plot(", nombreModelo, ")")
        updateAceEditor(session, "fieldCodeBoostingPlotImport", value = codigo)
        
        idioma <- updateData$idioma
        titulos <- c(
          tr("impVarRI", idioma),
          tr("RI", idioma),
          tr("variable", idioma)
        )
        
        boosting_importance_plot(modelo.boost,titulos)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (Boost-02) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  # Update prediction tab
  output$boostingPrediTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$boost[[nombreModelo]])){
        prediccion.boost <- modelos$boost[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          real.val <- datos.prueba[updateData$variable.predecir]
        })
        tb_predic(real.val, prediccion.boost, updateData$decimals, updateData$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (Boost-03) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  
  
  # Update Dispersion Tab
  output$plot_boosting_disp <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$boost[[nombreModelo]])){
        prediccion.boost <- modelos$boost[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
          distribution <- input$tipo.boosting
        })
        
        idioma <- updateData$idioma
        
        codigo <- disp_models(nombreModelo, paste0(tr("boost", idioma),"-",distribution), variable.predecir)
        updateAceEditor(session, "fieldCodeBoostingDisp", value = codigo)
        
        titulos <- c(
          tr("predvsreal", idioma),
          tr("realValue", idioma),
          tr("pred", idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion.boost,
                             paste0(tr("boost", idioma),"-",distribution),titulos)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (Boost-03) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update Indices tab
  output$indexdfb <- renderTable({
    tryCatch({
      if(!is.null(modelos$boost[[nombreModelo]])){
        idioma <- updateData$idioma
        indices.boost<- modelos$boost[[nombreModelo]]$indices
        tabla.indicesPrecision(indices.boost, updateData$decimals, idioma)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (Boost-05) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
  
  
  output$indexdfb2 <- renderTable({
    tryCatch({
      if(!is.null(modelos$boost[[nombreModelo]])& !is.null(updateData$summary.var.pred)){
        idioma <- updateData$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(updateData$summary.var.pred, decimals, idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (Boost-06) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%',align = 'c')
}

## To be copied in the UI
# mod_boosting_ui("boosting_ui_1")

## To be copied in the server
# callModule(mod_boosting_server, "boosting_ui_1")

