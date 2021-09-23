#' neural_networks UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_neural_networks_ui <- function(id){
  
  ns <- NS(id)
  
  nn.options <- list(options.run(ns("runNn")), tags$hr(style = "margin-top: 0px;"),
                     fluidRow(column(numericInput(ns("threshold.nn"),labelInput("threshold"),
                                                  min = 0, step = 0.1, value = 0.1), width = 5),
                              column(numericInput(ns("stepmax.nn"),labelInput("stepmax"),
                                                  min = 100, step = 100, value = 5000), width = 5)),
                     fluidRow(column(sliderInput(inputId = ns("cant.capas.nn"), min = 1, max = 10,
                                                 label = labelInput("selectCapas"), value = 2), width = 12)),
                     fluidRow(lapply(1:10, function(i) tags$span(numericInput(paste0(ns("nn.cap."),i), NULL,
                                                                              min = 1, step = 1, value = 2),
                                                                 class = "mini-numeric-select"))))
  
  nn.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                         codigo.monokai(ns("fieldCodeNn"), height = "7vh"))
  
  
  nn.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                  conditionalPanel("input.BoxNn == 'tabNnPlot'",
                                   codigo.monokai(ns("fieldCodeNnPlot"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxNn == 'tabNnPred'",
                                   codigo.monokai(ns("fieldCodeNnPred"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxNn == 'tabNnDisp'",
                                   codigo.monokai(ns("fieldCodeNnDisp"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxNn == 'tabNnIndex'",
                                   codigo.monokai(ns("fieldCodeNnIG"), height = "7vh"),ns = ns))
  
  tabs.nn <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(75,100), heights = c(95, 95),
                         tabs.content = list(nn.options, nn.code))
  
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("gear"), icon("code")), widths = c(50,100), heights = c(80,70),
                                       tabs.content = list(nn.options,nn.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(70),
                                         tabs.content = list(nn.code))
  
  
  plot.nn <- tabPanel(title = labelInput("redPlot"), value = "tabNnPlot",
                      withLoader(plotOutput(ns('plot_nn') , height = "75vh"),type = "html", loader = "loader4"))
  
  generate.nn.panel <- tabPanel(title = labelInput("generatem"), value = "tabNnModelo",
                                withLoader(verbatimTextOutput(ns("txtnn")),type = "html", loader = "loader4"))
  
  prediction.nn.panel <- tabPanel(title = labelInput("predm"), value = "tabNnPred",
                                  withLoader(DT::dataTableOutput(ns("nnPrediTable")),type = "html", loader = "loader4"))
  
  disp.nn.panel <- tabPanel(title = labelInput("dispersion"), value = "tabNnDisp",
                            echarts4rOutput(ns('plot_nn_disp'), height = "75vh"))
  
  general.index.nn.panel <- tabPanel(title = labelInput("indices"), value = "tabNnIndex",
                                     br(),
                                     fluidRow(withLoader(tableOutput(ns('indexdfnn')),type = "html", loader = "loader4")),
                                     br(),
                                     fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     fluidRow(withLoader(tableOutput(ns('indexdfnn2')),type = "html", loader = "loader4")))
  
  page.nn  <- tabItem(tabName = "nn",
                      tabBox(id = ns("BoxNn"), width = NULL, height ="80%",
                             generate.nn.panel,
                             plot.nn,
                             prediction.nn.panel,
                             disp.nn.panel,
                             general.index.nn.panel,
                             conditionalPanel("input.BoxNn == 'tabNnModelo'",tabs.options.generate,ns = ns),
                             conditionalPanel("input.BoxNn != 'tabNnModelo'",tabs.options.Nogenerate,ns = ns)))
  
  
  tagList(
    page.nn
  )
}

#' neural_networks Server Function
#'
#' @noRd 
mod_neural_networks_server <- function(input, output, session,updateData, modelos){
  ns <- session$ns
  
  nombreModelo <- "modelo.nn"
  
  
  return.nn.default.values <- function(){
    updateSliderInput(session, "cant.capas.nn", value = 2)
    updateNumericInput(session, "threshold.nn", value = 0.1)
    updateNumericInput(session, "stepmax.nn", value = 5000)
    updateLayers()
  }
  
  observeEvent(updateData$datos.aprendizaje, {
    #Change to default values
    return.nn.default.values()
  })
  
  # When user change the layer selector
  observeEvent(input$cant.capas.nn, {
    updateLayers()
  })
  
  
  updateLayers <- function(){
    isolate({
      datos.aprendizaje <- updateData$datos.aprendizaje
      cant.capas <- input$cant.capas.nn
    })
    if(!is.null(datos.aprendizaje) && !is.null(input$cant.capas.nn)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn) {
          #No se usa ns() x el parámetro asis de show y hide.
          shinyjs::show(paste0("nn.cap.", i))
          updateNumericInput(session, paste0("nn.cap.", i), value = 2)
        } else {
          shinyjs::hide(paste0("nn.cap.", i))
        }
      }
    }
  }
  
  # When the nn model is generated
  observeEvent(input$runNn, {
    if (validate_data(updateData, idioma = updateData$idioma)) { # Si se tiene los datos entonces :
      nn_full()
    }
  })
  
  # Execute model, prediction and indices
  nn_full <- function() {
    
    tryCatch({
      shinyjs::runjs(code = "generating_model = true")
      
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        threshold <- input$threshold.nn
        stepmax <- input$stepmax.nn
        cant.capas <- input$cant.capas.nn
      })
      
      threshold <- ifelse(threshold == 0, 0.01, threshold)
      stepmax <- ifelse(stepmax < 100, 100, stepmax)
      hidden <- c(input$nn.cap.1,input$nn.cap.2,input$nn.cap.3,input$nn.cap.4,
                  input$nn.cap.5,input$nn.cap.6,input$nn.cap.7,input$nn.cap.8,
                  input$nn.cap.9,input$nn.cap.10)
      hidden <- hidden[1:cant.capas]
      
      #Model generate
      modelo.nn <- nn_model(datos.aprendizaje,variable.predecir, hidden, threshold, stepmax)
      updateAceEditor(session, "fieldCodeNn", value = codeNn(variable.predecir, hidden, threshold, stepmax))
      
      #Prediccion
      prediccion.nn <- nn_prediction(modelo.nn, datos.prueba)
      updateAceEditor(session, "fieldCodeNnPred", value = codeNnPred(nombreModelo))
      
      #Indices
      indices.nn <- general_indices(datos.prueba[,variable.predecir], prediccion.nn)
      updateAceEditor(session, "fieldCodeNnIG", value = codeNnIG(variable.predecir))
      
      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$nn[[nombreModelo]] <- list(modelo = modelo.nn, prediccion = prediccion.nn, indices = indices.nn))
      
    }, error = function(e){
      isolate(modelos$nn[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (NN-00) : ",e), duration = 10, type = "error")
      
    },warning = function(w){
      isolate(modelos$nn[[nombreModelo]] <- NULL)
      showNotification(paste0(tr("nnWar")," (NN-00) : ",w), duration = 10, type = "warning")
    },
    finally = {
      shinyjs::runjs(code = "generating_model = false")
    })
  }
  
  
  #Update model tab
  output$txtnn <- renderPrint({
    tryCatch({
      if(!is.null(modelos$nn[[nombreModelo]])){
        modelo.nn <- modelos$nn[[nombreModelo]]$modelo
        print(modelo.nn)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (NN-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update neural network plot tab
  output$plot_nn <- renderPlot({
    tryCatch({
      if(!is.null(modelos$nn[[nombreModelo]])){
        isolate({
          modelo.nn <- modelos$nn[[nombreModelo]]$modelo
          cant.capas <- input$cant.capas.nn
          hidden <- c(input$nn.cap.1,input$nn.cap.2,input$nn.cap.3,input$nn.cap.4,
                      input$nn.cap.5,input$nn.cap.6,input$nn.cap.7,input$nn.cap.8,
                      input$nn.cap.9,input$nn.cap.10)
          hidden <- hidden[1:cant.capas]
        })
        
        #Cambia el codigo del grafico del árbol
        updateAceEditor(session, "fieldCodeNnPlot", value = paste0("nn_plot(", nombreModelo, ")"))
        
        if(cant.capas * sum(hidden) <= 1000 & ncol(modelo.nn$covariate) <= 25){
          nn_plot(modelo.nn)
        }else{
          showNotification(tr("bigPlot"), duration = 10, type = "message")
          NULL
        }
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (NN-02) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  
  # Update prediction tab
  output$nnPrediTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$nn[[nombreModelo]])){
        prediccion.nn <- modelos$nn[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          real.val <- datos.prueba[updateData$variable.predecir]
        })
        tb_predic(real.val, prediccion.nn, updateData$decimals, updateData$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (NN-03) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  
  # Update Dispersion Tab
  output$plot_nn_disp <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$nn[[nombreModelo]])){
        prediccion.nn <- modelos$nn[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
        })
        
        idioma <- updateData$idioma
        
        codigo <- disp_models(nombreModelo, tr("nn", idioma), variable.predecir)
        updateAceEditor(session, "fieldCodeNnDisp", value = codigo)
        
        titulos <- c(
          tr("predvsreal", idioma),
          tr("realValue", idioma),
          tr("pred", idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion.nn,
                             tr("nn", idioma),titulos)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (NN-04) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update Indices tab
  output$indexdfnn <- renderTable({
    tryCatch({
      if(!is.null(modelos$nn[[nombreModelo]])){
        idioma <- updateData$idioma
        indices.nn <- modelos$nn[[nombreModelo]]$indices
        tabla.indicesPrecision(indices.nn, updateData$decimals, idioma)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (NN-05) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
  
  
  output$indexdfnn2 <- renderTable({
    tryCatch({
      if(!is.null(modelos$nn[[nombreModelo]])& !is.null(updateData$summary.var.pred)){
        idioma <- updateData$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(updateData$summary.var.pred, decimals, idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (NN-06) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
}

## To be copied in the UI
# mod_neural_networks_ui("neural_networks_ui_1")

## To be copied in the server
# callModule(mod_neural_networks_server, "neural_networks_ui_1")

