#' neural_net UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_neural_net_ui <- function(id){
  
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
  
  
  tabs.options.generate <- tabsOptions(widths = c(100), heights = c(80),
                                       tabs.content = list(nn.options))
  
  
  tabs.options <- list(conditionalPanel("input.BoxNn == 'tabNnModelo'",tabs.options.generate,ns = ns))
  
  
  plot.nn <- tabPanel(title = labelInput("redPlot"), value = "tabNnPlot",
                      withLoader(plotOutput(ns('plot_nn') , height = "75vh"),type = "html", loader = "loader4"))
  
  generate.nn.panel <- tabPanel(title = labelInput("generatem"), value = "tabNnModelo",
                                withLoader(verbatimTextOutput(ns("txtnn")),type = "html", loader = "loader4"))
  
  prediction.nn.panel <- tabPanel(title = labelInput("predm"), value = "tabNnPred",
                                  withLoader(DT::dataTableOutput(ns("nnPrediTable")),type = "html", loader = "loader4"))
  
  disp.nn.panel <- tabPanel(title = labelInput("dispersion"), value = "tabNnDisp",
                            withLoader(echarts4rOutput(ns('plot_nn_disp'),height = "75vh"),type = "html", loader = "loader4"))
  
  general.index.nn.panel <- tabPanel(title = labelInput("indices"), value = "tabNnIndex",
                                     br(),
                                     div(withLoader(tableOutput(ns('indexdfnn')),type = "html", loader = "loader4")),
                                     br(),
                                     div(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     div(withLoader(tableOutput(ns('indexdfnn2')),type = "html", loader = "loader4")))
  
  page.nn  <- tabItem(tabName = "nn",
                      tabBoxPrmdt(id = ns("BoxNn"), opciones = tabs.options,
                             generate.nn.panel,
                             plot.nn,
                             prediction.nn.panel,
                             disp.nn.panel,
                             general.index.nn.panel))
  
  
  tagList(
    page.nn
  )
}

#' neural_net Server Function
#'
#' @noRd 
mod_neural_net_server <- function(input, output, session,updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  
  nombreModelo <- "modelo.nn"
  
  
  return.nn.default.values <- function(){
    updateSliderInput(session,  "cant.capas.nn", value = 2)
    updateNumericInput(session, "threshold.nn",  value = 0.1)
    updateNumericInput(session, "stepmax.nn",    value = 5000)
    updateLayers()
  }
  
  observeEvent(c(updateData$datos,updateData$variable.predecir), {
    modelos2$nn = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
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
      cant.capas        <- input$cant.capas.nn
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
  
  
  
  #Update model tab
  output$txtnn <- renderPrint({
    input$runNn
    tryCatch({
      codigo.nn()
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba      <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        threshold  <- input$threshold.nn
        stepmax    <- input$stepmax.nn
        cant.capas <- input$cant.capas.nn
        
        hidden    <- c(isolate(input$nn.cap.1),isolate(input$nn.cap.2),
                       isolate(input$nn.cap.3),isolate(input$nn.cap.4),
                       isolate(input$nn.cap.5),isolate(input$nn.cap.6),
                       isolate(input$nn.cap.7),isolate(input$nn.cap.8),
                       isolate(input$nn.cap.9),isolate(input$nn.cap.10))
        
      })
      
      threshold <- ifelse(threshold == 0, 0.01, threshold)
      stepmax   <- ifelse(stepmax < 100, 100, stepmax)
      hidden    <- hidden[1:cant.capas]
      #Model generate
      
      form <- formula(paste0(variable.predecir,"~."))
      modelo <- train.neuralnet(form, data = datos.aprendizaje, hidden = hidden, 
                                   linear.output = TRUE, threshold = threshold, stepmax = stepmax)
      
      #Prediccion
      prediccion.nn <- predict(modelo, datos.prueba)$prediction

      #Indices
      indices.nn    <- general_indices(datos.prueba[,variable.predecir], prediccion.nn)

      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$nn[[nombreModelo]] <- list(modelo     = modelo, 
                                                 prediccion = prediccion.nn, 
                                                 indices    = indices.nn))

      #Cambiamos la forma en que va aparecer el call
      modelo$call$formula   <- form
      modelo$call$hidden    <- hidden
      modelo$call$threshold <- threshold
      modelo$call$stepmax   <- stepmax
      
      print(modelo)
      
    }, error = function(e){
      isolate(modelos$nn[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (NN-01) : ",e), duration = 10, type = "error")
      NULL
      
    },warning = function(w){
      isolate(modelos$nn[[nombreModelo]] <- NULL)
      showNotification(paste0(tr("nnWar")," (NN-01) : ",w), duration = 10, type = "warning")
    })
  })
  
  
  #Update neural network plot tab
  output$plot_nn <- renderPlot({
    tryCatch({
      if(!is.null(modelos$nn[[nombreModelo]])){
        isolate({
          modelo     <- modelos$nn[[nombreModelo]]$modelo
          cant.capas <- input$cant.capas.nn
          hidden     <- c(input$nn.cap.1,input$nn.cap.2,input$nn.cap.3,input$nn.cap.4,
                          input$nn.cap.5,input$nn.cap.6,input$nn.cap.7,input$nn.cap.8,
                          input$nn.cap.9,input$nn.cap.10)
          hidden <- hidden[1:cant.capas]
        })
        
        #Cambia el codigo del grafico del árbol
        codigo <- paste0("nn_plot(", nombreModelo, ")")
        cod    <- paste0("### redPlot\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        if(cant.capas * sum(hidden) <= 1000 & ncol(modelo$covariate) <= 25){
          nn_plot(modelo)
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
          real.val     <- datos.prueba[updateData$variable.predecir]
        })
        tb_predic(real.val, prediccion.nn, updateData$decimals, codedioma$idioma)
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
          datos.prueba      <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
        })
        
        idioma <- codedioma$idioma
        
        codigo <- disp_models("modelo.nn", tr("nn", idioma), variable.predecir)
        cod    <- paste0("### docdisp\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
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
        idioma     <- codedioma$idioma
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
      if(!is.null(modelos$nn[[nombreModelo]])){
        idioma   <- codedioma$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(summary_indices(updateData$datos.prueba[,updateData$variable.predecir]),
                              decimals, 
                              idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (NN-06) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
  # Execute model, prediction and indices
  codigo.nn <- function() {
    
    tryCatch({
      
      isolate({
        variable.predecir <- updateData$variable.predecir
        threshold  <- input$threshold.nn
        stepmax    <- input$stepmax.nn
        cant.capas <- input$cant.capas.nn
        hidden <- c(input$nn.cap.1,input$nn.cap.2,input$nn.cap.3,input$nn.cap.4,
                    input$nn.cap.5,input$nn.cap.6,input$nn.cap.7,input$nn.cap.8,
                    input$nn.cap.9,input$nn.cap.10)
      })
      
      threshold <- ifelse(threshold == 0, 0.01, threshold)
      stepmax <- ifelse(stepmax < 100, 100, stepmax)

      hidden <- hidden[1:cant.capas]
      #Model generate
      codigo <- codeNn(variable.predecir, hidden, threshold, stepmax)
      cod    <- paste0("### NN\n", codigo)
      
      #Prediccion
      codigo <- codigo.prediccion("nn")
      cod    <- paste0(cod, codigo)
      #Indices
      codigo <- codigo.IG(model.name = "nn", variable.pr = variable.predecir)
      cod    <- paste0(cod, codigo)
      
      isolate(codedioma$code <- append(codedioma$code, cod))
      
    })
  }
  
}

## To be copied in the UI
# mod_neural_net_ui("neural_net_ui_1")

## To be copied in the server
# callModule(mod_neural_net_server, "neural_net_ui_1")

