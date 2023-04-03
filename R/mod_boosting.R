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
  
  tabs.options <- 
    list(
      conditionalPanel("input.BoxB == 'tabBModelo'",
                        tabsOptions(widths  = c(100), heights = c(80),
                                    tabs.content = list(list(options.run(ns("runBoosting")), tags$hr(style = "margin-top: 0px;"),
                                                             div(col_5(numericInput(ns("iter.boosting"), labelInput("numTree"), 20, width = "100%",min = 1)),
                                                                 col_5(numericInput(ns("shrinkage.boosting"), labelInput("shrinkage"), 0.1, width = "100%",min = 0.01, step = 0.01))),
                                                             div(col_5(selectInput(inputId = ns("tipo.boosting"), label = labelInput("selectAlg"),selected = "gaussian",
                                                                                    choices =  c("gaussian", "laplace", "tdist"))))))),ns = ns))
  
  tagList(
    tabBoxPrmdt(id = ns("BoxB"), opciones = tabs.options,
                tabPanel(title = labelInput("generatem"), value = "tabBModelo",
                         withLoader(verbatimTextOutput(ns("txtBoosting")),
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("varImp"), value = "tabBImp",
                         withLoader(echarts4rOutput(ns('plot_boosting_import'),height = "75vh"),
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("predm"), value = "tabBPred",
                         withLoader(DT::dataTableOutput(ns("boostingPrediTable")),
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("evolerror"), value = "tabBRMSE",
                         withLoader(echarts4rOutput(ns('plot_b_rmse'),height = "75vh"),
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("dispersion"), value = "tabBDisp",
                         withLoader(echarts4rOutput(ns('plot_boosting_disp'),height = "75vh"),
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("indices"),value = "tabBIndex",
                         br(),
                         div(withLoader(tableOutput(ns('indexdfb')),
                                        type = "html", loader = "loader4")),
                         br(),
                         div(col_12(align="center", tags$h3(labelInput("resumenVarPre")))),
                         br(),
                         div(withLoader(tableOutput(ns('indexdfb2')),type = "html", loader = "loader4"))))
  )
}

#' boosting Server Function
#'
#' @noRd 
mod_boosting_server <- function(input, output, session,updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  
  nombreBase <- "modelo.boost."
  nombreModelo <- "modelo.boost."
  
  return.boosting.default.values <- function(){
    updateSelectInput(session,  inputId = "tipo.boosting", selected = "gaussian")
    updateNumericInput(session, inputId = "iter.boosting", value = 20)
    updateNumericInput(session, inputId = "shrinkage.boosting", value = 0.1)
    
    nombreModelo <- "modelo.boost."
  }
  
  observeEvent(c(updateData$datos), {
    modelos2$boosting = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
  observeEvent(updateData$datos.aprendizaje,{
    return.boosting.default.values()
  })
  
  
  #Update model tab
  output$txtBoosting <- renderPrint({
    input$runBoosting
    tryCatch({
      codigo.boosting()
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba      <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        n.trees      <- input$iter.boosting
        distribution <- input$tipo.boosting
        shrinkage    <- input$shrinkage.boosting
      })
      
      if(!is.null(calibrate_boosting(datos.aprendizaje))){
        nombreModelo <<- paste0(nombreBase, distribution)
        n.trees   <- ifelse(!is.numeric(n.trees), 50, n.trees)
        shrinkage <- ifelse(!is.numeric(shrinkage), 0.1, shrinkage)
        
        #Model generate
        modelo.boost <- boosting_model(datos.aprendizaje,variable.predecir, n.trees, distribution, shrinkage)

        #Prediccion
        prediccion.boost <- predict(modelo.boost,datos.prueba, n.trees = n.trees)$prediction

        #Indices
        indices.boost <- general_indices(datos.prueba[,variable.predecir], prediccion.boost)

        
        #isolamos para que no entre en un ciclo en el primer renderPrint
        isolate({
          modelos$boost[[nombreModelo]] <- list(modelo = modelo.boost, prediccion = prediccion.boost, indices = indices.boost,
                                                id = distribution)
          modelos2$boosting$n <- modelos2$boosting$n + 1
          modelos2$boosting$mcs[modelos2$boosting$n] <- list(indices.boost)
          if(modelos2$boosting$n > 9)
            modelos2$boosting$n <- 0
          
        })
        
        if(!is.null(modelos$boost[[nombreModelo]])){
          modelo.boost <- modelos$boost[[nombreModelo]]$modelo
          print(modelo.boost)
        }
        else{NULL}
        
      }
      else{
        isolate(modelos$boost[[nombreModelo]] <- NULL)
        showNotification(tr("ErrorBsize"), duration = 10, type = "error")
      }
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
        cod    <- paste0("### varImp\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        idioma  <- codedioma$idioma
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
          real.val     <- datos.prueba[updateData$variable.predecir]
        })
        tb_predic(real.val, prediccion.boost, updateData$decimals, codedioma$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (Boost-03) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  
  # Update rmse tab
  output$plot_b_rmse <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$boost[[nombreModelo]])){
        df_plot       <- b_ntree_values(modelos$boost[[nombreModelo]]$modelo)
        plot_RMSEK(datos = df_plot ,titles = get_title("RF", codedioma$idioma))
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (B-04) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  # Update Dispersion Tab
  output$plot_boosting_disp <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$boost[[nombreModelo]])){
        prediccion.boost <- modelos$boost[[nombreModelo]]$prediccion
        isolate({
          datos.prueba      <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
          distribution      <- input$tipo.boosting
        })
        
        idioma <- codedioma$idioma
        
        codigo <- disp_models(nombreModelo, paste0(tr("boost", idioma),"-",distribution), variable.predecir)
        cod    <- paste0("### docdisp\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
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
        idioma        <- codedioma$idioma
        indices.boost <- modelos$boost[[nombreModelo]]$indices
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
      if(!is.null(modelos$boost[[nombreModelo]])){
        idioma   <- codedioma$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(summary_indices(updateData$datos.prueba[,updateData$variable.predecir]),
                              decimals, 
                              idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (Boost-06) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%',align = 'c')
  
  
  
  # Execute model, prediction and indices
  codigo.boosting <- function() {
    tryCatch({
      isolate({
        variable.predecir <- updateData$variable.predecir
        n.trees      <- input$iter.boosting
        distribution <- input$tipo.boosting
        shrinkage    <- input$shrinkage.boosting
      })
      
        n.trees   <- ifelse(!is.numeric(n.trees), 50, n.trees)
        shrinkage <- ifelse(!is.numeric(shrinkage), 0.1, shrinkage)
        
        #Model generate
        codigo <- codeBoost(variable.predecir, n.trees, distribution, shrinkage)
        cod    <- paste0("### BOOST\n", codigo)
        
        #Prediccion
        codigo <- codigo.prediccion("boosting")
        cod    <- paste0(cod, codigo)
        #Indices
        codigo <- codigo.IG(model.name = "boosting", variable.pr = variable.predecir)
        cod    <- paste0(cod, codigo)
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
      
      
    }, error = function(e){
      showNotification(paste0("Error (Boost-00) : ",e), duration = 10, type = "error")
    })
  }
  
}

## To be copied in the UI
# mod_boosting_ui("boosting_ui_1")

## To be copied in the server
# callModule(mod_boosting_server, "boosting_ui_1")

