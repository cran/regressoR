#' r_forest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_r_forest_ui <- function(id){
  
  ns <- NS(id)
  
  rf.options <- list(conditionalPanel("input.BoxRf != 'tabRfRules'",
                                      options.run(ns("runRf")), tags$hr(style = "margin-top: 0px;"),
                                      fluidRow(column(numericInput(ns("ntree.rf"), labelInput("numTree"), 20, width = "100%", min = 0), width = 5),
                                               column(numericInput(ns("mtry.rf"), labelInput("numVars"),1, width = "100%", min = 1), width = 5)), ns = ns),
                     conditionalPanel("input.BoxRf == 'tabRfRules'",
                                      numericInput(ns("rules.rf.n"),labelInput("ruleNumTree"),1, width = "25%", min = 1), ns = ns))
  
  
  tabs.options.generate <- tabsOptions( widths = c(100), heights = c(80),
                                       tabs.content = list(rf.options))
  
  
  tabs.options <- list(conditionalPanel("input.BoxRf == 'tabRfModelo' || input.BoxRf == 'tabRfRules'", tabs.options.generate, ns = ns))
  
  generate.rf.panel <- tabPanel(title = labelInput("generatem"), 
                                value = "tabRfModelo",
                                withLoader(verbatimTextOutput(ns("txtRf")), type = "html", loader = "loader4"))
  
  plot.rf <- tabPanel(title = labelInput("varImp"), 
                      value = "tabRfImp",
                      withLoader(echarts4rOutput(ns('plot_rf'), height = "75vh"), type = "html", loader = "loader4"))
  
  prediction.rf.panel <- tabPanel(title = labelInput("predm"), 
                                  value = "tabRfPred",
                                  withLoader(DT::dataTableOutput(ns("rfPrediTable")), type = "html", loader = "loader4"))
  
  disp.rf.panel <- tabPanel(title = labelInput("dispersion"), 
                            value = "tabRfDisp",
                            withLoader(echarts4rOutput(ns('plot_rf_disp'),height = "75vh"), type = "html", loader = "loader4"))
  
  general.index.rf.panel <- tabPanel(title = labelInput("indices"), value = "tabRfIndex",
                                     br(),
                                     div(withLoader(tableOutput(ns('indexdfrf')), type = "html", loader = "loader4")),
                                     br(),
                                     div(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     div(withLoader(tableOutput(ns('indexdfrf2')), type = "html", loader = "loader4")))
  
  rf.rules.panel <- tabPanel(title = labelInput("reglas"), 
                             value = "tabRfRules",
                             withLoader(verbatimTextOutput(ns("rulesRf")), 
                                        type = "html", 
                                        loader = "loader4"))
  
  ntree.rf.panel <- tabPanel(title = labelInput("evolerror"), 
                             value = "tabRfRMSE",
                             withLoader(echarts4rOutput(ns('plot_rf_rmse'), height = "75vh"), 
                                        type = "html", 
                                        loader = "loader4"))
   
  page.rf <- tabItem(tabName = "rf",
                     tabBoxPrmdt(id = ns("BoxRf"), 
                                 opciones = tabs.options,
                                 generate.rf.panel,
                                 plot.rf,
                                 prediction.rf.panel,
                                 ntree.rf.panel,
                                 disp.rf.panel,
                                 general.index.rf.panel,
                                 rf.rules.panel))
  
  
  tagList(
    page.rf
  )
}

#' r_forest Server Function
#'
#' @noRd 
mod_r_forest_server <- function(input, output, session,updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  
  nombreModelo <- "modelo.rf"
  
  
  observeEvent(c(updateData$datos), {
    modelos2$rf = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
  return.rf.default.values <- function(){
    updateNumericInput(session = session, inputId = "ntree.rf", value = 20)
    updateNumericInput(session,"mtry.rf",value = 1)
    
    isolate(datos.aprendizaje <- updateData$datos.aprendizaje)
    if(!is.null(datos.aprendizaje)){
      mtry.value <- round(sqrt(ncol(datos.aprendizaje) - 1))
      updateNumericInput(session,"mtry.rf",value = mtry.value)
    }
  }
  
  
  observeEvent(updateData$datos.aprendizaje,{
    return.rf.default.values()
  })
  
  
  
  #Update model tab
  output$txtRf <- renderPrint({
    input$runRf
    tryCatch({
      codigo.rf()
        isolate({
          datos.aprendizaje <- updateData$datos.aprendizaje
          datos.prueba      <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
          ntree <- input$ntree.rf
          mtry  <- input$mtry.rf
        })
        
        #Validacion tamaño del mtry
        tam <- ncol(datos.aprendizaje)
        if(mtry >= tam){
          mtry <- tam - 1
          updateNumericInput(session, "mtry.rf", value = mtry)
        }
        
        form <- formula(paste0(variable.predecir,"~."))
        
        #Model generate
        modelo <- train.randomForest(form, data = datos.aprendizaje, ntree = ntree, 
                                        mtry = mtry, importance = TRUE)

        #Prediccion
        prediccion <- predict(modelo,datos.prueba)$prediction

        #Indices
        indices.rf <- general_indices(datos.prueba[,variable.predecir], prediccion)

        #isolamos para que no entre en un ciclo en el primer renderPrint
        isolate(modelos$rf[[nombreModelo]] <- list(modelo = modelo, prediccion = prediccion, indices = indices.rf))
        
        
        isolate({
          modelos$rf[[nombreModelo]] <- list(modelo = modelo, prediccion = prediccion, indices = indices.rf)
          modelos2$rf$n <- modelos2$rf$n + 1
          modelos2$rf$mcs[modelos2$rf$n] <- list(indices.rf)
          if(modelos2$rf$n > 9)
            modelos2$rf$n <- 0
          
        })
        
        if(!is.null(modelos$rf[[nombreModelo]])){
          modelo <- modelos$rf[[nombreModelo]]$modelo
          #Cambiamos la forma en que va aparecer el call
          # Guardamos los datos dentro del modelo para utilizar 
          #la función printRandomForest() en Utilities
          modelo$datos <- datos.aprendizaje
          #Cambiamos la forma en que va aparecer el call
          modelo$call$formula <- form
          modelo$call$ntree   <- ntree
          modelo$call$mtry    <- mtry
          
          print(modelo)
        }
        else{NULL}
    }, error = function(e){
      isolate(modelos$rf[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (RF-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update importance chart
  output$plot_rf <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rf[[nombreModelo]])){
        
        modelo <- modelos$rf[[nombreModelo]]$modelo
        idioma    <- codedioma$idioma
        
        # Actualiza el codigo del grafico de rf
        codigo <- "importance_plot_rf(modelo.rf)"
        updateAceEditor(session, "fieldCodeRfPlot", value = codigo)
        
        titulos <- c(
          tr("impVarA", idioma),
          tr("IncMSE", idioma),
          tr("variable", idioma)
        )
        
        importance_plot_rf(modelo,titulos)
      }else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RF-02) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  # Update prediction tab
  output$rfPrediTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$rf[[nombreModelo]])){
        prediccion <- modelos$rf[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          real.val     <- datos.prueba[updateData$variable.predecir]
        })
        tb_predic(real.val, prediccion, updateData$decimals, codedioma$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (RF-03) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  # Update rmse tab
  output$plot_rf_rmse <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rf[[nombreModelo]])){
        df_plot <- rf_ntree_values(modelos$rf[[nombreModelo]]$modelo)
        plot_RMSEK(datos = df_plot ,titles = get_title("RF", codedioma$idioma))
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (RF-03) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  # Update Dispersion Tab
  output$plot_rf_disp <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rf[[nombreModelo]])){
        prediccion <- modelos$rf[[nombreModelo]]$prediccion
        isolate({
          datos.prueba      <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
        })
        idioma <- codedioma$idioma
        
        codigo <- disp_models(nombreModelo, tr("rf", idioma), variable.predecir)
        cod    <- paste0("### docdisp\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        titulos <- c(
          tr("predvsreal", idioma),
          tr("realValue", idioma),
          tr("pred", idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion,
                             tr("rf", idioma),titulos)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RF-04) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update Indices tab
  output$indexdfrf <- renderTable({
    tryCatch({
      if(!is.null(modelos$rf[[nombreModelo]])){
        idioma     <- codedioma$idioma
        indices.rf <- modelos$rf[[nombreModelo]]$indices
        tabla.indicesPrecision(indices.rf, updateData$decimals, idioma)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RF-05) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
  
  
  output$indexdfrf2 <- renderTable({
    tryCatch({
      if(!is.null(modelos$rf[[nombreModelo]])){
        idioma   <- codedioma$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(summary_indices(updateData$datos.prueba[,updateData$variable.predecir]),
                              decimals, 
                              idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (RF-06) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
  
  
  
  # Update Rules Tab
  output$rulesRf <- renderPrint({
    tryCatch({
      n <- input$rules.rf.n
      if(!is.null(modelos$rf[[nombreModelo]]) && !is.na(n)){
        modelo <- modelos$rf[[nombreModelo]]$modelo
        codigo <- paste0("printRandomForests(modelo.rf, ",n,", format='VB')")
        cod    <- paste0("### reglas\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        rulesRandomForest(modelo, n, format='VB')
      }
      else{NULL}
    },
    error = function(e){
      showNotification(paste0("Error (RF-07) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  # Execute model, prediction and indices
  codigo.rf <- function(){
    tryCatch({
      isolate({
        variable.predecir <- updateData$variable.predecir
        ntree <- input$ntree.rf
        mtry  <- input$mtry.rf
      })
      
      #Model generate
      codigo <- codeRf(variable.predecir, ntree, mtry)
      cod    <- paste0("### RF\n", codigo)
      
      #Prediccion
      codigo <- codigo.prediccion("rf")
      cod    <- paste0(cod, codigo)
      #Indices
      codigo <- codigo.IG(model.name = "rf", variable.pr = variable.predecir)
      cod    <- paste0(cod, codigo)
      
      isolate(codedioma$code <- append(codedioma$code, cod))
      
    }, error = function(e){
      showNotification(paste0("Error (RF-00) : ",e), duration = 10, type = "error")
    })
  }
  
}

## To be copied in the UI
# mod_r_forest_ui("r_forest_ui_1")

## To be copied in the server
# callModule(mod_r_forest_server, "r_forest_ui_1")

