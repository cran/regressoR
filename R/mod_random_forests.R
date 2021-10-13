#' random_forests UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_random_forests_ui <- function(id){
  
  ns <- NS(id)
  
  rf.options <- list(conditionalPanel("input.BoxRf != 'tabRfRules'",
                                      options.run(ns("runRf")), tags$hr(style = "margin-top: 0px;"),
                                      fluidRow(column(numericInput(ns("ntree.rf"), labelInput("numTree"), 20, width = "100%", min = 0), width = 5),
                                               column(numericInput(ns("mtry.rf"),labelInput("numVars"),1, width = "100%", min = 1), width=5)), ns = ns),
                     conditionalPanel("input.BoxRf == 'tabRfRules'",
                                      numericInput(ns("rules.rf.n"),labelInput("ruleNumTree"),1, width = "25%", min = 1),ns = ns))
  
  rf.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                         conditionalPanel("input.BoxRf == 'tabRfModelo'",codigo.monokai(ns("fieldCodeRf"), height = "7vh"), ns = ns),
                         conditionalPanel("input.BoxRf == 'tabRfRules'",codigo.monokai(ns("fieldCodeRfRules"), height = "7vh"), ns = ns))
  
  
  rf.code  <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                   conditionalPanel("input.BoxRf == 'tabRfImp'",
                                    codigo.monokai(ns("fieldCodeRfPlot"), height = "7vh"),ns = ns),
                   conditionalPanel("input.BoxRf == 'tabRfPred'",
                                    codigo.monokai(ns("fieldCodeRfPred"), height = "7vh"),ns = ns),
                   conditionalPanel("input.BoxRf == 'tabRfDisp'",
                                    codigo.monokai(ns("fieldCodeRfDisp"), height = "7vh"),ns = ns),
                   conditionalPanel("input.BoxRf == 'tabRfIndex'",
                                    codigo.monokai(ns("fieldCodeRfIG"), height = "7vh"),ns = ns))
  
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("cog"), icon("code")), widths = c(50,100), heights = c(80,70),
                                       tabs.content = list(rf.options,rf.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(70),
                                         tabs.content = list(rf.code))
  
  tabs.options <- list(conditionalPanel("input.BoxRf == 'tabRfModelo' || input.BoxRf == 'tabRfRules'",tabs.options.generate,ns = ns),
                       conditionalPanel("input.BoxRf != 'tabRfModelo' && input.BoxRf != 'tabRfRules'",tabs.options.Nogenerate,ns = ns))
  
  generate.rf.panel <- tabPanel(title = labelInput("generatem"),value = "tabRfModelo",
                                withLoader(verbatimTextOutput(ns("txtRf")),type = "html", loader = "loader4"))
  
  plot.rf <- tabPanel(title = labelInput("varImp"), value = "tabRfImp",
                      withLoader(echarts4rOutput(ns('plot_rf'),height = "75vh"),type = "html", loader = "loader4"))
  
  prediction.rf.panel <- tabPanel(title = labelInput("predm"), value = "tabRfPred",
                                  withLoader(DT::dataTableOutput(ns("rfPrediTable")),type = "html", loader = "loader4"))
  
  disp.rf.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRfDisp",
                            withLoader(echarts4rOutput(ns('plot_rf_disp'),height = "75vh"),type = "html", loader = "loader4"))
  
  general.index.rf.panel <- tabPanel(title = labelInput("indices"), value = "tabRfIndex",
                                     br(),
                                     fluidRow(withLoader(tableOutput(ns('indexdfrf')),type = "html", loader = "loader4")),
                                     br(),
                                     fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     fluidRow(withLoader(tableOutput(ns('indexdfrf2')),type = "html", loader = "loader4")))
  
  rf.rules.panel <- tabPanel(title = labelInput("reglas"), value = "tabRfRules",
                             withLoader(verbatimTextOutput(ns("rulesRf")),type = "html", loader = "loader4"))
  
  page.rf <- tabItem(tabName = "rf",
                     tabBoxPrmdt(id = ns("BoxRf"), opciones = tabs.options,
                            generate.rf.panel,
                            plot.rf,
                            prediction.rf.panel,
                            disp.rf.panel,
                            general.index.rf.panel,
                            rf.rules.panel))
  
  
  tagList(
    page.rf
  )
}

#' random_forests Server Function
#'
#' @noRd 
mod_random_forests_server <- function(input, output, session,updateData, modelos){
  ns <- session$ns
  
  nombreModelo <- "modelo.rf"
  
  
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
  
  
  observeEvent(input$runRf, {
    if (validate_data(updateData, idioma = updateData$idioma)) { # Si se tiene los datos entonces :
      rf_full()
    }
  })
  
  
  # Execute model, prediction and indices
  rf_full <- function(){
    tryCatch({
      shinyjs::runjs(code = "generating_model = true")
      
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        ntree <- input$ntree.rf
        mtry <- input$mtry.rf
      })
      
      #Validacion tamaño del mtry
      tam <- ncol(datos.aprendizaje)
      if(mtry >= tam){
        mtry <- tam - 1
        updateNumericInput(session, "mtry.rf", value = mtry)
      }
      
      #Model generate
      modelo.rf <- rf_model(datos.aprendizaje,variable.predecir, ntree, mtry)
      updateAceEditor(session, "fieldCodeRf", value = codeRf(variable.predecir, ntree, mtry))
      
      #Prediccion
      prediccion.rf <- rf_prediction(modelo.rf, datos.prueba)
      updateAceEditor(session, "fieldCodeRfPred", value = codeRfPred(nombreModelo))
      
      #Indices
      indices.rf <- general_indices(datos.prueba[,variable.predecir], prediccion.rf)
      updateAceEditor(session, "fieldCodeRfIG", value = codeRfIG(variable.predecir))
      
      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$rf[[nombreModelo]] <- list(modelo = modelo.rf, prediccion = prediccion.rf, indices = indices.rf))
      
    }, error = function(e){
      isolate(modelos$rf[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (RF-00) : ",e), duration = 10, type = "error")
    },
    finally = {shinyjs::runjs(code = "generating_model = false")})
  }
  
  #Update model tab
  output$txtRf <- renderPrint({
    tryCatch({
      if(!is.null(modelos$rf[[nombreModelo]])){
        modelo.rf <- modelos$rf[[nombreModelo]]$modelo
        print(modelo.rf)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RF-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update importance chart
  output$plot_rf <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rf[[nombreModelo]])){
        
        modelo.rf <- modelos$rf[[nombreModelo]]$modelo
        idioma <- updateData$idioma
        
        # Actualiza el codigo del grafico de rf
        codigo <- "importance_plot_rf(modelo.rf)"
        updateAceEditor(session, "fieldCodeRfPlot", value = codigo)
        
        titulos <- c(
          tr("impVarA", idioma),
          tr("IncMSE", idioma),
          tr("variable", idioma)
        )
        
        importance_plot_rf(modelo.rf,titulos)
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
        prediccion.rf <- modelos$rf[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          real.val <- datos.prueba[updateData$variable.predecir]
        })
        tb_predic(real.val, prediccion.rf, updateData$decimals, updateData$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (RF-03) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  
  # Update Dispersion Tab
  output$plot_rf_disp <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rf[[nombreModelo]])){
        prediccion.rf <- modelos$rf[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
        })
        idioma <- updateData$idioma
        
        codigo <- disp_models(nombreModelo, tr("rf", idioma), variable.predecir)
        updateAceEditor(session, "fieldCodeRfDisp", value = codigo)
        
        titulos <- c(
          tr("predvsreal", idioma),
          tr("realValue", idioma),
          tr("pred", idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion.rf,
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
        idioma <- updateData$idioma
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
      if(!is.null(modelos$rf[[nombreModelo]])& !is.null(updateData$summary.var.pred)){
        idioma <- updateData$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(updateData$summary.var.pred, decimals, idioma)
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
        modelo.rf <- modelos$rf[[nombreModelo]]$modelo
        updateAceEditor(session,"fieldCodeRfRules",paste0("printRandomForests(modelo.rf, ",n,", format='VB')"))
        rulesRandomForest(modelo.rf, n, format='VB')
      }
      else{NULL}
    },
    error = function(e){
      showNotification(paste0("Error (RF-07) : ",e), duration = 10, type = "error")
      NULL
    })
  })
}

## To be copied in the UI
# mod_random_forests_ui("random_forests_ui_1")

## To be copied in the server
# callModule(mod_random_forests_server, "random_forests_ui_1")

