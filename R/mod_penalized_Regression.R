#' penalized_Regression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_penalized_Regression_ui <- function(id){
  ns <- NS(id)
  
  
  rlr.options <- list(options.run(ns("runRlr")), tags$hr(style = "margin-top: 0px;"),
                      fluidRow(
                        column(selectInput(inputId = ns("alpha.rlr"), label = labelInput("selectAlg"), selected = 1,
                                           choices = list("Ridge" = 0, "Lasso" = 1)),width = 5),
                        column(width = 5,radioSwitch(id = ns("switch_scale_rlr"), label = "escal", 
                                                 names = c("si", "no")))),
                      fluidRow(column(id = ns("colManualLanda"),width = 5, 
                                      numericInput(ns("log_landa"), labelInput("log_landa"),value = 2, "NULL", width = "100%")),
                               column(width = 5,
                                      radioSwitch(id = ns("permitir_landa"), label = "",
                                                  names = c("manual", "automatico"), val.def = FALSE))))
  
  
  rlr.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                          codigo.monokai(ns("fieldCodeRlr"), height = "7vh"))
  
  
  rlr.code  <- list(fluidRow(column(width = 9, h3(labelInput("codigo")))),
                    hr(style = "margin-top: 0px;"),
                    conditionalPanel("input.BoxRlr == 'tabRlrPosibLanda'",
                                     aceEditor(ns("fieldCodeRlrPosibLanda"), mode = "r", theme = "monokai",
                                               value = "", height = "7vh", readOnly = F, autoComplete = "enabled"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrCoeff_landa'",
                                     codigo.monokai(ns("fieldCodeRlrCoeff_landa"), height = "7vh"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrCoeff'",
                                     codigo.monokai(ns("fieldCodeRlrCoeff"), height = "7vh"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrPred'",
                                     codigo.monokai(ns("fieldCodeRlrPred"), height = "7vh"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrDisp'",
                                     codigo.monokai(ns("fieldCodeRlrDisp"), height = "7vh"),ns = ns),
                    conditionalPanel("input.BoxRlr == 'tabRlrIndex'",
                                     codigo.monokai(ns("fieldCodeRlrIG"),height = "7vh"),ns = ns))
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("gear"), icon("code")), widths = c(50,100), heights = c(80,70),
                                       tabs.content = list(rlr.options,rlr.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(70),
                                         tabs.content = list(rlr.code))
  
  
  generate.rlr.panel <- tabPanel(title = labelInput("generatem"),value = "tabRlrModelo",
                                 withLoader(verbatimTextOutput(ns("txtRlr")),type = "html", loader = "loader4"))
  
  posib.landa.rlr.panel <- tabPanel(title = labelInput("posibLanda"),value = "tabRlrPosibLanda",
                                    echarts4rOutput(ns('plot_rlr_posiblanda'), height = "80vh"))
  
  coeff.rlr.panel <- tabPanel(title = labelInput("coeff"),value = "tabRlrCoeff",
                              withLoader(DT::dataTableOutput(ns("dtRlrCoeff")),type = "html", loader = "loader4"))
  
  landa.rlr.panel <- tabPanel(title = labelInput("gcoeff"),value = "tabRlrCoeff_landa",
                              echarts4rOutput(ns('plot_rlr_Coeff_landa'), height = "75vh"))
  
  prediccion.rlr.panel <- tabPanel(title = labelInput("predm"), value = "tabRlrPred",
                                   withLoader(DT::dataTableOutput(ns("rlrPrediTable")),type = "html", loader = "loader4"))
  
  disp.rlr.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRlrDisp",
                             echarts4rOutput(ns('plot_rlr_disp'), height = "75vh"))
  
  rlr.general.index.panel <- tabPanel(title = labelInput("indices"), value = "tabRlrIndex",
                                      br(),
                                      fluidRow(withLoader(tableOutput(ns('indexdfrlr')),type = "html", loader = "loader4")),
                                      br(),
                                      fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                      br(),
                                      fluidRow(withLoader(tableOutput(ns('indexdfrlr2')),type = "html", loader = "loader4")))
  
  
  page.rlr <- tabItem(tabName = "rlr",
                      tabBox(id = ns("BoxRlr"), width = NULL, height ="80%",
                             generate.rlr.panel,
                             posib.landa.rlr.panel,
                             landa.rlr.panel,
                             coeff.rlr.panel,
                             prediccion.rlr.panel,
                             disp.rlr.panel,
                             rlr.general.index.panel,
                             conditionalPanel("input.BoxRlr == 'tabRlrModelo'",tabs.options.generate,ns = ns),
                             conditionalPanel("input.BoxRlr != 'tabRlrModelo'",tabs.options.Nogenerate,ns = ns)
                      ))
  
  tagList(
    page.rlr
  )
}

#' penalized_Regression Server Function
#'
#' @noRd 
mod_penalized_Regression_server <- function(input, output, session, updateData, modelos){
  ns <- session$ns
  
  nombreBase <- "modelo.rlr."
  nombreModelo <- "modelo.rlr."
  log.landa <- NULL
  coefficients <- NULL
  
  return.rlr.default.values <- function(){
    updateSelectInput(session, "alpha.rlr",selected = 1)
    updateRadioSwitch(session,"switch_scale_rlr","TRUE")
    updateNumericInput(session,"log_landa",value = 2)
    updateRadioSwitch(session,"permitir_landa","FALSE")

    log.landa <<- NULL
    coefficients <<- NULL
  }
  
  
  observeEvent(updateData$datos.aprendizaje,{
    return.rlr.default.values()
  })
  
  # When the rlr model is generated
  observeEvent(input$runRlr, {
    if (validate_data(updateData, idioma = updateData$idioma)) { # If you have the data then :
      rlr_full()
    }
  })
  
  # When user press enable or disable the lambda
  observeEvent(input$permitir_landa, {
    if (as.logical(input$permitir_landa)) {
      shinyjs::enable("log_landa")
    } else {
      shinyjs::disable("log_landa")
    }
  })
  
  # Execute model, prediction and indices
  rlr_full <- function(){
    tryCatch({
      shinyjs::runjs(code = "generating_model = true")
      
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        alpha <- as.numeric(input$alpha.rlr)
        standardize <- as.logical(input$switch_scale_rlr)
      })

      nombreModelo <<- paste0(nombreBase, rlr_type(alpha))
      
      #Model generate
      modelo.rlr <- rlr_model(data = datos.aprendizaje, variable.pred = variable.predecir,
                              alpha = alpha, standardize = standardize)
      updateAceEditor(session, "fieldCodeRlr", value = codeRlr(variable.predecir,alpha,standardize))
      
      if (isolate(as.logical(input$permitir_landa) && !is.na(isolate(input$log_landa)))) {
        log.landa <<- isolate(input$log_landa)
      }
      else{log.landa <<- NULL}
      
      # Coefficients
      coefficients <<- coef_lambda(data = datos.aprendizaje, variable.pred = variable.predecir,
                                   model = modelo.rlr, log.lambda = log.landa)
      updateAceEditor(session, "fieldCodeRlrCoeff", value = codeRlrCoeff(variable.predecir,
                                                                         nombreModelo,log.landa))

      # Prediction
      prediccion.rlr <- rlr_prediction(modelo.rlr, datos.prueba, variable.predecir,
                                       log.lambda = log.landa)
      updateAceEditor(session, "fieldCodeRlrPred", value = codeRlrPred(nombreModelo,variable.predecir,
                                                                       log.landa))
      
      
      #Indices
      indices.rlr <- general_indices(datos.prueba[,variable.predecir], prediccion.rlr)
      updateAceEditor(session, "fieldCodeRlrIG", value = codeRlrIG(variable.predecir))

      
      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$rlr[[nombreModelo]] <- list(modelo = modelo.rlr, prediccion = prediccion.rlr, indices = indices.rlr,
                                                  id = rlr_type(alpha)))
    }, error = function(e){
      isolate(modelos$rlr[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (RLR-00) : ",e), duration = 10, type = "error")
    },
    finally = {
      shinyjs::runjs(code = "generating_model = false")
    })
  }
  
  
  #Update model tab
  output$txtRlr <- renderPrint({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        modelos.rlr <- modelos$rlr[[nombreModelo]]$modelo
        print(modelos.rlr)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RLR-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  output$plot_rlr_posiblanda <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        titulos <- c(
          tr("MSE", updateData$idioma),
          tr("lowCurve", updateData$idioma),
          tr("uppCurve", updateData$idioma),
          tr("selected", updateData$idioma),
          tr("automatico", updateData$idioma),
          tr("nonZeroCoeff", updateData$idioma)
        )
        
        param.lambda <- ifelse(is.null(log.landa),"",paste0(", log.lambda = ",log.landa))
        codigo <- paste0("e_posib_lambda(", nombreModelo, param.lambda, ")")
        updateAceEditor(session, "fieldCodeRlrPosibLanda", value = codigo)
        
        e_posib_lambda(modelos$rlr[[nombreModelo]]$modelo, log.landa, titulos)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (RLR-02) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  
  output$plot_rlr_Coeff_landa <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        param.lambda <- ifelse(is.null(log.landa),"",paste0(", log.lambda = ",log.landa))
        codigo <- paste0("e_coeff_landa(", nombreModelo, param.lambda, ")")
        updateAceEditor(session, "fieldCodeRlrCoeff_landa", value = codigo)
        
        titulos <- c(
          tr("coeff", updateData$idioma),
          tr("selected", updateData$idioma),
          tr("automatico", updateData$idioma)
        )
        
        e_coeff_landa(modelos$rlr[[nombreModelo]]$modelo, log.landa, titulos)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RLR-03) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update coefficients tab
  output$dtRlrCoeff <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        dttable.custom(data.frame(row.names = row.names(coefficients),coeff = as.vector(coefficients)), 
                       decimals = updateData$decimals,translatable = TRUE, language = isolate(updateData$idioma))
      }
      else{
        NULL
      }
    }, error = function(e){
      showNotification(paste0("Error (RLR-04) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = FALSE)
  
  
  # Update prediction tab
  output$rlrPrediTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        prediccion.rlr <- modelos$rlr[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          real.val <- datos.prueba[updateData$variable.predecir]
        })
        tb_predic(real.val, prediccion.rlr, updateData$decimals, updateData$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (RLR-05) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  
  # Update dispersion tab
  output$plot_rlr_disp <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        prediccion.rlr <- modelos$rlr[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
          alpha <- as.numeric(input$alpha.rlr)
        })
        tipo <- rlr_type(alpha)
        
        codigo <- disp_models(nombreModelo, paste0(tr("rlr", updateData$idioma),"-",tipo), variable.predecir)
        updateAceEditor(session, "fieldCodeRlrDisp", value = codigo)
        
        titulos <- c(
          tr("predvsreal", updateData$idioma),
          tr("realValue", updateData$idioma),
          tr("pred", updateData$idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion.rlr,
                             paste0(tr("rlr", updateData$idioma),"-",tipo),titulos)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RLR-06) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update Indices tab
  output$indexdfrlr <- renderTable({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        idioma <- updateData$idioma
        indices.rlr <- modelos$rlr[[nombreModelo]]$indices
        tabla.indicesPrecision(indices.rlr, updateData$decimals, idioma)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RL-07) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
  
  
  output$indexdfrlr2 <- renderTable({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])& !is.null(updateData$summary.var.pred)){
        idioma <- updateData$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(updateData$summary.var.pred, decimals, idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (RL-08) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
}

## To be copied in the UI
# mod_penalized_Regression_ui("penalized_Regression_ui_1")

## To be copied in the server
# callModule(mod_penalized_Regression_server, "penalized_Regression_ui_1")