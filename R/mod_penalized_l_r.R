#' penalized_l_r UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_penalized_l_r_ui <- function(id){
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
  
  tabs.options.generate <- tabsOptions(heights = c(70),
                                       tabs.content = list(rlr.options))

  
  tabs.options <- list(conditionalPanel("input.BoxRlr == 'tabRlrModelo'",tabs.options.generate,ns = ns))
  
  
  generate.rlr.panel <- tabPanel(title = labelInput("generatem"),value = "tabRlrModelo",
                                 withLoader(verbatimTextOutput(ns("txtRlr")),type = "html", loader = "loader4"))
  
  posib.landa.rlr.panel <- tabPanel(title = labelInput("posibLanda"),value = "tabRlrPosibLanda",
                                    withLoader(echarts4rOutput(ns('plot_rlr_posiblanda'),height = "75vh"),type = "html", loader = "loader4"))
  
  coeff.rlr.panel <- tabPanel(title = labelInput("coeff"),value = "tabRlrCoeff",
                              withLoader(DT::dataTableOutput(ns("dtRlrCoeff")),type = "html", loader = "loader4"))
  
  landa.rlr.panel <- tabPanel(title = labelInput("gcoeff"),value = "tabRlrCoeff_landa",
                              withLoader(echarts4rOutput(ns('plot_rlr_Coeff_landa'),height = "75vh"),type = "html", loader = "loader4"))
  
  prediccion.rlr.panel <- tabPanel(title = labelInput("predm"), value = "tabRlrPred",
                                   withLoader(DT::dataTableOutput(ns("rlrPrediTable")),type = "html", loader = "loader4"))
  
  disp.rlr.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRlrDisp",
                             withLoader(echarts4rOutput(ns('plot_rlr_disp'),height = "75vh"),type = "html", loader = "loader4"))
  
  rlr.general.index.panel <- tabPanel(title = labelInput("indices"), value = "tabRlrIndex",
                                      br(),
                                      div(withLoader(tableOutput(ns('indexdfrlr')),type = "html", loader = "loader4")),
                                      br(),
                                      div(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                      br(),
                                      div(withLoader(tableOutput(ns('indexdfrlr2')),type = "html", loader = "loader4")))
  
  
  page.rlr <- tabItem(tabName = "rlr",
                      tabBoxPrmdt(id = ns("BoxRlr"), opciones = tabs.options,
                             generate.rlr.panel,
                             posib.landa.rlr.panel,
                             landa.rlr.panel,
                             coeff.rlr.panel,
                             prediccion.rlr.panel,
                             disp.rlr.panel,
                             rlr.general.index.panel))
  
  tagList(
    page.rlr
  )
}

#' penalized_l_r Server Function
#'
#' @noRd 
mod_penalized_l_r_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  
  nombreBase   <- "modelo.rlr."
  nombreModelo <- "modelo.rlr."
  log.landa    <- NULL
  coefficients <- NULL
  
  observeEvent(c(updateData$datos), {
    modelos2$rlr = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
  return.rlr.default.values <- function(){
    updateSelectInput(session, "alpha.rlr",selected = 1)
    updateRadioSwitch(session,"switch_scale_rlr","TRUE")
    updateNumericInput(session,"log_landa",value = 2)
    updateRadioSwitch(session,"permitir_landa","FALSE")

    log.landa    <<- NULL
    coefficients <<- NULL
  }

  # When user press enable or disable the lambda
  observeEvent(input$permitir_landa, {
    if (as.logical(input$permitir_landa)) {
      shinyjs::enable("log_landa")
    } else {
      shinyjs::disable("log_landa")
    }
  })
  
  
  #Update model tab
  output$txtRlr <- renderPrint({
    input$runRlr
    tryCatch({
      codigo.penalized.regression()
      
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba      <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        alpha             <- as.numeric(input$alpha.rlr)
        standardize       <- as.logical(input$switch_scale_rlr)
      })
      
      nombreModelo <<- paste0(nombreBase, rlr_type(alpha))
      
      
      #Model generate
      modelo.rlr <- rlr_model(data = datos.aprendizaje, variable.pred = variable.predecir,
                               alpha = alpha, standardize = standardize)

      if (isolate(as.logical(input$permitir_landa) && !is.na(isolate(input$log_landa)))) {
        log.landa <- isolate(input$log_landa)
      }
      else{
        log.landa <- NULL
        }
      # Coefficients
      coefficients <- coef_lambda(data          = datos.aprendizaje, 
                                   variable.pred = variable.predecir,
                                   model         = modelo.rlr, 
                                   log.lambda    = log.landa)
      # Prediction
      prediccion.rlr <- rlr_prediction(modelo.rlr, 
                                       datos.prueba, 
                                       variable.predecir,
                                       log.lambda = log.landa)
      #Indices
      indices.rlr <- general_indices(datos.prueba[,variable.predecir], 
                                     prediccion.rlr)
      
      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$rlr[[nombreModelo]] <- list(modelo     = modelo.rlr, 
                                                  prediccion = prediccion.rlr, 
                                                  indices  = indices.rlr,
                                                  id       = rlr_type(alpha), 
                                                  coefficients = coefficients))
      
      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate({
        modelos$rlr[[nombreModelo]] <- list(modelo     = modelo.rlr, 
                                            prediccion = prediccion.rlr, 
                                            indices  = indices.rlr,
                                            id       = rlr_type(alpha), 
                                            coefficients = coefficients)
        modelos2$rlr$n <- modelos2$rlr$n + 1
        modelos2$rlr$mcs[modelos2$rlr$n] <- list(indices.rlr)
        if(modelos2$rlr$n > 9)
          modelos2$rlr$n <- 0
        
      })
      
      if(!is.null(modelos$rlr[[nombreModelo]])){
        modelos.rlr <- modelos$rlr[[nombreModelo]]$modelo
        print(modelos.rlr)
      }
      else{NULL}
      
    }, error = function(e){
      isolate(modelos$rlr[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (RLR-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  output$plot_rlr_posiblanda <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rlr[[nombreModelo]])){
        titulos <- c(
          tr("MSE",          codedioma$idioma),
          tr("lowCurve",     codedioma$idioma),
          tr("uppCurve",     codedioma$idioma),
          tr("selected",     codedioma$idioma),
          tr("automatico",   codedioma$idioma),
          tr("nonZeroCoeff", codedioma$idioma)
        )
        log.landa    <- isolate(input$log_landa)
        param.lambda <- ifelse(is.null(log.landa),"",paste0(", log.lambda = ",log.landa))
        codigo       <- paste0("e_posib_lambda(", nombreModelo, param.lambda, ")")
        cod          <- paste0("### posibLanda\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
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
        codigo       <- paste0("e_coeff_landa(", nombreModelo, param.lambda, ")")
        cod    <- paste0("### gcoeff\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        log.landa <- isolate(input$log_landa)
        titulos <- c(
          tr("coeff", codedioma$idioma),
          tr("selected", codedioma$idioma),
          tr("automatico", codedioma$idioma)
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
        coefficients <- modelos$rlr[[nombreModelo]]$coefficients
        dttable.custom(data.frame(row.names = row.names(coefficients),
                                  coeff     = as.vector(coefficients)), 
                       decimals     = updateData$decimals,
                       translatable = TRUE, 
                       language     = isolate(codedioma$idioma))
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
          real.val     <- datos.prueba[updateData$variable.predecir]
        })
        
        tb_predic(real.val, prediccion.rlr, updateData$decimals, codedioma$idioma)
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
          datos.prueba      <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
          alpha             <- as.numeric(input$alpha.rlr)
        })
        
        tipo   <- rlr_type(alpha)
        
        codigo <- disp_models(nombreModelo, 
                              paste0(tr("rlr", codedioma$idioma),"-",tipo), 
                              variable.predecir)
        cod    <- paste0("### docdisp\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        titulos <- c(
          tr("predvsreal", codedioma$idioma),
          tr("realValue",  codedioma$idioma),
          tr("pred",       codedioma$idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],
                             prediccion.rlr,
                             paste0(tr("rlr", codedioma$idioma),"-",tipo),
                             titulos)
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
        idioma      <- codedioma$idioma
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
      if(!is.null(modelos$rlr[[nombreModelo]])){
        idioma   <- codedioma$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(summary_indices(updateData$datos.prueba[,updateData$variable.predecir]),
                              decimals, 
                              idioma)
        }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (RL-08) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
  codigo.penalized.regression <- function() {
    isolate({
      datos.aprendizaje <- updateData$datos.aprendizaje
      datos.prueba      <- updateData$datos.prueba
      variable.predecir <- updateData$variable.predecir
      alpha       <- as.numeric(input$alpha.rlr)
      standardize <- as.logical(input$switch_scale_rlr)
    })
    
    nombreModelo <- paste0(nombreBase, rlr_type(alpha))

    #Model generate
    codigo <- codeRlr(variable.predecir,alpha,standardize)
    cod    <- paste0("### regpen\n", codigo)
    
    #Coefficients
    codigo <- codeRlrCoeff(variable.predecir,
                           nombreModelo,log.landa)
    cod    <- paste0(cod, codigo)
    #Prediccion
    codigo <- codeRlrPred("rlr",variable.predecir,
                          log.landa)
    cod    <- paste0(cod, codigo)
    #Indices
    codigo <- codigo.IG(model.name = "rlr", variable.pr = variable.predecir)
    cod    <- paste0(cod, codigo)
    
    isolate(codedioma$code <- append(codedioma$code, cod))


  }
}

## To be copied in the UI
# mod_penalized_l_r_ui("penalized_l_r_ui_1")

## To be copied in the server
# callModule(mod_penalized_l_r_server, "penalized_l_r_ui_1")