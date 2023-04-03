#' l_regression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_l_regression_ui <- function(id){
  ns <- NS(id)

  opc_rl <- div(
    conditionalPanel(
      "input['l_regression_ui_1-BoxRl'] == 'tabRlModelo'",
      tabsOptions(heights = c(70), tabs.content = list(
        list(
          options.run(ns("runRl")), tags$hr(style = "margin-top: 0px;"))
        
      )))
  )

  generate.rl.panel <- tabPanel(title = labelInput("generatem"),
                                value = "tabRlModelo",
                                withLoader(verbatimTextOutput(ns("txtRl")),
                                           type   = "html", 
                                           loader = "loader4"))
  
  coefficients.rl.panel <- tabPanel(title = labelInput("coeff"), 
                                    value = "tabRlCoef",
                                    withLoader(DT::dataTableOutput(ns("rlCoefTable")),
                                               type   = "html", 
                                               loader = "loader4"))
  
  prediccion.rl.panel <- tabPanel(title = labelInput("predm"), 
                                  value = "tabRlPred",
                                  withLoader(DT::dataTableOutput(ns("rlPrediTable")),
                                             type   = "html", 
                                             loader = "loader4"))
  
  disp.rl.panel <- tabPanel(title = labelInput("dispersion"), 
                            value = "tabRlDisp",
                            withLoader(echarts4rOutput(ns('plot_rl_disp'),height = "75vh"),
                                       type   = "html", 
                                       loader = "loader4"))
  
  rl.general.index.panel <- tabPanel(title = labelInput("indices"), 
                                     value = "tabRlIndex",
                                     br(),
                                     div(withLoader(tableOutput(ns('indexdfrl')),
                                                         type   = "html", 
                                                         loader = "loader4")),
                                     br(),
                                     div(column(width = 12, 
                                                     align = "center", 
                                                     tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     div(withLoader(tableOutput(ns('indexdfrl2')),
                                                         type   = "html", 
                                                         loader = "loader4")))
  
  
  page.rl <- tabItem(tabName = "rl",
                     tabBoxPrmdt(id       = ns("BoxRl"), 
                                 opciones = opc_rl,
                                 generate.rl.panel,
                                 coefficients.rl.panel,
                                 prediccion.rl.panel,
                                 disp.rl.panel,
                                 rl.general.index.panel))
  
  tagList(
    page.rl
  )
}
    
#' l_regression Server Function
#'
#' @noRd 
mod_l_regression_server <- function(input, output, session, updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  
  nombreModelo <- "modelo.rl"
  df.rl <- NULL
  r2    <- NULL
  
  observeEvent(c(updateData$datos), {
    modelos2$rl = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
  #Update model 
  output$txtRl <- renderPrint({
    input$runRl
    tryCatch({
      codigo.linear.regression()
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba      <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
      })
      form <- formula(paste0(variable.predecir,"~."))
      #Model generate
      modelo.rl <- lm(formula = form, data = datos.aprendizaje)
      modelo.rl$call$formula <- form
      #Coefficients
      model.information <- rl_coeff(modelo.rl)
      df.rl <<- model.information$df.rl
      r2    <<- model.information$r2
      
      #Prediccion
      prediccion.rl <- predict(modelo.rl, datos.prueba)
      
      #Indices
      indices.rl <- general_indices(datos.prueba[,variable.predecir], prediccion.rl)
      
      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$rl[[nombreModelo]] <- list(modelo     = modelo.rl, 
                                                 prediccion = prediccion.rl, 
                                                 indices    = indices.rl, 
                                                 id = NULL))
      #Cambiamos la forma en que va aparecer el call
      
      isolate({
        modelos$rl[[nombreModelo]] <- list(modelo     = modelo.rl, 
                                            prediccion = prediccion.rl, 
                                            indices    = indices.rl, 
                                            id = NULL)
        modelos2$rl$n <- modelos2$rl$n + 1
        modelos2$rl$mcs[modelos2$rl$n] <- list(indices.rl)
        if(modelos2$rl$n > 9)
          modelos2$rl$n <- 0
        
      })
      
      if(!is.null(modelos$rl[[nombreModelo]])){
        modelo.rl <- modelos$rl[[nombreModelo]]$modelo
        #Cambiamos la forma en que va aparecer el call
        modelo.rl$call$formula <- form
        
        print(summary(modelo.rl))
      }
      else{NULL}
    }, error = function(e){
      isolate(modelos$rl[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (RL-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  #Update Coefficients tab
  output$rlCoefTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(df.rl) && !is.null(modelos$rl[[nombreModelo]])){
        dttable.custom(data.frame(row.names = row.names(df.rl), coeff = df.rl[,1]), 
                       decimals     = updateData$decimals,
                       translatable = TRUE, 
                       language     = isolate(codedioma$idioma))
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RL-02) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)

  
  
  # Update prediction tab
  output$rlPrediTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$rl[[nombreModelo]])){
        prediccion.rl <- modelos$rl[[nombreModelo]]$prediccion
        
        isolate({
          datos.prueba <- updateData$datos.prueba
          real.val     <- datos.prueba[updateData$variable.predecir]
        })
        
        tb_predic(real.val, prediccion.rl, updateData$decimals,codedioma$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (RL-03) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  
  # Update dispersion tab
  output$plot_rl_disp <- renderEcharts4r({
    tryCatch({
      
      if(!is.null(modelos$rl[[nombreModelo]])){
        prediccion.rl <- modelos$rl[[nombreModelo]]$prediccion
        
        isolate({
          datos.prueba      <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
        })
        
        codigo <- disp_models("prediccion.rl", tr("rl",codedioma$idioma), variable.predecir)
        cod    <- paste0("### docdisp\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        titulos <- c(
          tr("predvsreal", codedioma$idioma),
          tr("realValue", codedioma$idioma),
          tr("pred", codedioma$idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir], 
                             prediccion.rl,tr("rl",codedioma$idioma),
                             titulos)
      }
      else{NULL}
    },
    error = function(e) {
      showNotification(paste0("Error (RL-04): ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update Indices tab
  output$indexdfrl <- renderTable({
    tryCatch({
      if(!is.null(modelos$rl[[nombreModelo]])){
        idioma     <- codedioma$idioma
        indices.rl <- modelos$rl[[nombreModelo]]$indices
        df         <- cbind(as.data.frame(indices.rl), r2)
        df         <- df[,c(1,2,3,5,4)]
        colnames(df) <- c(tr("RMSE",idioma), tr("MAE",idioma),
                          tr("ER",idioma),   tr("R2",idioma),
                          tr("correlacion", idioma))

        df <- round(df,updateData$decimals)
        #Esto es necesario debido a problema con la cantidad de decimales
        #con la función renderTable
        df[,] <- sapply(df[,], as.character)
        df
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RL-05) : ",e), duration = 10, type = "error")
      NULL
    })
  }, striped = TRUE, bordered = TRUE,
  spacing = 'l', width = '100%',align = 'c')
  
  
  output$indexdfrl2 <- renderTable({
    tryCatch({
      if(!is.null(modelos$rl[[nombreModelo]])){
        idioma   <- codedioma$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(summary_indices(updateData$datos.prueba[,updateData$variable.predecir]),
                              decimals, 
                              idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (RL-06) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%',align = 'c')
  
  codigo.linear.regression <- function() {
    
    isolate({
      variable.predecir <- updateData$variable.predecir
    })
    
    #Model generate
    codigo <- codeRl(variable.predecir)
    cod    <- paste0("### reglin\n", codigo)
    
    #Coefficients
    codigo <- codeRlCoef()
    cod    <- paste0(cod, codigo)
    #Prediccion
    codigo <- codigo.prediccion("rl")
    cod    <- paste0(cod, codigo)
    #Indices
    codigo <- codigo.IG(model.name = "rl", variable.pr = variable.predecir)
    cod    <- paste0(cod, codigo)
    
    isolate(codedioma$code <- append(codedioma$code, cod))
  }
  
}
    
## To be copied in the UI
# mod_l_regression_ui("l_regression_ui_1")
    
## To be copied in the server
# callModule(mod_l_regression_server, "l_regression_ui_1")
 
