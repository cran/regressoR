#' regression_trees UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_regression_trees_ui <- function(id){
  
  ns <- NS(id)
  
  dt.options <- list(options.run(ns("runDt")), tags$hr(style = "margin-top: 0px;"),
                     fluidRow(column(numericInput(ns("minsplit.dt"), labelInput("minsplit"), 20, width = "100%",min = 1), width = 5),
                              column(numericInput(ns("maxdepth.dt"), labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1),width = 5)))
  
  
  tabs.options.generate <- tabsOptions( widths = c(100), heights = c(70),
                                       tabs.content = list(dt.options))
  
  tabs.options <- list(conditionalPanel("input.BoxDt == 'tabDtModelo'",tabs.options.generate,ns = ns))
  
  generate.dt.panel <- tabPanel(title = labelInput("generatem"), value = "tabDtModelo",
                                withLoader(verbatimTextOutput(ns("txtDt")),type = "html", loader = "loader4"))
  
  plot.dt <- tabPanel(title = labelInput("garbol"), value = "tabDtPlot",
                      withLoader(plotOutput(ns('plot_dt'), height = "75vh"),type = "html", loader = "loader4"))
  
  prediction.dt.panel <- tabPanel(title = labelInput("predm"), value = "tabDtPred",
                                  withLoader(DT::dataTableOutput(ns("dtPrediTable")),type = "html", loader = "loader4"))
  
  disp.dt.panel <- tabPanel(title = labelInput("dispersion"), value = "tabDtDisp",
                            withLoader(echarts4rOutput(ns('plot_dt_disp'),height = "75vh"),type = "html", loader = "loader4"))
  
  general.index.dt.panel <- tabPanel(title = labelInput("indices"),value = "tabDtIndex",
                                     br(),
                                     div(withLoader(tableOutput(ns('indexdfdt')),type = "html", loader = "loader4")),
                                     br(),
                                     div(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     div(withLoader(tableOutput(ns('indexdfdt2')),type = "html", loader = "loader4")))
  
  rules.dt.panel <- tabPanel(title = labelInput("reglas"),value = "tabDtReglas",
                             withLoader(verbatimTextOutput(ns("rulesDt")),type = "html", loader = "loader4"))
  
  page.dt <- tabItem(tabName = "dt",
                     tabBoxPrmdt(id = ns("BoxDt"), opciones = tabs.options,
                            generate.dt.panel,
                            plot.dt,
                            prediction.dt.panel,
                            disp.dt.panel,
                            general.index.dt.panel,
                            rules.dt.panel))
  
  tagList(
    page.dt
  )
}

#' regression_trees Server Function
#'
#' @noRd 
mod_regression_trees_server <- function(input, output, session,updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  
  nombreModelo <- "modelo.dt"
  
  return.dt.default.values <- function(){
    updateNumericInput(session,inputId = "minsplit.dt", value = 20)
    updateNumericInput(session,inputId = "maxdepth.dt", value = 15)
  }
  
  observeEvent(c(updateData$datos), {
    modelos2$dt = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
  observeEvent(updateData$datos.aprendizaje,{
    return.dt.default.values()
  })
  
  
  #Update model tab
  output$txtDt <- renderPrint({
    input$runDt
    tryCatch({
      codigo.dt()
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba      <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        ms <- input$minsplit.dt
        md <- input$maxdepth.dt
      })
      
      minsplit <- ifelse(!is.numeric(ms), 20, ms)
      maxdepth <- ifelse(!is.numeric(md), 15, md)
      
      var    <- as.formula(paste0(variable.predecir, "~."))
      
      # Model Generate
      modelo.dt <- train.rpart(var, data = datos.aprendizaje,
                               control = rpart.control(minsplit = minsplit, maxdepth = maxdepth), model = TRUE)

      #Prediccion
      prediccion.dt <- predict(modelo.dt,datos.prueba)$prediction

      #Indices
      indices.dt <- general_indices(datos.prueba[,variable.predecir], prediccion.dt)

      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$dt[[nombreModelo]] <- list(modelo = modelo.dt, prediccion = prediccion.dt, indices = indices.dt, 
                                                 id = NULL))


      isolate({
        modelos$dt[[nombreModelo]] <- list(modelo = modelo.dt, prediccion = prediccion.dt, indices = indices.dt, 
                                            id = NULL)
        modelos2$dt$n <- modelos2$dt$n + 1
        modelos2$dt$mcs[modelos2$dt$n] <- list(indices.dt)
        if(modelos2$dt$n > 9)
          modelos2$dt$n <- 0
        
      })
      
      if(!is.null(modelos$dt[[nombreModelo]])){
        modelo.dt <- modelos$dt[[nombreModelo]]$modelo
        #Cambiamos la forma en que va aparecer el call
        modelo.dt$call$formula   <- var
        modelo.dt$call$control$minsplit <- minsplit
        modelo.dt$call$control$maxdepth <- maxdepth
        print(summary(modelo.dt))
        
      }
      else{NULL}
    }, error = function(e){
      isolate(modelos$dt[[nombreModelo]] <- NULL)
      
      showNotification(paste0("Error (DT-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  # Shows the graph of the tree
  output$plot_dt <- renderPlot({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        codigo <- codeDtPlot(nombreModelo)
        cod    <- paste0("### garbol\n",codigo)
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        modelo.dt <- modelos$dt[[nombreModelo]]$modelo
        dt_plot(modelo.dt)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (DT-02) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  # Update prediction tab
  output$dtPrediTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        prediccion.dt <- modelos$dt[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          real.val <- datos.prueba[updateData$variable.predecir]
        })
        tb_predic(real.val, prediccion.dt, updateData$decimals, codedioma$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (DT-03) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)

  
  # Update dispersion tab
  output$plot_dt_disp <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        prediccion.dt <- modelos$dt[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
        })
        
        codigo <- disp_models("prediccion.dt", tr("dt",codedioma$idioma), variable.predecir)
        cod    <- paste0("### docdisp\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        
        titulos <- c(
          tr("predvsreal", codedioma$idioma),
          tr("realValue", codedioma$idioma),
          tr("pred", codedioma$idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion.dt,tr("dt", codedioma$idioma),titulos)
      }
      else{NULL}
    },
    error = function(e) {
      showNotification(paste0("Error (DT-04) : ", e), duration = 15, type = "error")
      NULL
    })
  })
  
  
  # Update Rules tab
  output$rulesDt <- renderPrint({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        isolate({
          variable.predecir <- updateData$variable.predecir
        })
        
        modelo.dt <- modelos$dt[[nombreModelo]]$modelo
        
        codigo <- paste0("rpart.rules(model.dt, cover = TRUE,nn = TRUE , style = 'tall', digits=3,
                            response.name ='",paste0("Rule Number - ", variable.predecir),"')")
        cod    <- paste0("### reglas\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
        rpart.plot::rpart.rules(modelo.dt, cover = TRUE,nn = TRUE ,roundint=FALSE, style = "tall", digits=3, 
                                response.name = paste0("Rule Number - ", variable.predecir))
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (DT-05) : ", e), duration = 15, type = "error")
      NULL
    })
  })
  
  
  #Update Indices tab
  output$indexdfdt <- renderTable({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        idioma <- codedioma$idioma
        indices.dt <- modelos$dt[[nombreModelo]]$indices
        tabla.indicesPrecision(indices.dt, updateData$decimals, idioma)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (DT-06) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
  
  
  output$indexdfdt2 <- renderTable({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        idioma <- codedioma$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(summary_indices(updateData$datos.prueba[,updateData$variable.predecir]),
                              decimals, 
                              idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (DT-07) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
  
  codigo.dt<- function() {
    isolate({
      variable.predecir <- updateData$variable.predecir
      ms <- input$minsplit.dt
      md <- input$maxdepth.dt
    })
    
    minsplit <- ifelse(!is.numeric(ms), 20, ms)
    maxdepth <- ifelse(!is.numeric(md), 15, md)
    
    #Model generate
    codigo <- codeDt(variable.predecir,minsplit,maxdepth)
    cod    <- paste0("### DT\n", codigo)
    
    #Prediccion
    codigo <- codigo.prediccion("dt")
    cod    <- paste0(cod, codigo)
    #Indices
    codigo <- codigo.IG(model.name = "dt", variable.pr = variable.predecir)
    cod    <- paste0(cod, codigo)
    
    isolate(codedioma$code <- append(codedioma$code, cod))
  }
  
}

## To be copied in the UI
# mod_regression_trees_ui("regression_trees_ui_1")

## To be copied in the server
# callModule(mod_regression_trees_server, "regression_trees_ui_1")

