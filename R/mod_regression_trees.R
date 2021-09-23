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
  
  dt.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                         codigo.monokai(ns("fieldCodeDt"), height = "7vh"))
  
  dt.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                  conditionalPanel("input.BoxDt == 'tabDtPlot'",
                                   codigo.monokai(ns("fieldCodeDtPlot"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxDt == 'tabDtPred'",
                                   codigo.monokai(ns("fieldCodeDtPred"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxDt == 'tabDtDisp'",
                                   codigo.monokai(ns("fieldCodeDtDisp"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxDt == 'tabDtIndex'",
                                   codigo.monokai(ns("fieldCodeDtIG"), height = "7vh"),ns = ns),
                  conditionalPanel("input.BoxDt == 'tabDtReglas'",
                                   codigo.monokai(ns("fieldCodeDtRule"), height = "7vh"),ns = ns))
  
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("gear"), icon("code")), widths = c(50,100), heights = c(70,70),
                                       tabs.content = list(dt.options,dt.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(70),
                                         tabs.content = list(dt.code))
  
  generate.dt.panel <- tabPanel(title = labelInput("generatem"), value = "tabDtModelo",
                                withLoader(verbatimTextOutput(ns("txtDt")),type = "html", loader = "loader4"))
  
  plot.dt <- tabPanel(title = labelInput("garbol"), value = "tabDtPlot",
                      withLoader(plotOutput(ns('plot_dt'), height = "75vh"),type = "html", loader = "loader4"))
  
  prediction.dt.panel <- tabPanel(title = labelInput("predm"), value = "tabDtPred",
                                  withLoader(DT::dataTableOutput(ns("dtPrediTable")),type = "html", loader = "loader4"))
  
  disp.dt.panel <- tabPanel(title = labelInput("dispersion"), value = "tabDtDisp",
                            echarts4rOutput(ns('plot_dt_disp'), height = "75vh"))
  
  general.index.dt.panel <- tabPanel(title = labelInput("indices"),value = "tabDtIndex",
                                     br(),
                                     fluidRow(withLoader(tableOutput(ns('indexdfdt')),type = "html", loader = "loader4")),
                                     br(),
                                     fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     fluidRow(withLoader(tableOutput(ns('indexdfdt2')),type = "html", loader = "loader4")))
  
  rules.dt.panel <- tabPanel(title = labelInput("reglas"),value = "tabDtReglas",
                             withLoader(verbatimTextOutput(ns("rulesDt")),type = "html", loader = "loader4"))
  
  page.dt <- tabItem(tabName = "dt",
                     tabBox(id = ns("BoxDt"), width = NULL, height ="80%",
                            generate.dt.panel,
                            plot.dt,
                            prediction.dt.panel,
                            disp.dt.panel,
                            general.index.dt.panel,
                            rules.dt.panel,
                            conditionalPanel("input.BoxDt == 'tabDtModelo'",tabs.options.generate,ns = ns),
                            conditionalPanel("input.BoxDt != 'tabDtModelo'",tabs.options.Nogenerate,ns = ns)))
  
  tagList(
    page.dt
  )
}

#' regression_trees Server Function
#'
#' @noRd 
mod_regression_trees_server <- function(input, output, session,updateData, modelos){
  ns <- session$ns
  
  nombreModelo <- "modelo.dt"
  
  return.dt.default.values <- function(){
    updateNumericInput(session,inputId = "minsplit.dt", value = 20)
    updateNumericInput(session,inputId = "maxdepth.dt", value = 15)
  }
  
  observeEvent(updateData$datos.aprendizaje,{
    return.dt.default.values()
  })
  
  
  #  When the dt model is generated
  observeEvent(input$runDt, {
    if (validate_data(updateData, idioma = updateData$idioma)) { # Si se tiene los datos entonces :
      dt_full()
    }
  })
  
  # Execute model, prediction and indices
  dt_full <- function() {
    tryCatch({
      shinyjs::runjs(code = "generating_model = true")
      
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        ms <- input$minsplit.dt
        md <- input$maxdepth.dt
      })
      
      minsplit <- ifelse(!is.numeric(ms), 20, ms)
      maxdepth <- ifelse(!is.numeric(md), 15, md)
      
      # Model Generate
      modelo.dt <- dt_model(datos.aprendizaje, variable.predecir,
                            minsplit = minsplit,
                            maxdepth = maxdepth)
      updateAceEditor(session, "fieldCodeDt", value = codeDt(variable.predecir,minsplit,maxdepth))
      
      #Prediccion
      prediccion.dt <- dt_prediction(modelo.dt,datos.prueba)
      updateAceEditor(session, "fieldCodeDtPred", value = codeDtPred(nombreModelo))
      
      #Indices
      indices.dt <- general_indices(datos.prueba[,variable.predecir], prediccion.dt)
      updateAceEditor(session, "fieldCodeDtIG", value = codeDtIG(variable.predecir))
      
      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$dt[[nombreModelo]] <- list(modelo = modelo.dt, prediccion = prediccion.dt, indices = indices.dt, 
                                                 id = NULL))
      
    }, error = function(e){
      isolate(modelos$dt[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (DT-00) : ",e), duration = 10, type = "error")
    },
    finally = {
      shinyjs::runjs(code = "generating_model = false")
    })
  }
  
  
  #Update model tab
  output$txtDt <- renderPrint({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        modelo.dt <- modelos$dt[[nombreModelo]]$modelo
        print(modelo.dt)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (DT-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  # Shows the graph of the tree
  output$plot_dt <- renderPlot({
    tryCatch({
      if(!is.null(modelos$dt[[nombreModelo]])){
        updateAceEditor(session, "fieldCodeDtPlot", value = codeDtPlot(nombreModelo))
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
        tb_predic(real.val, prediccion.dt, updateData$decimals, updateData$idioma)
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
        
        codigo <- disp_models("prediccion.dt", tr("dt",updateData$idioma), variable.predecir)
        updateAceEditor(session, "fieldCodeDtDisp", value = codigo)
        
        titulos <- c(
          tr("predvsreal", updateData$idioma),
          tr("realValue", updateData$idioma),
          tr("pred", updateData$idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion.dt,tr("dt", updateData$idioma),titulos)
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
        
        updateAceEditor(session, "fieldCodeDtRule", paste0("rpart.rules(model.dt, cover = TRUE,nn = TRUE , style = 'tall', digits=3,
                            response.name ='",paste0("Rule Number - ", variable.predecir),"')"))
        
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
        idioma <- updateData$idioma
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
      if(!is.null(modelos$dt[[nombreModelo]])& !is.null(updateData$summary.var.pred)){
        idioma <- updateData$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(updateData$summary.var.pred, decimals, idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (DT-07) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
}

## To be copied in the UI
# mod_regression_trees_ui("regression_trees_ui_1")

## To be copied in the server
# callModule(mod_regression_trees_server, "regression_trees_ui_1")

