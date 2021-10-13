#' dimension_reduction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dimension_reduction_ui <- function(id){
  
  ns <- NS(id)
  
  rd.options  <- list(options.run(ns("runRd")), tags$hr(style = "margin-top: 0px;"),
                      fluidRow(column(selectInput(inputId = ns("modo.rd"), label = labelInput("selectAlg"),selected = 0,
                                                  choices = list("ACP" = 0, "MCP" = 1)),width = 5),
                               column(width=5,radioSwitch(id = ns("switch_scale_rd"), label = "escal",names = c("si", "no")))),
                      fluidRow(column(id = ns("colManualCom"),width = 5, 
                                      numericInput(ns("ncomp.rd"), labelInput("ncomp"),value = 2, min = 1, width = "100%")),
                               column(width = 5, radioSwitch(id = ns("permitir_ncomp"), label = "",
                                                             names = c("manual", "automatico"), val.def = FALSE))))
  
  
  rd.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                         codigo.monokai(ns("fieldCodeRd"), height = "8vh"))
  
  
  rd.code   <- list(fluidRow(column(width = 9,h3(labelInput("codigo")))),
                    hr(style = "margin-top: 0px;"),
                    conditionalPanel("input.BoxRd == 'tabRdRMSE'",
                                     codigo.monokai(ns("fieldCodeRdRMSE"), height = "7vh"), ns = ns),
                    conditionalPanel("input.BoxRd == 'tabRdPlotPred'",
                                     codigo.monokai(ns("fieldCodeRdPlotPred"), height = "7vh"), ns = ns),
                    conditionalPanel("input.BoxRd == 'tabRdPlotVarPred'",
                                     codigo.monokai(ns("fieldCodeRdPlotVarPred"), height = "7vh"), ns = ns),
                    conditionalPanel("input.BoxRd == 'tabRdPred'",
                                     codigo.monokai(ns("fieldCodeRdPred"), height = "7vh"), ns = ns),
                    conditionalPanel("input.BoxRd == 'tabRdDisp'",
                                     codigo.monokai(ns("fieldCodeRdDisp"), height = "7vh"), ns = ns),
                    conditionalPanel("input.BoxRd == 'tabRdIndex'",
                                     codigo.monokai(ns("fieldCodeRdIG"), height = "7vh"), ns = ns))
  
  
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("cog"), icon("code")), widths = c(50,100), heights = c(80,70),
                                       tabs.content = list(rd.options,rd.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(70),
                                         tabs.content = list(rd.code))
  
  tabs.options <- list(conditionalPanel("input.BoxRd == 'tabRdModelo'",tabs.options.generate,ns = ns),
                       conditionalPanel("input.BoxRd != 'tabRdModelo'",tabs.options.Nogenerate,ns = ns))
  
  
  generate.rd.panel <- tabPanel(title = labelInput("generatem"),value = "tabRdModelo",
                                withLoader(verbatimTextOutput(ns("txtRd")),type = "html", loader = "loader4"))
  
  rmse.rd.panel <- tabPanel(title = labelInput("RMSE"),value = "tabRdRMSE",
                            withLoader(echarts4rOutput(ns('plot_rd_rmse'),height = "75vh"),type = "html", loader = "loader4"))
  
  plot.pred.rd.panel <- tabPanel(title = labelInput("RdPred"), value = "tabRdPlotPred",
                                 withLoader(echarts4rOutput(ns('plot_rd_pred'),height = "75vh"),type = "html", loader = "loader4"))
  
  panel.plot.var.pred.rd <- tabPanel(title = labelInput("RdVarPred"), value = "tabRdPlotVarPred",
                                     withLoader(echarts4rOutput(ns('plot_rd_var_pred'),height = "75vh"),type = "html", loader = "loader4"))
  
  prediction.rd.panel <- tabPanel(title = labelInput("predm"), value = "tabRdPred",
                                  withLoader(DT::dataTableOutput(ns("rdPrediTable")),type = "html", loader = "loader4"))
  
  disp.rd.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRdDisp",
                            withLoader(echarts4rOutput(ns('plot_rd_disp'),height = "75vh"),type = "html", loader = "loader4"))
  
  general.index.rd.panel <- tabPanel(title = labelInput("indices"), value = "tabRdIndex",
                                     br(),
                                     fluidRow(withLoader(tableOutput(ns('indexdfrd')),type = "html", loader = "loader4")),
                                     br(),
                                     fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                     br(),
                                     fluidRow(withLoader(tableOutput(ns('indexdfrd2')),type = "html", loader = "loader4")))
  
  page.rd <- tabItem(tabName = "rd",
                     tabBoxPrmdt(id = ns("BoxRd"), opciones = tabs.options,
                            generate.rd.panel,
                            rmse.rd.panel,
                            plot.pred.rd.panel,
                            panel.plot.var.pred.rd,
                            prediction.rd.panel,
                            disp.rd.panel,
                            general.index.rd.panel))
  
  
  tagList(
    page.rd
  )
}

#' dimension_reduction Server Function
#'
#' @noRd 
mod_dimension_reduction_server <- function(input, output, session,updateData, modelos){
  ns <- session$ns
  
  nombreBase <- "modelo.rd."
  nombreModelo <- "modelo.rd."
  
  ncomp <- NULL
  
  return.rd.default.values <- function(){
    updateSelectInput(session,"modo.rd",selected = 0)
    updateRadioSwitch(session,"switch_scale_rd","TRUE")
    updateNumericInput(session,"ncomp.rd", value = 2)
    updateRadioSwitch(session,"permitir_ncomp","FALSE")
    
    ncomp <<- NULL
    nombreModelo <- "modelo.rd."
  }
  
  
  observeEvent(updateData$datos.aprendizaje,{
    return.rd.default.values()
  })
  
  observeEvent(input$runRd, {
    if (validate_data(updateData, idioma = updateData$idioma)) { # Si se tiene los datos entonces :
      rd_full()
    }
  })
  
  
  # Habilitada o deshabilitada el número de componenetes 
  observeEvent(input$permitir_ncomp, {
    if (as.logical(input$permitir_ncomp)) {
      shinyjs::enable("ncomp.rd")
    } else {
      shinyjs::disable("ncomp.rd")
    }
  })
  
  
  # Execute model, prediction and indices
  rd_full <- function(){
    tryCatch({
      shinyjs::runjs(code = "generating_model = true")
      
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        scale <- as.logical(input$switch_scale_rd)
        modo.rd <- input$modo.rd
        
        ncomp <<- NULL
        if (as.logical(input$permitir_ncomp) && !is.na(input$ncomp.rd)) {
          if(input$ncomp.rd >= 1 && input$ncomp.rd <= ncol(datos.aprendizaje)){
            ncomp <<- input$ncomp.rd
          }
        }
      })
      
      nombreModelo <<- paste0(nombreBase, rd_type(modo.rd))
      
      #Model generate
      modelo.rd <- rd_model(datos.aprendizaje,variable.predecir, modo.rd, scale)
      if(is.null(ncomp)){
        ncomp <<- modelo.rd$optimal.n.comp
        updateNumericInput(session,"ncomp.rd", value = ncomp)
      }
      updateAceEditor(session, "fieldCodeRd", value = codeRd(variable.predecir,modo.rd, scale))
      
      #Prediccion
      prediccion.rd <- rd_prediction(modelo.rd, datos.prueba, ncomp)
      updateAceEditor(session, "fieldCodeRdPred", value = codeRdPred(nombreModelo, ncomp))
      
      #Indices
      indices.rd <- general_indices(datos.prueba[,variable.predecir], prediccion.rd)
      updateAceEditor(session, "fieldCodeRdIG", value = codeRdIG(variable.predecir))
      
      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$rd[[nombreModelo]] <- list(modelo = modelo.rd, prediccion = prediccion.rd, indices = indices.rd, 
                                                 id = rd_type(modo.rd)))
    }, error = function(e){
      isolate(modelos$rd[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (RD-00) : ",e), duration = 10, type = "error")
    },
    finally = {shinyjs::runjs(code = "generating_model = false")})
  }
  
  
  #Update model tab
  output$txtRd <- renderPrint({
    tryCatch({
      if(!is.null(modelos$rd[[nombreModelo]])){
        modelo.rd <- modelos$rd[[nombreModelo]]$modelo
        print(summary(modelo.rd))
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RD-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  output$plot_rd_rmse <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rd[[nombreModelo]])){
        modelo.rd <- modelos$rd[[nombreModelo]]$modelo
        idioma <- updateData$idioma
        
        # Actualizar el código en el AceEditor
        codigo <- paste0("plot_RMSE(",nombreModelo, ", ncomp = ", ncomp ,")")
        updateAceEditor(session, "fieldCodeRdRMSE", value = codigo)
        
        titulos <- c(
          tr("RMSEComp", idioma),
          tr("ncomp", idioma),
          tr("RMSE", idioma)
        )
        
        plot_RMSE(modelo.rd, ncomp, titulos)
      }else{NULL}
    }, 
    
    error = function(e){
      showNotification(paste0("Error (RD-04) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  output$plot_rd_pred <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rd[[nombreModelo]])){
        
        modelo.rd <- modelos$rd[[nombreModelo]]$modelo
        idioma <- updateData$idioma
        
        # Se actualiza el codigo
        codigo <- paste0("plot_pred_rd(",nombreModelo, ", ncomp = ", ncomp ,")")
        updateAceEditor(session, "fieldCodeRdPlotPred", value = codigo)
        
        titulos <- c(
          tr("RdPred", idioma),
          tr("ncomp", idioma),
          tr("VarExp", idioma)
        )
        
        plot_pred_rd(modelo.rd, ncomp,titulos)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (RD-05) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  output$plot_rd_var_pred <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rd[[nombreModelo]])){
        modelo.rd <- modelos$rd[[nombreModelo]]$modelo
        idioma <- updateData$idioma
        
        # Se actualiza el codigo del plot
        codigo <- paste0("plot_var_pred_rd(",nombreModelo, ", ncomp = ", ncomp ,")")
        updateAceEditor(session, "fieldCodeRdPlotVarPred", value = codigo)
        
        titulos <- c(
          tr("RdVarPred", idioma),
          tr("ncomp", idioma),
          tr("VarExp", idioma)
        )
        
        plot_var_pred_rd(modelo.rd, ncomp,titulos)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RD-06) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  # Update prediction tab
  output$rdPrediTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$rd[[nombreModelo]])){
        prediccion.rd <- modelos$rd[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          real.val <- datos.prueba[updateData$variable.predecir]
        })
        tb_predic(real.val, prediccion.rd, updateData$decimals, updateData$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (RD-07) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  
  
  # Update Dispersion Tab
  output$plot_rd_disp <- renderEcharts4r({
    tryCatch({
      
      if(!is.null(modelos$rd[[nombreModelo]])){
        
        prediccion.rd <- modelos$rd[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
          modo.rd <- input$modo.rd
        })
        idioma <- updateData$idioma
        
        model.name <- paste0(tr("rd", idioma), "-", rd_type(modo.rd))
        codigo <- disp_models(nombreModelo, model.name, variable.predecir)
        updateAceEditor(session, "fieldCodeRdDisp", value = codigo)
        
        titulos <- c(
          tr("predvsreal", idioma),
          tr("realValue", idioma),
          tr("pred", idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion.rd,
                             model.name,titulos)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RD-08) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update Indices tab
  output$indexdfrd <- renderTable({
    tryCatch({
      if(!is.null(modelos$rd[[nombreModelo]])){
        idioma <- updateData$idioma
        indices.rd <- modelos$rd[[nombreModelo]]$indices
        tabla.indicesPrecision(indices.rd, updateData$decimals, idioma)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (RD-09) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
  
  
  output$indexdfrd2 <- renderTable({
    tryCatch({
      if(!is.null(modelos$rd[[nombreModelo]])& !is.null(updateData$summary.var.pred)){
        idioma <- updateData$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(updateData$summary.var.pred, decimals, idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (RD-10) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
}

## To be copied in the UI
# mod_dimension_reduction_ui("dimension_reduction_ui_1")

## To be copied in the server
# callModule(mod_dimension_reduction_server, "dimension_reduction_ui_1")

