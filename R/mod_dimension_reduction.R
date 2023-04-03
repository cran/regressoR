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
  
  tabs.options <- list(
    conditionalPanel("input.BoxRd == 'tabRdModelo'", tabsOptions(widths = c(100), heights = c(80),
        tabs.content = list(list(options.run(ns("runRd")), tags$hr(style = "margin-top: 0px;"),
                                 fluidRow(col_6(selectInput(inputId = ns("modo.rd"), label = labelInput("selectAlg"),selected = 0,
                                                             choices = list("ACP" = 0, "MCP" = 1))),
                                          col_6(radioSwitch(id = ns("switch_scale_rd"), label = "escal",names = c("si", "no")))),
                                 fluidRow(col_6(id = ns("colManualCom"), 
                                                 numericInput(ns("ncomp.rd"), labelInput("ncomp"),value = 2, min = 1, width = "100%")),
                                          col_6(radioSwitch(id = ns("permitir_ncomp"), label = "",
                                                                        names = c("manual", "automatico"), val.def = FALSE)))))),ns = ns))
  

  tagList(
    tabBoxPrmdt(id = ns("BoxRd"), opciones = tabs.options,
                tabPanel(title = labelInput("generatem"),value = "tabRdModelo",
                         withLoader(verbatimTextOutput(ns("txtRd")),
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("RMSE"),value = "tabRdRMSE",
                         withLoader(echarts4rOutput(ns('plot_rd_rmse'),height = "75vh"),
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("RdPred"), value = "tabRdPlotPred",
                         withLoader(echarts4rOutput(ns('plot_rd_pred'),height = "75vh"),
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("RdVarPred"), value = "tabRdPlotVarPred",
                         withLoader(echarts4rOutput(ns('plot_rd_var_pred'),height = "75vh"),
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("predm"), value = "tabRdPred",
                         withLoader(DT::dataTableOutput(ns("rdPrediTable")),
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("dispersion"), value = "tabRdDisp",
                         withLoader(echarts4rOutput(ns('plot_rd_disp'),height = "75vh"),
                                    type = "html", loader = "loader4")),
                tabPanel(title = labelInput("indices"), value = "tabRdIndex",
                         br(),
                         div(withLoader(tableOutput(ns('indexdfrd')),type = "html", loader = "loader4")),
                         br(),
                         div(col_12(align="center", tags$h3(labelInput("resumenVarPre")))),
                         br(),
                         div(withLoader(tableOutput(ns('indexdfrd2')),type = "html", loader = "loader4"))))
  )
}

#' dimension_reduction Server Function
#'
#' @noRd 
mod_dimension_reduction_server <- function(input, output, session,updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  
  nombreBase <- "modelo.rd."
  nombreModelo <- "modelo.rd."
  
  ncomp <- NULL
  
  observeEvent(c(updateData$datos), {
    modelos2$rd = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
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
  
  
  
  # Habilitada o deshabilitada el número de componenetes 
  observeEvent(input$permitir_ncomp, {
    if (as.logical(input$permitir_ncomp)) {
      shinyjs::enable("ncomp.rd")
    } else {
      shinyjs::disable("ncomp.rd")
    }
  })
  
  
  #Update model tab
  output$txtRd <- renderPrint({
    input$runRd
    tryCatch({
      codigo.rd()
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba      <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        scale   <- as.logical(input$switch_scale_rd)
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

      #Prediccion
      prediccion.rd <- predict(modelo.rd, datos.prueba, ncomp = ncomp)

      #Indices
      indices.rd <- general_indices(datos.prueba[,variable.predecir], prediccion.rd)

      #isolamos para que no entre en un ciclo en el primer renderPrint

      isolate({
        modelos$rd[[nombreModelo]] <- list(modelo = modelo.rd, prediccion = prediccion.rd, indices = indices.rd, 
                                            id = rd_type(modo.rd))
        modelos2$rd$n <- modelos2$rd$n + 1
        modelos2$rd$mcs[modelos2$rd$n] <- list(indices.rd)
        if(modelos2$rd$n > 9)
          modelos2$rd$n <- 0
        
      })
      
      if(!is.null(modelos$rd[[nombreModelo]])){
        modelo.rd <- modelos$rd[[nombreModelo]]$modelo
        print(modelo.rd)
      }
      else{NULL}
      
    }, error = function(e){
      isolate(modelos$rd[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (RD-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  output$plot_rd_rmse <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$rd[[nombreModelo]])){
        modelo.rd <- modelos$rd[[nombreModelo]]$modelo
        idioma <- codedioma$idioma
        
        # Actualizar el código en el AceEditor
        codigo <- paste0("plot_RMSE(",nombreModelo, ", ncomp = ", ncomp ,")")
        cod    <- paste0("### Rmse\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
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
        idioma <- codedioma$idioma
        
        # Se actualiza el codigo
        codigo <- paste0("plot_pred_rd(",nombreModelo, ", ncomp = ", ncomp ,")")
        cod    <- paste0("### RdPred\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
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
        idioma <- codedioma$idioma
        
        # Se actualiza el codigo del plot
        codigo <- paste0("plot_var_pred_rd(",nombreModelo, ", ncomp = ", ncomp ,")")
        cod    <- paste0("### RdVarPred\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
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
        tb_predic(real.val, prediccion.rd, updateData$decimals, codedioma$idioma)
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
        idioma <- codedioma$idioma
        
        model.name <- paste0(tr("rd", idioma), "-", rd_type(modo.rd))
        codigo <- disp_models(nombreModelo, model.name, variable.predecir)
        cod    <- paste0("### docdisp\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
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
        idioma <- codedioma$idioma
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
      if(!is.null(modelos$rd[[nombreModelo]])){
        idioma <- codedioma$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(summary_indices(updateData$datos.prueba[,updateData$variable.predecir]),
                              decimals, 
                              idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (RD-10) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
  
  # Execute model, prediction and indices
  codigo.rd <- function(){
    
    tryCatch({
      
      isolate({
        variable.predecir <- updateData$variable.predecir
        scale   <- as.logical(input$switch_scale_rd)
        modo.rd <- input$modo.rd
        ncomp   <- input$ncomp.rd
      })
      
      #Model generate
      codigo <- codeRd(variable.predecir,modo.rd, scale)
      cod    <- paste0("### RD\n", codigo)
      
      #Prediccion
      codigo <- codigo.prediccion("rd")
      cod    <- paste0(cod, codigo)
      #Indices
      codigo <- codigo.IG(model.name = "rd", variable.pr = variable.predecir)
      cod    <- paste0(cod, codigo)
      
      isolate(codedioma$code <- append(codedioma$code, cod))
      
    }, error = function(e){
      showNotification(paste0("Error (RD-00) : ",e), duration = 10, type = "error")
    })
  }
  
}

## To be copied in the UI
# mod_dimension_reduction_ui("dimension_reduction_ui_1")

## To be copied in the server
# callModule(mod_dimension_reduction_server, "dimension_reduction_ui_1")

