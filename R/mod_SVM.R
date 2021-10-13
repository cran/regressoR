#' SVM UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_SVM_ui <- function(id){
  
  ns <- NS(id)
  
  svm.options <- list(options.run(ns("runSvm")), tags$hr(style = "margin-top: 0px;"),
                      conditionalPanel("input.BoxSvm != 'tabSvmPlot'",
                                       fluidRow(column(width = 5,radioSwitch(id = ns("switch_scale_svm"), label = "escal",
                                                                             names = c("si", "no"))),
                                                column(width=5, selectInput(inputId = ns("kernel.svm"), label = labelInput("selkernel"), selected = "radial",
                                                                            choices =  c("linear", "polynomial", "radial", "sigmoid")))), ns = ns))
  
  svm.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                          codigo.monokai(ns("fieldCodeSvm"), height = "7vh"))
  
  
  svm.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                   conditionalPanel("input.BoxSvm == 'tabSvmDisp'",
                                    codigo.monokai(ns("fieldCodeSvmDisp"), height = "7vh"), ns = ns),
                   conditionalPanel("input.BoxSvm == 'tabSvmPred'",
                                    codigo.monokai(ns("fieldCodeSvmPred"), height = "7vh"), ns = ns),
                   conditionalPanel("input.BoxSvm == 'tabSvmIndex'",
                                    codigo.monokai(ns("fieldCodeSvmIG"), height = "7vh"), ns = ns))
  
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("cog"), icon("code")), widths = c(50,100), heights = c(70,70),
                                       tabs.content = list(svm.options,svm.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(70),
                                         tabs.content = list(svm.code))
  
  tabs.options <- list(conditionalPanel("input.BoxSvm == 'tabSvmModelo'",tabs.options.generate,ns = ns),
                       conditionalPanel("input.BoxSvm != 'tabSvmModelo'",tabs.options.Nogenerate,ns = ns))
  
  generate.svm.panel <- tabPanel(title = labelInput("generatem"), value = "tabSvmModelo",
                                 withLoader(verbatimTextOutput(ns("txtSvm")),type = "html", loader = "loader4"))
  
  disp.svm.panel <- tabPanel(title = labelInput("dispersion"), value = "tabSvmDisp",
                             withLoader(echarts4rOutput(ns('plot_svm_disp'),height = "75vh"),type = "html", loader = "loader4"))
  
  prediction.svm.panel <- tabPanel(title = labelInput("predm"), value = "tabSvmPred",
                                   withLoader(DT::dataTableOutput(ns("svmPrediTable")),type = "html", loader = "loader4"))
  
  general.index.svm.panel <- tabPanel(title = labelInput("indices"), value = "tabSvmIndex",
                                      br(),
                                      fluidRow(withLoader(tableOutput(ns('indexdfsvm')),type = "html", loader = "loader4")),
                                      br(),
                                      fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                      br(),
                                      fluidRow(withLoader(tableOutput(ns('indexdfsvm2')),type = "html", loader = "loader4")))
  
  page.svm <- tabItem(tabName = "svm",
                      tabBoxPrmdt(id = ns("BoxSvm"), opciones = tabs.options,
                             generate.svm.panel,
                             prediction.svm.panel,
                             disp.svm.panel,
                             general.index.svm.panel))
  
  
  tagList(
    page.svm
  )
}

#' SVM Server Function
#'
#' @noRd 
mod_SVM_server <- function(input, output, session,updateData, modelos){
  ns <- session$ns
  
  nombreBase <- "modelo.svm."
  nombreModelo <- "modelo.svm."
  
  return.svm.default.values <- function(){
    updateRadioSwitch(session,"switch_scale_svm","TRUE")
    updateSelectInput(session,"kernel.svm",selected = "radial")
  }
  
  observeEvent(updateData$datos.aprendizaje,{
    return.svm.default.values()
  })
  
  # When the knn model is generated
  observeEvent(input$runSvm, {
    if (validate_data(updateData, idioma = updateData$idioma)) { # Si se tiene los datos entonces :
      svm_full()
    }
  })
  
  # Execute model, prediction and indices
  svm_full <- function() {
    tryCatch({
      shinyjs::runjs(code = "generating_model = true")
      
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        scale <- as.logical(input$switch_scale_svm)
        kernel <- input$kernel.svm
      })
      
      nombreModelo <<- paste0(nombreBase, kernel)
      
      #Model generate
      modelo.svm <- svm_model(datos.aprendizaje,variable.predecir, scale, kernel)
      updateAceEditor(session, "fieldCodeSvm", value = codeSvm(variable.predecir,scale, kernel))
      
      #Prediccion
      prediccion.svm <- svm_prediction(modelo.svm, datos.prueba)
      updateAceEditor(session, "fieldCodeSvmPred", value = codeSvmPred(nombreModelo))
      
      #Indices
      indices.svm <- general_indices(datos.prueba[,variable.predecir], prediccion.svm)
      updateAceEditor(session, "fieldCodeSvmIG", value = codeSvmIG(variable.predecir))
      
      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$svm[[nombreModelo]] <- list(modelo = modelo.svm, prediccion = prediccion.svm, indices = indices.svm, 
                                                  id = kernel))
    }, error = function(e){
      isolate(modelos$svm[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (SVM-00) : ",e), duration = 10, type = "error")
    },
    finally = {shinyjs::runjs(code = "generating_model = false")})
  }
  
  
  #Update model tab
  output$txtSvm <- renderPrint({
    tryCatch({
      if(!is.null(modelos$svm[[nombreModelo]])){
        modelo.svm <- modelos$svm[[nombreModelo]]$modelo
        print(modelo.svm)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (SVM-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  # Update prediction tab
  output$svmPrediTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$svm[[nombreModelo]])){
        prediccion.svm <- modelos$svm[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          real.val <- datos.prueba[updateData$variable.predecir]
        })
        tb_predic(real.val, prediccion.svm, updateData$decimals, updateData$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (SVM-02) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  
  # Update Dispersion Tab
  output$plot_svm_disp <- renderEcharts4r({
    
    tryCatch({
      if(!is.null(modelos$svm[[nombreModelo]])){
        prediccion.svm <- modelos$svm[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
          kernel <- input$kernel.svm
        })
        idioma <- updateData$idioma
        
        codigo <- disp_models(nombreModelo, paste0(tr("svm", idioma),"-",kernel), variable.predecir)
        updateAceEditor(session, "fieldCodeSvmDisp", value = codigo)
        
        titulos <- c(
          tr("predvsreal", idioma),
          tr("realValue", idioma),
          tr("pred", idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion.svm,
                             paste0(tr("svm", idioma),"-",kernel),titulos)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (SVM-03) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  #Update Indices tab
  output$indexdfsvm <- renderTable({
    tryCatch({
      if(!is.null(modelos$svm[[nombreModelo]])){
        idioma <- updateData$idioma
        indices.svm <- modelos$svm[[nombreModelo]]$indices
        tabla.indicesPrecision(indices.svm, updateData$decimals, idioma)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (SVM-04) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
  
  
  output$indexdfsvm2 <- renderTable({
    tryCatch({
      if(!is.null(modelos$svm[[nombreModelo]])& !is.null(updateData$summary.var.pred)){
        idioma <- updateData$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(updateData$summary.var.pred, decimals, idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (SVM-05) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
}

## To be copied in the UI
# mod_SVM_ui("SVM_ui_1")

## To be copied in the server
# callModule(mod_SVM_server, "SVM_ui_1")

