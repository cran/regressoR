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

  
  tabs.options.generate <- tabsOptions(widths = c(100), heights = c(70),
                                       tabs.content = list(svm.options))
  
  
  tabs.options <- list(conditionalPanel("input.BoxSvm == 'tabSvmModelo'",tabs.options.generate,ns = ns))
  
  generate.svm.panel <- tabPanel(title = labelInput("generatem"), value = "tabSvmModelo",
                                 withLoader(verbatimTextOutput(ns("txtSvm")),type = "html", loader = "loader4"))
  
  disp.svm.panel <- tabPanel(title = labelInput("dispersion"), value = "tabSvmDisp",
                             withLoader(echarts4rOutput(ns('plot_svm_disp'),height = "75vh"),type = "html", loader = "loader4"))
  
  prediction.svm.panel <- tabPanel(title = labelInput("predm"), value = "tabSvmPred",
                                   withLoader(DT::dataTableOutput(ns("svmPrediTable")),type = "html", loader = "loader4"))
  
  general.index.svm.panel <- tabPanel(title = labelInput("indices"), value = "tabSvmIndex",
                                      br(),
                                      div(withLoader(tableOutput(ns('indexdfsvm')),type = "html", loader = "loader4")),
                                      br(),
                                      div(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                      br(),
                                      div(withLoader(tableOutput(ns('indexdfsvm2')),type = "html", loader = "loader4")))
  
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
mod_SVM_server <- function(input, output, session,updateData, modelos, codedioma, modelos2){
  ns <- session$ns
  
  nombreBase   <- "modelo.svm."
  nombreModelo <- "modelo.svm."
  
  return.svm.default.values <- function(){
    updateRadioSwitch(session,"switch_scale_svm","TRUE")
    updateSelectInput(session,"kernel.svm",selected = "radial")
  }
  
  observeEvent(c(updateData$datos), {
    modelos2$svm = list(n = 0, mcs = vector(mode = "list", length = 10))
  })
  
  observeEvent(updateData$datos.aprendizaje,{
    return.svm.default.values()
  })

  
  #Update model tab
  output$txtSvm <- renderPrint({
    input$runSvm
    tryCatch({
      codigo.svm()
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba      <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        scale  <- as.logical(input$switch_scale_svm)
        kernel <- input$kernel.svm
      })
      
      var    <- as.formula(paste0(variable.predecir, "~."))
       
      nombreModelo <<- paste0(nombreBase, kernel)
      
      #Model generate
      modelo.svm <- traineR::train.svm(var, data = datos.aprendizaje, scale = as.logical(scale), kernel = kernel)

      #Prediccion
      prediccion.svm <- predict(modelo.svm, datos.prueba)$prediction

      #Indices
      indices.svm <- general_indices(datos.prueba[,variable.predecir], prediccion.svm)

      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate({
        modelos$svm[[nombreModelo]] <- list(modelo     = modelo.svm, 
                                            prediccion = prediccion.svm, 
                                            indices = indices.svm, 
                                            id      = kernel)
        modelos2$svm$n <- modelos2$svm$n + 1
        modelos2$svm$mcs[modelos2$svm$n] <- list(indices.svm)
        if(modelos2$svm$n > 9)
          modelos2$svm$n <- 0
        
      })
      if(!is.null(modelos$svm[[nombreModelo]])){
        modelo.svm <- modelos$svm[[nombreModelo]]$modelo
        #Cambiamos la forma en que va aparecer el call
        modelo.svm$call$formula <- var
        modelo.svm$call$kernel  <- kernel
        modelo.svm$call$scale   <- scale
        
        print(modelo.svm)
      }
      else{NULL}
      
    }, error = function(e){
      isolate(modelos$svm[[nombreModelo]] <- NULL)
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
        tb_predic(real.val, prediccion.svm, updateData$decimals, codedioma$idioma)
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
        idioma <- codedioma$idioma
        
        codigo <- disp_models("modelo.svm", paste0(tr("svm", idioma),"-",kernel), variable.predecir)
        cod    <- paste0("### docdisp\n",codigo, "\n")
        
        isolate(codedioma$code <- append(codedioma$code, cod))
        
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
        idioma <- codedioma$idioma
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
      if(!is.null(modelos$svm[[nombreModelo]])){
        idioma <- codedioma$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(summary_indices(updateData$datos.prueba[,updateData$variable.predecir]),
                              decimals, 
                              idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (SVM-05) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%', align = 'c')
  
  # Execute model, prediction and indices
  codigo.svm <- function() {
    tryCatch({

      isolate({
        variable.predecir <- updateData$variable.predecir
        scale  <- as.logical(input$switch_scale_svm)
        kernel <- input$kernel.svm
      })
      
      #Model generate
      codigo <- codeSvm(variable.predecir,scale, kernel)
      cod    <- paste0("### SVM\n", codigo)
      
      #Prediccion
      codigo <- codigo.prediccion("svm")
      cod    <- paste0(cod, codigo)
      #Indices
      codigo <- codigo.IG(model.name = "svm", variable.pr = variable.predecir)
      cod    <- paste0(cod, codigo)
      
      isolate(codedioma$code <- append(codedioma$code, cod))
      
    }, error = function(e){
      showNotification(paste0("Error (SVM-00) : ",e), duration = 10, type = "error")
    })
  }
  
}

## To be copied in the UI
# mod_SVM_ui("SVM_ui_1")

## To be copied in the server
# callModule(mod_SVM_server, "SVM_ui_1")

