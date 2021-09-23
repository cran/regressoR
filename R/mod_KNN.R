#' KNN UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_KNN_ui <- function(id){
  ns <- NS(id)
  
  knn.options <- list(options.run(ns("runKnn")), tags$hr(style = "margin-top: 0px;"),
                      fluidRow(column(numericInput(ns("k.knn"), labelInput("kv"), min = 1,step = 1, value = 7), width = 5),
                               column(selectInput(inputId = ns("kernel.knn"), label = labelInput("selkernel"),selected = "optimal",
                                                  choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                              "triweight", "cos","inv","gaussian")),width = 5)),
                      fluidRow(column(radioSwitch(id = ns("switch_scale_knn"), label = "escal",
                                                       names = c("si", "no")), width=5),
                               column(width=5, numericInput(ns("distance.knn"), labelInput("distknn"), min = 1,step = 1, value = 2))) )
  
  knn.code.config <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                          codigo.monokai(ns("fieldCodeKnn"), height = "7vh"))
  
  knn.code <- list(h3(labelInput("codigo")), hr(style = "margin-top: 0px;"),
                   conditionalPanel("input.BoxKnn == 'tabKknPred'",
                                    codigo.monokai(ns("fieldCodeKnnPred"), height = "7vh"),ns = ns),
                   conditionalPanel("input.BoxKnn == 'tabKknDisp'",
                                    codigo.monokai(ns("fieldCodeKnnDisp"), height = "7vh"),ns = ns),
                   conditionalPanel("input.BoxKnn == 'tabKknIndex'",
                                    codigo.monokai(ns("fieldCodeKnnIG"), height = "7vh"),ns = ns))
  
  
  tabs.options.generate <- tabsOptions(buttons = list(icon("gear"), icon("code")), widths = c(50,100), heights = c(90,70),
                                       tabs.content = list(knn.options,knn.code.config))
  
  tabs.options.Nogenerate <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(70),
                                         tabs.content = list(knn.code))
  
  generate.knn.panel <- tabPanel(title = labelInput("generatem"), value = "tabKknModelo",
                                 withLoader(verbatimTextOutput(ns("txtknn")),type = "html", loader = "loader4"))
  
  prediccion.knn.panel <- tabPanel(title = labelInput("predm"), value = "tabKknPred",
                                   withLoader(DT::dataTableOutput(ns("knnPrediTable")),type = "html", loader = "loader4"))
  
  disp.knn.panel <- tabPanel(title = labelInput("dispersion"), value = "tabKknDisp",
                             echarts4rOutput(ns('plot_knn_disp'), height = "75vh"))
  
  general.index.knn.panel <- tabPanel(title = labelInput("indices"), value = "tabKknIndex",
                                      br(),
                                      fluidRow(withLoader(tableOutput(ns('indexdfknn')),type = "html", loader = "loader4")),
                                      br(),
                                      fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                      br(),
                                      fluidRow(withLoader(tableOutput(ns('indexdfknn2')),type = "html", loader = "loader4")))
  
  page.knn <- tabItem(tabName = "knn",
                      tabBox(id = ns("BoxKnn"), width = NULL, height ="80%",
                             generate.knn.panel,
                             prediccion.knn.panel,
                             disp.knn.panel,
                             general.index.knn.panel,
                             conditionalPanel("input.BoxKnn == 'tabKknModelo'",tabs.options.generate,ns = ns),
                             conditionalPanel("input.BoxKnn != 'tabKknModelo'",tabs.options.Nogenerate,ns = ns)))
  
  tagList(
    page.knn
  )
}

#' KNN Server Function
#'
#' @noRd 
mod_KNN_server <- function(input, output, session,updateData, modelos){
  ns <- session$ns
  
  nombreBase <- "modelo.knn."
  nombreModelo <- "modelo.knn."
  
  return.knn.default.values <- function(){
    updateNumericInput(session, "k.knn", value = 7)
    updateSelectInput(session, "kernel.knn",selected = "optimal")
    updateRadioSwitch(session,"switch_scale_knn","TRUE")
    updateNumericInput(session, "distance.knn", value = 2)
    
    isolate(datos.aprendizaje <- updateData$datos.aprendizaje)
    if(!is.null(datos.aprendizaje)){
      updateNumericInput(session, "k.knn", value = round(sqrt(nrow(datos.aprendizaje))))
    }
    
    nombreModelo <- "modelo.knn."
  }
  
  
  
  observeEvent(updateData$datos.aprendizaje,{
    return.knn.default.values()
  })
  
  
  # When the knn model is generated
  observeEvent(input$runKnn, {
    if (validate_data(updateData, idioma = updateData$idioma)) { # Si se tiene los datos entonces :
      knn_full()
    }
  })
  
  
  # Execute model, prediction and indices
  knn_full <- function() {
    tryCatch({
      shinyjs::runjs(code = "generating_model = true")
      
      isolate({
        datos.aprendizaje <- updateData$datos.aprendizaje
        datos.prueba <- updateData$datos.prueba
        variable.predecir <- updateData$variable.predecir
        scale <- as.logical(input$switch_scale_knn)
        kernel <- input$kernel.knn
        k <- input$k.knn
        distance <- input$distance.knn
      })
      
      nombreModelo <<- paste0(nombreBase, kernel)
      
      #Validacion tamaño del k
      tam <- nrow(datos.aprendizaje)
      if(k >= tam){
        k <- tam - 2
        updateNumericInput(session, "k.knn", value = tam - 2)
      }
      
      #Model generate
      modelo.knn <- kkn_model(datos.aprendizaje,variable.predecir, scale, k, kernel, distance)
      updateAceEditor(session, "fieldCodeKnn", value = codeKnn(variable.predecir,scale, k, kernel, distance))
      
      #Prediccion
      prediccion.knn <- kkn_prediction(modelo.knn, datos.prueba)
      updateAceEditor(session, "fieldCodeKnnPred", value = codeKnnPred(nombreModelo))
      
      #Indices
      indices.knn <- general_indices(datos.prueba[,variable.predecir], prediccion.knn)
      updateAceEditor(session, "fieldCodeKnnIG", value = codeKnnIG(variable.predecir))
      
      #isolamos para que no entre en un ciclo en el primer renderPrint
      isolate(modelos$knn[[nombreModelo]] <- list(modelo = modelo.knn, prediccion = prediccion.knn, indices = indices.knn,
                                                  id = kernel))
    }, error = function(e){
      isolate(modelos$knn[[nombreModelo]] <- NULL)
      showNotification(paste0("Error (KNN-00) : ",e), duration = 10, type = "error")
    },
    finally = {shinyjs::runjs(code = "generating_model = false")})
  }
  
  #Update model tab
  output$txtknn <- renderPrint({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])){
        modelo.knn <- modelos$knn[[nombreModelo]]$modelo
        print(modelo.knn)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (KNN-01) : ",e), duration = 10, type = "error")
      NULL
    })
  })
  
  # Update prediction tab
  output$knnPrediTable <- DT::renderDataTable({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])){
        prediccion.knn <- modelos$knn[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          real.val <- datos.prueba[updateData$variable.predecir]
        })
        tb_predic(real.val, prediccion.knn, updateData$decimals, updateData$idioma)
      }
      else{NULL}
      
    }, error = function(e){
      showNotification(paste0("Error (KNN-02) : ", e), duration = 10, type = "error")
      NULL
    })
  }, server = F)
  
  
  # Update Dispersion Tab
  output$plot_knn_disp <- renderEcharts4r({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])){
        prediccion.knn <- modelos$knn[[nombreModelo]]$prediccion
        isolate({
          datos.prueba <- updateData$datos.prueba
          variable.predecir <- updateData$variable.predecir
          kernel <- input$kernel.knn
        })
        idioma <- updateData$idioma
        
        codigo <- disp_models(nombreModelo, paste0(tr("knn", idioma),"-",kernel), variable.predecir)
        updateAceEditor(session, "fieldCodeKnnDisp", value = codigo)
        
        titulos <- c(
          tr("predvsreal", idioma),
          tr("realValue", idioma),
          tr("pred", idioma)
        )
        
        plot_real_prediction(datos.prueba[variable.predecir],prediccion.knn,
                             paste0(tr("knn", idioma),"-",kernel),titulos)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (KNN-03) : ", e), duration = 10, type = "error")
      NULL
    })
  })
  
  
  
  #Update Indices tab
  output$indexdfknn <- renderTable({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])){
        idioma <- updateData$idioma
        indices.knn <- modelos$knn[[nombreModelo]]$indices
        tabla.indicesPrecision(indices.knn, updateData$decimals, idioma)
      }
      else{NULL}
    }, error = function(e){
      showNotification(paste0("Error (KNN-04) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%',align = 'c')
  
  
  
  output$indexdfknn2 <- renderTable({
    tryCatch({
      if(!is.null(modelos$knn[[nombreModelo]])& !is.null(updateData$summary.var.pred)){
        idioma <- updateData$idioma
        decimals <- updateData$decimals
        tabla.varpred.summary(updateData$summary.var.pred, decimals, idioma)
      }
      else{NULL}
    }
    , error = function(e){
      showNotification(paste0("Error (KNN-05) : ",e), duration = 10, type = "error")
      NULL
    })
  },striped = TRUE, bordered = TRUE, spacing = 'l', 
  width = '100%',align = 'c')
  
}

## To be copied in the UI
# mod_KNN_ui("KNN_ui_1")

## To be copied in the server
# callModule(mod_KNN_server, "KNN_ui_1")

