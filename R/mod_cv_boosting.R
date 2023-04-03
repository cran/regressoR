#' cv_boosting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_boosting_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxboosting"), 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVboostingModelo",
               div(col_6(numericInput(ns("iter.boosting"), labelInput("numTree"), 20, width = "100%",min = 1)),
                   col_6(numericInput(ns("shrinkage.boosting"), labelInput("shrinkage"), 0.1, width = "100%",min = 0.01, step = 0.01))),
               div(col_12(
                 selectizeInput(inputId = ns("sel_kernel"), label = labelInput("selectAlg"), multiple = T,
                                choices =  c("gaussian", "laplace", "tdist"))
               )),
               div(col_12(id = ns("texto"),
                          style = "display:block",withLoader(verbatimTextOutput(ns("txtcvboosting")), 
                                                             type = "html", loader = "loader4"))),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_boosting"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVboostingIndices",
               div(col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectInd"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("cvcv_glo"),label = NULL,
                                                  choices =  "", width = "100%")))),
                   col_4(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type_p"),label = NULL,
                                                  choices =  c( "lineas","barras", "error"), width = "100%")))), hr()),
               div(col_12(echarts4rOutput(ns("e_boosting_ind"), width = "100%", height = "70vh"))))
    )
    
  )
}
    
#' cv_boosting Server Functions
#'
#' @noRd 
mod_cv_boosting_server <- function(input, output, session, updateData, codedioma){
  ns <- session$ns
  
  
  M <- rv(MCs.boosting = NULL, grafico = NULL, er = NULL, ea = NULL, corr = NULL, times = 0, summary = NULL)
  
  observeEvent(codedioma$idioma, {
    
    nombres <- list( "lineas", "barras","error")
    names(nombres) <- tr(c( "grafLineas","grafBarras", "grafError"),codedioma$idioma)
    indices <- list(0, 1, 2, 3)
    names(indices) <- tr(c("RMSE", "MAE", "ER", "correlacion"),codedioma$idioma) 
    updateSelectInput(session, "cvcv_glo", choices = indices, selected = 0)
    updateSelectInput(session, "plot_type_p", choices = nombres, selected = "lineas")
  })
  
  observeEvent(c(updateData$datos, updateData$variable.predecir), {
    M$MCs.boosting <- NULL
    M$grafico <- NULL
    M$ea   <- NULL
    M$er   <- NULL
    M$corr <- NULL
    M$times      <- 0
    datos        <- updateData$datos
    
    if(!is.null(datos)){
      updateNumericInput(session, inputId = "iter.boosting", value = 20)
      updateNumericInput(session, inputId = "shrinkage.boosting", value = 0.1)
      updateSelectizeInput(session, "sel_kernel", selected = "")
    }
  })
  
  output$txtcvboosting <- renderPrint({
    input$btn_cv_boosting
    M$MCs.boosting <- NULL
    M$grafico <- NULL
    M$ea   <- NULL
    M$er   <- NULL
    M$corr <- NULL
    tryCatch({
      kernels   <- isolate(input$sel_kernel)
      cant.vc   <- isolate(updateData$numValC)
      MCs.boosting <- vector(mode = "list")
      datos     <- isolate(updateData$datos)
      numGrupos <- isolate(updateData$numGrupos)
      grupos    <- isolate(updateData$grupos)
      n.trees   <- isolate(input$iter.boosting)
      shrinkage <- isolate(input$shrinkage.boosting)
      variable  <- updateData$variable.predecir
      var_      <- paste0(variable, "~.")
      nombres   <- vector(mode = "character", length = length(kernels))
      
      if(length(kernels)<1){
        if(M$times != 0)
          showNotification("Debe seleccionar al menos un kernel")
      }
      for (kernel in 1:length(kernels)){
        MCs.boosting[[paste0("MCs.",kernels[kernel])]] <- vector(mode = "list", length = cant.vc)
        nombres[kernel] <- paste0("MC.",kernels[kernel])
      }
      for (i in 1:cant.vc){
        MC.boosting <- vector(mode = "list", length = length(kernels))
        names(MC.boosting) <- nombres
        for (kernel in 1:length(kernels)){
          MC.boosting[[kernel]] <- vector(mode = "list", 4)
          names(MC.boosting[[kernel]]) <- c("Raiz.Error.Cuadratico", "Error.Absoluto", "Error.Relativo", "Correlacion")
        }
        
        for (k in 1:numGrupos){
          muestra   <- grupos[[i]][[k]]
          ttraining <- datos[-muestra, ]
          ttesting  <- datos[muestra, ]
          
          for (j in 1:length(kernels)){
            modelo      <- train.gbm(as.formula(var_), 
                                     data         = ttraining, 
                                     distribution = kernels[j], 
                                     n.trees      = n.trees, 
                                     shrinkage    = shrinkage)
            
            prediccion  <- predict(modelo, ttesting)
            MC          <- general_indices(ttesting[,variable], prediccion$prediction)
            MC.boosting[[j]] <- Map(c, MC.boosting[[j]], MC)
            
          }
        }
        
        for (l in 1:length(MCs.boosting)){
          MCs.boosting[[l]][[i]] <-  sapply(MC.boosting[[l]],mean)
        }
      }
      
      M$MCs.boosting  <- MCs.boosting
      
      resultados <- indices.cv(cant.vc, kernels, MCs.boosting)
      
      M$grafico  <- resultados$grafico
      M$ea   <- resultados$ea
      M$er   <- resultados$er
      M$corr <- resultados$corr
      M$summary <- summary_indices_v(datos[[variable]])
      isolate(codedioma$code <- append(codedioma$code, cv_boost_code(variable, cant.vc, numGrupos)))
      
      print(MCs.boosting)
      
    },error = function(e){
      M$MCs.boosting <- NULL
      M$grafico <- NULL
      M$ea   <- NULL
      M$er   <- NULL
      M$corr <- NULL
      M$times    <- 0 
      return(invisible(""))
    })
  })
  
  output$e_boosting_ind  <-  renderEcharts4r({
    idioma <- codedioma$idioma
    tryCatch({
      indice  <- input$cvcv_glo
      type    <- input$plot_type_p
      grafico <- M$grafico
      if(!is.null(grafico)){
        label <- switch (indice,
                         "0" = tr("RMSE",idioma),
                         "1" = tr("MAE",idioma),
                         "2" = tr("ER",idioma),
                         "3" = tr("correlacion",idioma)
        )
        grafico$value <- switch (indice,
                                 "0" = grafico$value,
                                 "1" = M$ea,
                                 "2" = M$er,
                                 "3" = M$corr
        )
        p <- ifelse(indice == "2", TRUE, FALSE)
        switch (type,
                "barras" = return( resumen.barras(grafico, labels = c(label,"Kernel",tr(c("maximo","minimo", "q1", "q3"), idioma)) ,percent = p, vals = M$summary)),
                "error" = return( resumen.error(grafico, labels = c(label, "Kernel", tr(c("maximo","minimo", "q1", "q3"), idioma)),percent = p, vals = M$summary)),
                "lineas" = return( resumen.lineas(grafico, labels = c(label, tr(c("crossval", "maximo","minimo", "q1", "q3"), idioma)), percent = p, vals = M$summary))
        )
      }
      else
        return(NULL)
    },error = function(e){
      showNotification(e)
      return(NULL)
    })
  })
  
}
    
## To be copied in the UI
# mod_cv_boosting_ui("cv_boosting_1")
    
## To be copied in the server
# mod_cv_boosting_server("cv_boosting_1")
