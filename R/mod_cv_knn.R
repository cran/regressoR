#' cv_knn UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_knn_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxKnn"), 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVKnnModelo",
               div(col_6(numericInput(ns("kmax_cvknn"), labelInput("kv"), min = 1,step = 1, value = 7)),
                   col_6(radioSwitch(ns("scale_cvknn"), "escal", c("si", "no")))),
               div(col_6(
                 selectizeInput(
                   ns("sel_kernel"), labelInput("selkernel"), multiple = T,
                   choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                               "triweight", "cos","inv","gaussian"))
               ),col_6(
                 numericInput(ns("distance_cvknn"), labelInput("distknn"), min = 1,step = 1, value = 2)
               )),
               fluidRow(),
               div(col_12(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txtcvknn")), 
                                                      type = "html", loader = "loader4"))),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_knn"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVKnnIndices",
               div(col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectInd"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("cvcv_glo"),label = NULL,
                                                  choices =  "", width = "100%")))),
                   col_4(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type_p"),label = NULL,
                                                  choices =  c("lineas","barras",  "error"), width = "100%")))), hr()),
               div(col_12(echarts4rOutput(ns("e_knn_ind"), width = "100%", height = "70vh"))))
    )
    
  )
}

#' cv_knn Server Functions
#'
#' @noRd 
mod_cv_knn_server <- function(input, output, session, updateData, codedioma){
  ns <- session$ns
  
  
  M <- rv(MCs.knn = NULL, grafico = NULL, er = NULL, ea = NULL, corr = NULL, times = 0, summary = NULL)
  
  observeEvent(codedioma$idioma, {
    
    nombres <- list("lineas","barras",  "error")
    names(nombres) <- tr(c("grafLineas","grafBarras",  "grafError"),codedioma$idioma)
    indices <- list(0, 1, 2, 3)
    names(indices) <- tr(c("RMSE", "MAE", "ER", "correlacion"),codedioma$idioma) 
    updateSelectInput(session, "cvcv_glo", choices = indices, selected = 0)
    updateSelectInput(session, "plot_type_p", choices = nombres, selected = "lineas")
  })
  
  observeEvent(c(updateData$datos, updateData$variable.predecir), {
    M$MCs.knn <- NULL
    M$grafico <- NULL
    M$ea   <- NULL
    M$er   <- NULL
    M$corr <- NULL
    M$times      <- 0
    datos        <- updateData$datos

    if(!is.null(datos)){
      updateNumericInput(session,"kmax_cvknn",value = round(sqrt(nrow(datos))))
      updateSelectizeInput(session, "sel_kernel", selected = "")
    }
  })
  
  output$txtcvknn <- renderPrint({
    input$btn_cv_knn
    M$MCs.knn <- NULL
    M$grafico <- NULL
    M$ea   <- NULL
    M$er   <- NULL
    M$corr <- NULL
    tryCatch({
      kernels   <- isolate(input$sel_kernel)
      cant.vc   <- isolate(updateData$numValC)
      MCs.knn   <- vector(mode = "list")
      datos     <- isolate(updateData$datos)
      numGrupos <- isolate(updateData$numGrupos)
      grupos    <- isolate(updateData$grupos)
      kmax      <- isolate(input$kmax_cvknn)
      scales    <- isolate(input$scale_cvknn)
      distance  <- isolate(input$distance_cvknn)
      variable  <- updateData$variable.predecir
      var_      <- paste0(variable, "~.")
      nombres   <- vector(mode = "character", length = length(kernels))
      
      if(length(kernels)<1){
        if(M$times != 0)
          showNotification("Debe seleccionar al menos un kernel")
      }
      for (kernel in 1:length(kernels)){
        MCs.knn[[paste0("MCs.",kernels[kernel])]] <- vector(mode = "list", length = cant.vc)
        nombres[kernel] <- paste0("MC.",kernels[kernel])
      }
      for (i in 1:cant.vc){
        MC.knn <- vector(mode = "list", length = length(kernels))
        names(MC.knn) <- nombres
        for (kernel in 1:length(kernels)){
          MC.knn[[kernel]] <- vector(mode = "list", 4)
          names(MC.knn[[kernel]]) <- c("Raiz.Error.Cuadratico", "Error.Absoluto", "Error.Relativo", "Correlacion")
        }
        
        for (k in 1:numGrupos){
          muestra   <- grupos[[i]][[k]]
          ttraining <- datos[-muestra, ]
          ttesting  <- datos[muestra, ]
          
          for (j in 1:length(kernels)){
            modelo      <- train.knn(as.formula(var_), 
                                     data   = ttraining, 
                                     kernel = kernels[j], 
                                     kmax   = kmax, 
                                     scale  = as.logical(scales), 
                                     distance = distance)
            
            prediccion  <- predict(modelo, ttesting)
            MC          <- general_indices(ttesting[,variable], prediccion$prediction)
            MC.knn[[j]] <- Map(c, MC.knn[[j]], MC)
            
          }
        }
        
        for (l in 1:length(MCs.knn)){
          MCs.knn[[l]][[i]] <-  sapply(MC.knn[[l]],mean)
        }
      }
      
      M$MCs.knn  <- MCs.knn
      
      resultados <- indices.cv(cant.vc, kernels, MCs.knn)
      
      M$grafico  <- resultados$grafico
      M$ea   <- resultados$ea
      M$er   <- resultados$er
      M$corr <- resultados$corr
      M$summary <- summary_indices_v(datos[[variable]])
      isolate(codedioma$code <- append(codedioma$code, cv_knn_code(variable, cant.vc, numGrupos)))
      print(MCs.knn)
      
    },error = function(e){
      M$MCs.knn <- NULL
      M$grafico <- NULL
      M$ea   <- NULL
      M$er   <- NULL
      M$corr <- NULL
      M$times    <- 0 
      return(invisible(""))
    })
  })
  
  output$e_knn_ind  <-  renderEcharts4r({
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
# mod_cv_knn_ui("cv_knn_1")

## To be copied in the server
# mod_cv_knn_server("cv_knn_1")
