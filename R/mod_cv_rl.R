#' cv_rl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_rl_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxrl"), 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVrlModelo",
               div(col_12(id = ns("texto"),
                          style = "display:block",withLoader(verbatimTextOutput(ns("txtcvrl")), 
                                                             type = "html", loader = "loader4"))),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_rl"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVrlIndices",
               div(col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectInd"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("cvcv_glo"),label = NULL,
                                                  choices =  "", width = "100%")))),
                   col_4(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type_p"),label = NULL,
                                                  choices =  c("lineas", "barras",   "error"), width = "100%")))), hr()),
               div(col_12(echarts4rOutput(ns("e_rl_ind"), width = "100%", height = "70vh"))))
    )
    
  )
}

#' cv_rl Server Functions
#'
#' @noRd 
mod_cv_rl_server <- function(input, output, session, updateData, codedioma){
  ns <- session$ns
  
  
  M <- rv(MCs.rl = NULL, grafico = NULL, er = NULL, ea = NULL, corr = NULL, times = 0, summary = NULL)
  
  observeEvent(codedioma$idioma, {
    
    nombres <- list("lineas","barras",    "error")
    names(nombres) <- tr(c("grafLineas", "grafBarras", "grafError"),codedioma$idioma)
    indices <- list(0, 1, 2, 3)
    names(indices) <- tr(c("RMSE", "MAE", "ER", "correlacion"),codedioma$idioma) 
    updateSelectInput(session, "cvcv_glo", choices = indices, selected = 0)
    updateSelectInput(session, "plot_type_p", choices = nombres, selected = "lineas")
  })
  
  observeEvent(c(updateData$datos, updateData$variable.predecir), {
    M$MCs.rl <- NULL
    M$grafico <- NULL
    M$ea   <- NULL
    M$er   <- NULL
    M$corr <- NULL
    M$times      <- 0
    datos        <- updateData$datos
    
    if(!is.null(datos)){
    }
  })
  
  output$txtcvrl <- renderPrint({
    input$btn_cv_rl
    M$MCs.rl <- NULL
    M$grafico <- NULL
    M$ea   <- NULL
    M$er   <- NULL
    M$corr <- NULL
    tryCatch({
      cant.vc   <- isolate(updateData$numValC)
      MCs.rl    <- vector(mode = "list")
      datos     <- isolate(updateData$datos)
      numGrupos <- isolate(updateData$numGrupos)
      grupos    <- isolate(updateData$grupos)
      variable  <- updateData$variable.predecir
      var_      <- paste0(variable, "~.")
      nombre    <- "MC.rl"
      
      
      MCs.rl[["MCs.rl"]] <- vector(mode = "list", length = cant.vc)
      
      for (i in 1:cant.vc){
        MC.rl <- vector(mode = "list", length = 1)
        names(MC.rl) <- nombre
        MC.rl[[1]] <- vector(mode = "list", 4)
        names(MC.rl[[1]]) <- c("Raiz.Error.Cuadratico", "Error.Absoluto", "Error.Relativo", "Correlacion")
        
        
        for (k in 1:numGrupos){
          muestra   <- grupos[[i]][[k]]
          ttraining <- datos[-muestra, ]
          ttesting  <- datos[muestra, ]
          j <- 1
          
            modelo      <- lm(as.formula(var_), 
                                     data   = ttraining)
            prediccion  <- predict(modelo, ttesting)
            MC          <- general_indices(ttesting[,variable], prediccion)
            MC.rl[[j]]  <- Map(c, MC.rl[[j]], MC)
          
        }
        
        for (l in 1:length(MCs.rl)){
          MCs.rl[[l]][[i]] <-  sapply(MC.rl[[l]],mean)
        }
      }
      
      M$MCs.rl  <- MCs.rl
      
      resultados <- indices.cv(cant.vc, c("rl"), MCs.rl)
      
      M$grafico  <- resultados$grafico
      M$ea   <- resultados$ea
      M$er   <- resultados$er
      M$corr <- resultados$corr
      M$summary <- summary_indices_v(datos[[variable]])
      isolate(codedioma$code <- append(codedioma$code, cv_rl_code(variable, cant.vc, numGrupos)))
      
      print(MCs.rl)
      
    },error = function(e){
      print(e)
      M$MCs.rl <- NULL
      M$grafico <- NULL
      M$ea   <- NULL
      M$er   <- NULL
      M$corr <- NULL
      M$times    <- 0 
      return(invisible(""))
    })
  })
  
  output$e_rl_ind  <-  renderEcharts4r({
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
        grafico$name <-  tr(grafico$name, idioma)
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
# mod_cv_rl_ui("cv_rl_1")

## To be copied in the server
# mod_cv_rl_server("cv_rl_1")
