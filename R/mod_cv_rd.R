#' cv_rd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_rd_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxrd"), 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVrdModelo",
               div(col_6(
                 radioSwitch(ns("permitir_ncomp"), "", c("manual", "automatico"), val.def = FALSE)
               ),col_6(
                 numericInput(ns("ncomp.rd"), labelInput("ncomp"),value = 2, min = 1, width = "100%")
               )),
               div(col_6(selectizeInput(
                 ns("sel_kernel"), labelInput("selectAlg"), multiple = T,
                 choices = list("ACP" = 0, "MCP" = 1))),
                   col_6(radioSwitch(ns("scale_cvrd"), "escal", c("si", "no")))),
               div(col_12(id = ns("texto"),
                          style = "display:block",withLoader(verbatimTextOutput(ns("txtcvrd")), 
                                                             type = "html", loader = "loader4"))),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_rd"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVrdIndices",
               div(col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectInd"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("cvcv_glo"),label = NULL,
                                                  choices =  "", width = "100%")))),
                   col_4(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type_p"),label = NULL,
                                                  choices =  c("lineas","barras",  "error"), width = "100%")))), hr()),
               div(col_12(echarts4rOutput(ns("e_rd_ind"), width = "100%", height = "70vh"))))
    )
    
  )
}

#' cv_rd Server Functions
#'
#' @noRd 
mod_cv_rd_server <- function(input, output, session, updateData, codedioma){
  ns <- session$ns
  
  
  M <- rv(MCs.rd = NULL, grafico = NULL, er = NULL, ea = NULL, corr = NULL, times = 0, summary = NULL)
  
  observeEvent(codedioma$idioma, {
    
    nombres <- list("lineas","barras",  "error")
    names(nombres) <- tr(c("grafLineas","grafBarras",  "grafError"),codedioma$idioma)
    indices <- list(0, 1, 2, 3)
    names(indices) <- tr(c("RMSE", "MAE", "ER", "correlacion"),codedioma$idioma) 
    updateSelectInput(session, "cvcv_glo", choices = indices, selected = 0)
    updateSelectInput(session, "plot_type_p", choices = nombres, selected = "lineas")
  })
  
  observeEvent(c(updateData$datos, updateData$variable.predecir), {
    M$MCs.rd <- NULL
    M$grafico <- NULL
    M$ea   <- NULL
    M$er   <- NULL
    M$corr <- NULL
    M$times      <- 0
    datos        <- updateData$datos
    
    if(!is.null(datos)){
      updateRadioSwitch(session,"switch_scale_rd","TRUE")
      
      updateSelectizeInput(session, "sel_kernel", selected = "")
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
  
  output$txtcvrd <- renderPrint({
    input$btn_cv_rd
    M$MCs.rd  <- NULL
    M$grafico <- NULL
    M$ea   <- NULL
    M$er   <- NULL
    M$corr <- NULL
    tryCatch({
      kernels   <- isolate(input$sel_kernel)
      cant.vc   <- isolate(updateData$numValC)
      MCs.rd    <- vector(mode = "list")
      datos     <- isolate(updateData$datos)
      numGrupos <- isolate(updateData$numGrupos)
      grupos    <- isolate(updateData$grupos)
      scales    <- isolate(input$scale_cvrd)
      variable  <- updateData$variable.predecir
      var_      <- paste0(variable, "~.")
      nombres   <- vector(mode = "character", length = length(kernels))
      permitir_ncomp <- isolate(input$permitir_ncomp)
      ncomp.rd <- isolate(input$ncomp.rd)
      mode_labels <- kernels
      mode_labels[which(mode_labels == 0)] = "ACP"
      mode_labels[which(mode_labels == 1)] = "MCP"
      
      if(length(kernels)<1){
        if(M$times != 0)
          showNotification("Debe seleccionar al menos un kernel")
      }
      
      ncomp <- NULL
      if (as.logical(permitir_ncomp) && !is.na(ncomp.rd)) {
        if(ncomp.rd >= 1 && ncomp.rd <= ncol(datos)){
          ncomp <- ncomp.rd
        }
      }
      for (kernel in 1:length(kernels)){
        MCs.rd[[paste0("MCs.",mode_labels[kernel])]] <- vector(mode = "list", length = cant.vc)
        nombres[kernel] <- paste0("MC.",mode_labels[kernel])
      }
      for (i in 1:cant.vc){
        MC.rd <- vector(mode = "list", length = length(kernels))
        names(MC.rd) <- nombres
        for (kernel in 1:length(kernels)){
          MC.rd[[kernel]] <- vector(mode = "list", 4)
          names(MC.rd[[kernel]]) <- c("Raiz.Error.Cuadratico", "Error.Absoluto", "Error.Relativo", "Correlacion")
        }
        
        for (k in 1:numGrupos){
          muestra   <- grupos[[i]][[k]]
          ttraining <- datos[-muestra, ]
          ttesting  <- datos[muestra, ]
          
          for (j in 1:length(kernels)){
            modelo      <- rd_model(data = ttraining, variable.pred = variable,
                                     mode = kernels[j], scale = as.logical(scales))
            ncomp       <- ifelse(is.null(ncomp), modelo$optimal.n.comp, ncomp)
            prediccion  <- predict(modelo, ttesting, ncomp = ncomp)
            MC          <- general_indices(ttesting[,variable], prediccion)
            MC.rd[[j]]  <- Map(c, MC.rd[[j]], MC)
          }
        }
        
        for (l in 1:length(MCs.rd)){
          MCs.rd[[l]][[i]] <-  sapply(MC.rd[[l]],mean)
        }
      }
      
      M$MCs.rd  <- MCs.rd
      
      resultados <- indices.cv(cant.vc, mode_labels, MCs.rd)
      M$grafico  <- resultados$grafico
      M$ea   <- resultados$ea
      M$er   <- resultados$er
      M$corr <- resultados$corr
      M$summary <- summary_indices_v(datos[[variable]])
      isolate(codedioma$code <- append(codedioma$code, cv_rd_code(variable, cant.vc, numGrupos)))
      print(MCs.rd)
      
    },error = function(e){
      M$MCs.rd <- NULL
      M$grafico <- NULL
      M$ea   <- NULL
      M$er   <- NULL
      M$corr <- NULL
      M$times    <- 0 
      return(invisible(""))
    })
  })
  
  output$e_rd_ind  <-  renderEcharts4r({
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
# mod_cv_rd_ui("cv_rd_1")

## To be copied in the server
# mod_cv_rd_server("cv_rd_1")
