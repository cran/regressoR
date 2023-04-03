#' cv_dt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cv_dt_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tabBoxPrmdt(
      id = ns("Boxdt"), 
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVdtModelo",
               div(col_6(numericInput(ns("minsplit.dt"), labelInput("minsplit"), 20, width = "100%",min = 1)),
                   col_6(numericInput(ns("maxdepth.dt"), labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1) )),
               div(col_12(
                 selectizeInput(
                   ns("sel_split"), labelInput("splitIndex"), multiple = T,
                   choices =  list("gini" = "gini", "Entropia" = "information")))),
               div(col_12(id = ns("texto"),
                          style = "display:block",withLoader(verbatimTextOutput(ns("txtcvdt")), 
                                                             type = "html", loader = "loader4"))),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv_dt"), labelInput("generar"), width  = "100%" ),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabCVdtIndices",
               div(col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectInd"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("cvcv_glo"),label = NULL,
                                                  choices =  "", width = "100%")))),
                   col_4(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type_p"),label = NULL,
                                                  choices =  c("lineas","barras",  "error"), width = "100%")))), hr()),
               div(col_12(echarts4rOutput(ns("e_dt_ind"), width = "100%", height = "70vh"))))
    )
    
  )
}
    
#' cv_dt Server Functions
#'
#' @noRd 
mod_cv_dt_server <- function(input, output, session, updateData, codedioma){
  ns <- session$ns
  
  M <- rv(MCs.dt = NULL, grafico = NULL, er = NULL, ea = NULL, corr = NULL, times = 0, summary = NULL)
  
  observeEvent(codedioma$idioma, {
    
    nombres <- list("lineas","barras",  "error")
    names(nombres) <- tr(c("grafLineas","grafBarras",  "grafError"),codedioma$idioma)
    indices <- list(0, 1, 2, 3)
    names(indices) <- tr(c("RMSE", "MAE", "ER", "correlacion"),codedioma$idioma) 
    updateSelectInput(session, "cvcv_glo", choices = indices, selected = 0)
    updateSelectInput(session, "plot_type_p", choices = nombres, selected = "barras")
  })
  
  observeEvent(c(updateData$datos, updateData$variable.predecir), {
    M$MCs.dt <- NULL
    M$grafico <- NULL
    M$ea   <- NULL
    M$er   <- NULL
    M$corr <- NULL
    M$times      <- 0
    datos        <- updateData$datos
    
    if(!is.null(datos)){
      updateSelectizeInput(session, "sel_split", selected = "")
    }
  })
  
  
  output$txtcvdt <- renderPrint({
    input$btn_cv_dt
    M$MCs.dt <- NULL
    M$grafico <- NULL
    M$ea   <- NULL
    M$er   <- NULL
    M$corr <- NULL
    tryCatch({
      kernels   <- isolate(input$sel_split)
      cant.vc   <- isolate(updateData$numValC)
      MCs.dt    <- vector(mode = "list")
      datos     <- isolate(updateData$datos)
      numGrupos <- isolate(updateData$numGrupos)
      grupos    <- isolate(updateData$grupos)
      maxdepth  <- isolate(input$maxdepth.dt)
      minsplit  <- isolate(input$minsplit.dt)
      variable  <- updateData$variable.predecir
      var_      <- paste0(variable, "~.")
      nombres   <- vector(mode = "character", length = length(kernels))
      
      if(length(kernels)<1){
        if(M$times != 0)
          showNotification("Debe seleccionar al menos un kernel")
      }
      for (kernel in 1:length(kernels)){
        MCs.dt[[paste0("MCs.",kernels[kernel])]] <- vector(mode = "list", length = cant.vc)
        nombres[kernel] <- paste0("MC.",kernels[kernel])
      }
      for (i in 1:cant.vc){
        MC.dt <- vector(mode = "list", length = length(kernels))
        names(MC.dt) <- nombres
        for (kernel in 1:length(kernels)){
          MC.dt[[kernel]] <- vector(mode = "list", 4)
          names(MC.dt[[kernel]]) <- c("Raiz.Error.Cuadratico", "Error.Absoluto", "Error.Relativo", "Correlacion")
        }
        
        for (k in 1:numGrupos){
          muestra   <- grupos[[i]][[k]]
          ttraining <- datos[-muestra, ]
          ttesting  <- datos[muestra, ]
          
          for (j in 1:length(kernels)){
            modelo      <- train.rpart(as.formula(var_), 
                                     data   = ttraining, 
                                     parms = list(split = kernels[j]),
                                     control = rpart.control(minsplit = minsplit, 
                                                             maxdepth = maxdepth), 
                                     model = TRUE)
            
            prediccion  <- predict(modelo, ttesting)
            MC          <- general_indices(ttesting[,variable], prediccion$prediction)
            MC.dt[[j]] <- Map(c, MC.dt[[j]], MC)
            
          }
        }
        
        for (l in 1:length(MCs.dt)){
          MCs.dt[[l]][[i]] <-  sapply(MC.dt[[l]],mean)
        }
      }
      
      M$MCs.dt  <- MCs.dt
      
      resultados <- indices.cv(cant.vc, kernels, MCs.dt)
      
      M$grafico  <- resultados$grafico
      M$ea   <- resultados$ea
      M$er   <- resultados$er
      M$corr <- resultados$corr
      M$summary <- summary_indices_v(datos[[variable]])
      isolate(codedioma$code <- append(codedioma$code, cv_dt_code(variable, cant.vc, numGrupos)))
      
      print(MCs.dt)
      
    },error = function(e){
      M$MCs.dt <- NULL
      M$grafico <- NULL
      M$ea   <- NULL
      M$er   <- NULL
      M$corr <- NULL
      M$times    <- 0 
      return(invisible(""))
    })
  })
  
  output$e_dt_ind  <-  renderEcharts4r({
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
# mod_cv_dt_ui("cv_dt_1")
    
## To be copied in the server
# mod_cv_dt_server("cv_dt_1")
