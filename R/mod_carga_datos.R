#' carga_datos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_carga_datos_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_5(tabBox(
        id = "tabs",
        title = NULL, width = 12,
        tabPanel(
          title = labelInput("cargar"), width = 12, solidHeader = FALSE,
          collapsible = FALSE, collapsed = FALSE,
          checkboxInput(ns('header'), labelInput("header"), value = T),
          checkboxInput(ns('rowname'), labelInput("Rownames"), value = T),
          radioButtons(ns('sep'), labelInput("separador"), inline = T,
                       choiceNames = c(';', ',', 'TAB'), choiceValues = c(';', ',', '\t')),
          radioButtons(ns('dec'), labelInput("separadordec"), c(',', '.'), inline = T),
          radioSwitch(ns("deleteNA"), label = "eliminana", c("eliminar", "imputar")),
          fileInput(inputId = ns('archivo'), label = labelInput("cargarchivo"), width = "100%",
                    placeholder = "", buttonLabel = labelInput("subir"),
                    accept = c('text/csv', '.csv', '.txt')),
          hr(),
          actionButton(ns("loadButton"), labelInput("cargar"), width = "100%"),
          hr(), codigo.monokai(ns("fieldCodeData"), height = "10vh")),
        
        tabPanel(
          title = labelInput("trans"), width = 12, solidHeader = FALSE,
          collapsible = FALSE, collapsed = FALSE,
          uiOutput(ns('transData')), hr(),
          actionButton(ns('transButton'), labelInput("aplicar"), width = "100%"),
          hr(), codigo.monokai(ns("fieldCodeTrans"), height = "10vh")),
        
        tabPanel(
          title = labelInput("configuraciones"), width = 12, solidHeader = FALSE, 
          collapsible = FALSE, collapsed = FALSE,
          fluidRow(
            col_6(id = "colSemilla", numericInput(ns("semilla"), labelInput("semilla"), value =5, width = "100%")), 
            br(),
            col_6(
              radioSwitch(ns("permitir.semilla"), NULL, c("habilitada", "deshabilitada"), val.def = F)
            )
          ),
          selectInput(ns("sel.predic.var"), label = labelInput("seleccionarPredecir"),
                      choices =  ""),
          sliderInput(ns('segmentacionDatosA'), labelInput("propA"),width = "100%",
                      min = 5, max = 95, value = 70, step = 5),
          sliderInput(ns('segmentacionDatosP'), labelInput("propP"), width = "100%",
                      min = 5, max = 95, value = 30, step = 5),
          actionButton(ns('segmentButton'), labelInput("generar"), width = "100%"),
          br(),br(),codigo.monokai(ns("fieldCodeSegment"), height = "10vh"))
      )),
      col_7(
        box(
          title = labelInput("data"), status = "primary", width = 12,
          solidHeader = TRUE, collapsible = TRUE,
          withLoader(DT::dataTableOutput(ns('tabladatos')), 
                     type = "html", loader = "loader4"), hr(),
          downloadButton(ns("downloaDatos"), labelInput("descargar"), style = "width:100%")
        )
      )
    ),
    conditionalPanel(
      condition = paste0("input.tabs == '", labelInput("configuraciones"),"'"),
      fluidRow(
        col_6( 
          box(title = labelInput("dataA"), status = "primary", 
              width = 12, solidHeader = TRUE, collapsible = TRUE, type = 7, color = "#CBB051",
              withLoader(DT::dataTableOutput(ns('tablaAprendizaje')), 
                         type = "html", loader = "loader4"), hr(),
              downloadButton(ns("downloaDatosA"), labelInput("descargar"), width = "100%"))),
        col_6( 
          box(title = labelInput("dataP"), status = "primary", 
              width = 12, solidHeader = TRUE, collapsible = TRUE, type = 7, color = "#CBB051",
              withLoader(DT::dataTableOutput(ns('tablaPrueba')), 
                         type = "html", loader = "loader4"), hr(),
              downloadButton(ns("downloaDatosP"), labelInput("descargar"), width = "100%")))))
  )
}

#' carga_datos Server Function
#'
#' @keywords internal

mod_carga_datos_server <- function(input, output, session,  updateData, modelos){
  ns <- session$ns
  
  # Descarga tabla de datos
  output$downloaDatos <- downloadHandler(
    filename = function() {
      input$archivo$name
    },
    content = function(file) {
      write.csv(updateData$datos, file, row.names = input$rowname)
    }
  )
  
  # Descarga tabla de Prueba
  output$downloaDatosP <- downloadHandler(
    filename = function() {
      paste0("(",tr("dataP"),")",input$archivo$name)
    },
    content = function(file) {
      write.csv(updateData$datos.prueba, file, row.names = input$rowname)
    }
  )
  # Descarga tabla de Aprendizaje
  output$downloaDatosA <- downloadHandler(
    filename = function() {
      paste0("(",tr("dataA"),")",input$archivo$name)
    },
    content = function(file) {
      write.csv(updateData$datos.aprendizaje, file, row.names = input$rowname)
    }
  )
  
  # Load Button Function
  observeEvent(input$loadButton, {
    rowname    <- isolate(input$rowname)
    ruta       <- isolate(input$archivo)
    sep        <- isolate(input$sep)
    dec        <- isolate(input$dec)
    encabezado <- isolate(input$header)
    deleteNA   <- isolate(input$deleteNA)
    
    tryCatch({
      codigo <- code.carga(rowname, ruta$name, sep, dec, encabezado, deleteNA)
      updateAceEditor(session, "fieldCodeData", value = codigo)
      
      updateData$originales <- carga.datos(
      rowname, ruta$datapath, sep, dec, encabezado, deleteNA)
      borrar.modelos(updateData)

      
      if(ncol(updateData$originales) <= 1) {
        showNotification(
          "ERROR: Check Separators", duration = 10, type = "error")
        updateData$originales <- NULL
        updateData$datos      <- NULL
        datos                 <<- NULL
      } else {
        updateData$datos <- updateData$originales
        datos <- updateData$originales
      }
    }, error = function(e) {
      updateData$datos      <- NULL
      updateData$originales <- NULL
      datos                 <- NULL
      showNotification(paste0("ERROR al cargar datos: ", e), type = "error")
    })
    
    close.menu("parte1", is.null(updateData$datos))
    close.menu("parte2", is.null(updateData$datos.aprendizaje))
  })
  
  # Transform Button Function
  observeEvent(input$transButton, {
    datos <- updateData$originales
    cod = ""
    borrar.modelos(updateData)
    close.menu("parte2", is.null(updateData$datos.aprendizaje))
    
    updateAceEditor(session, "fieldCodeTrans", value = cod)
    for (var in colnames(datos)) {
      if(!input[[paste0("del", var)]]) {
        datos[, var] <- NULL
        cod <- paste0(cod, "datos[['", var, "']] <- NULL\n")
        
      } else {
        if(input[[paste0("sel", var)]] == "categorico" &
           class(datos[, var]) %in% c("numeric","integer")) {
          datos[, var] <- as.factor(datos[, var])
          cod <- paste0(cod, code.trans(var, "categorico"))
        }
        if(input[[paste0("sel", var)]] == "numerico" &
           !(class(datos[, var]) %in% c("numeric","integer"))) {
          datos[, var] <- as.numeric(datos[, var])
          cod <- paste0(cod, code.trans(var, "numerico"))
        }
        if(input[[paste0("sel", var)]] == "disyuntivo") {
          datos <- datos.disyuntivos(datos, var)
          datos[, var] <- NULL
          cod <- paste0(cod, code.trans(var, "disyuntivo"))
        }
      }
    }
    
    updateAceEditor(session, "fieldCodeTrans", value = cod)
    updateData$datos <- datos
  }) 
  
  # Segment Button Function
  observeEvent(input$segmentButton, {
    
    for(modelName in names(modelos)){
      modelos[[modelName]] <- NULL
    }
    porcentaje       <- isolate(input$segmentacionDatosA)
    variable         <- isolate(input$sel.predic.var)
    semilla          <- isolate(input$semilla)
    permitir.semilla <- isolate(input$permitir.semilla)
    tryCatch({
      if(variable != ""){
        updateData$variable.predecir <-  variable
        datos                        <-  updateData$datos
        
        codigo.editor <- code.segment(porcentaje,
                                      variable,
                                      semilla,
                                      permitir.semilla)
        updateAceEditor(session, "fieldCodeSegment", value = codigo.editor)
        
        res <- segmentar.datos(datos,porcentaje,semilla,permitir.semilla)
        updateData$datos.prueba      <-  res$test
        updateData$datos.aprendizaje <-  res$train
        updateData$summary.var.pred <- summary_indices(res$test[,updateData$variable.predecir])
      }
    }, error = function(e) {
      borrar.modelos(updateData)
      showNotification(paste0("ERROR al segmentar los datos: ", e), type = "error")
    })
  })
  
  # Update data on table
  output$tabladatos <- DT::renderDataTable({
    datos  <- updateData$datos
    tipos  <- c(
      tr("numerico",   isolate(updateData$idioma)),
      tr("categorico", isolate(updateData$idioma))
    )
    
    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        options = list(dom = 'frtip', scrollY = "40vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
      return(NULL)
    })
  }, server = T)
  
  
  # Update Transform Table
  output$transData = renderUI({
    datos  <- updateData$originales
    idioma <- updateData$idioma
    
    res <- list(fluidRow(
      column(4, tags$span(tags$b("Variable"))),
      column(5, tags$b(tr("tipo", idioma))),
      column(3, tags$b(tr("activa", idioma))),
    ), hr(style = paste0("margin-top: 10px; margin-bottom: 10px;", 
                         "border-top: 1px solid black;")))
    
    if(!is.null(datos) && ncol(datos) > 0) {
      res <- list(res, lapply(colnames(datos), function(x) {
        list(fluidRow(
          column(4, tags$span(x)),
          column(5, selectInputTrans(datos, x, session, idioma)),
          column(3, tags$input(type = "checkbox", id = ns(paste0("del", x)), 
                               checked = T))
        ), hr(style = "margin-top: 10px; margin-bottom: 10px"))
      }))
    }
    
    res <- tags$div(
      style = "height: 40vh; overflow-y: scroll;",
      do.call(tagList, res)
    )
    return(res)
  })
  

  # Update testing data on table
  output$tablaPrueba <- DT::renderDataTable({
    datos  <- updateData$datos.prueba
    tipos  <- c(
      tr("numerico",   isolate(updateData$idioma)),
      tr("categorico", isolate(updateData$idioma))
    )
    
    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        options = list(dom = 'frtip', scrollY = "40vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
      return(NULL)
    })
  }, server = T)
  
  
  # Update training data on table
  output$tablaAprendizaje <- DT::renderDataTable({
    datos  <- updateData$datos.aprendizaje
    tipos  <- c(
      tr("numerico",   isolate(updateData$idioma)),
      tr("categorico", isolate(updateData$idioma))
    )
    
    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", tipos[1], "</span>"),
               paste0("<span data-id='categorico'>", tipos[2], "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) 
            tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        options = list(dom = 'frtip', scrollY = "40vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR al mostrar datos: ", e), type = "error")
      return(NULL)
    })
  }, server = T)
  


  # Cuando cambia la barra de proporcion de datos de prueba (Segmentar Datos)
  observeEvent(input$segmentacionDatosA, {
    updateSliderInput(session, "segmentacionDatosP", value = 100 - input$segmentacionDatosA)
  })
  
  # Cuando cambia la barra de proporcion de datos de aprendizaje (Segmentar Datos)
  observeEvent(input$segmentacionDatosP, {
    updateSliderInput(session, "segmentacionDatosA", value = 100 - input$segmentacionDatosP)
  })
  
  # Habilitada o deshabilitada la semilla
  observeEvent(input$permitir.semilla, {
    if (input$permitir.semilla) {
      shinyjs::enable("semilla")
    } else {
      shinyjs::disable("semilla")
    }
  })
  
  # Update Predict Variable
  observeEvent(updateData$datos, {
    datos <- updateData$datos
    updateSelectInput(session, "sel.predic.var", choices = rev(colnames.empty(var.numericas(datos))))
  })
  
}
    
## To be copied in the UI
# mod_carga_datos_ui("carga_datos_ui_1")
    
## To be copied in the server
# callModule(mod_carga_datos_server, "carga_datos_ui_1")
 
