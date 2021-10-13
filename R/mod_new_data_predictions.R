#' new_data_predictions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList 
mod_new_data_predictions_ui <- function(id){
  
  ns <- NS(id)
  
  btn_style <- "width: 100%;background-color: #3d8dbc;color: white;"
  btn_style_hidden <- "width: 100%;background-color: #3d8dbc;color: white; display:none;"
  
  
  # Data display
  
  show.data1 <- box(title = labelInput("data"), status = "primary", width = 12,
                    solidHeader = TRUE, collapsible = TRUE,
                    withLoader(DT::dataTableOutput(ns('tabladatos1')),type = "html", loader = "loader4"))
  
  show.data2 <- box(title = labelInput("data"), status = "primary", width = 12,
                    solidHeader = TRUE, collapsible = TRUE,
                    withLoader(DT::dataTableOutput(ns('tabladatos2')),type = "html", loader = "loader4"))
  
  show.data3 <- box(title = labelInput("data"), status = "primary", width = 12,
                    solidHeader = TRUE, collapsible = TRUE,
                    withLoader(DT::dataTableOutput(ns('tabladatos3')),type = "html", loader = "loader4"))
  
  # Loading and transforming data
  
  data.upload.panel.pred <- div(id = ns("seccion1"),
                                fluidRow(
                                  column(width = 11,
                                         tabBox(width = 12,
                                                tabPanel(title =labelInput("cargarDatos"), 
                                                         fluidRow(
                                                           column(width = 5,
                                                                  checkboxInput(ns('headerNPred'), labelInput("header"), TRUE),
                                                                  checkboxInput(ns('rownameNPred'), labelInput("Rownames"), TRUE),
                                                                  radioButtons(ns('sepNPred'), labelInput("separador"), inline = T, 
                                                                               choiceValues = c(';', ',', '\t'), choiceNames = c(';', ',', 'TAB')),
                                                                  radioButtons(ns('decNPred'),labelInput("separadordec"), inline = T,
                                                                               choiceValues = c(',', '.'), choiceNames = c(',', '.')),
                                                                  radioSwitch(ns("deleteNAnPred"), "eliminana", c("eliminar", "imputar")),
                                                                  fileInput(ns('file2'), label = labelInput("cargarchivo"), placeholder = "", 
                                                                            buttonLabel =  labelInput("subir"), width = "100%",
                                                                            accept = c('text/csv', '.csv')),
                                                                  actionButton(ns("loadButtonNPred"), labelInput("cargar"), width = "100%")),
                                                           column(width = 7, show.data1))))),
                                  column(width = 1, actionButton(inputId = ns("btn_next1"),
                                                                 label = NULL, icon = icon("forward"), style = btn_style_hidden) )))
  
  
  transform.data.panel <-   div(id = ns("seccion2"), style= "display:none;",fluidRow(
    column(width = 1, actionButton(inputId = ns("btn_prev1"),
                                   label = NULL, icon = icon("backward"), style = btn_style) ),
    column(width = 10,tabBox(
      width = 12,
      tabPanel(title = labelInput("transDatos"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
               fluidRow(column(width = 5,
                               uiOutput(ns('transData1')), hr(),
                               actionButton(ns('transButton1'), labelInput("aplicar"), width = "100%")),
                        column(width = 7, show.data2))))),
    
    column(width = 1, actionButton(inputId = ns("btn_next2"),
                                   label = NULL, icon = icon("forward"), style = btn_style) )))
  
  
  
  data.upload.panel.pred2 <- div(id = ns("seccion4"), style= "display:none;",fluidRow(
    column(width = 1, actionButton(inputId = ns("btn_prev3"),
                                   label = NULL, icon = icon("backward"), style = btn_style) ),
    column(width = 10,tabBox(
      width = 12,
      tabPanel(title = labelInput("cargarNuev"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
               fluidRow(column(width = 5,
                               checkboxInput(ns('headerNPred2'), labelInput("header"), TRUE),
                               checkboxInput(ns('rownameNPred2'),  labelInput("Rownames"), TRUE),
                               radioButtons(ns('sep.nPred2'), labelInput("separador"), inline = T, choiceValues = c(';', ',', '\t'), choiceNames = c(';', ',', 'TAB')),
                               radioButtons(ns('dec.nPred2'),labelInput("separadordec"), inline = T,choiceValues = c(',', '.'), choiceNames = c(',', '.')),
                               radioSwitch(ns("deleteNAnPred2"), "eliminana", c("eliminar", "imputar")),
                               fileInput(ns('file3'), label = labelInput("cargarchivo"), placeholder = "", buttonLabel = labelInput("subir"), width = "100%",
                                         accept = c('text/csv', '.csv')),
                               actionButton(ns("loadButtonNPred2"), labelInput("cargar"), width = "100%")),
                        column(width = 7, show.data3))))),
    column(width = 1, actionButton(inputId = ns("btn_next4"),
                                   label = NULL, icon = icon("forward"), style = btn_style_hidden) )))
  
  # Model Options
  
  options.rl.pred <- list() # Vacio
  
  options.rlr.pred <- fluidRow(column(selectInput(inputId = ns("alpha.rlr"), label = labelInput("selectAlg"),selected = 1,
                                                  choices = list("Ridge" = 0, "Lasso" = 1)),width = 3),
                               column(width = 3,radioSwitch(id = ns("switch_scale_rlr"), label = "escal", 
                                                            names = c("si", "no"))),
                               column(id = ns("colManualLanda"),width = 3, numericInput(ns("log_landa"), labelInput("log_landa"),value = 2, min = 0, "NULL", width = "100%")),
                               column(width = 3, radioSwitch(id = ns("permitir_landa"), label = "",
                                                             names = c("manual", "automatico"), val.def = FALSE)))
  
  options.dt.pred <- fluidRow(column(width = 6, numericInput(ns("minsplit_dt"), labelInput("minsplit"), 20, width = "50%",min = 1)),
                              column(width = 6, numericInput(ns("maxdepth_dt"), labelInput("maxdepth"), 15, width = "50%",min = 0, max = 30, step = 1)))
  
  
  options.rf.pred <- fluidRow(column(width = 6, numericInput(ns("ntree_rf"), labelInput("numTree"), 100, width = "50%", min = 1)),
                              column(width = 6, numericInput(ns("mtry_rf"),labelInput("numVars"),1, width = "50%", min = 1)))
  
  
  options.boosting.pred <- list(fluidRow(column(width = 4, numericInput(ns("iter_boosting"), labelInput("numTree"), 200, width = "75%",min = 1)),
                                         column(width = 4, numericInput(ns("shrinkage_boosting"),labelInput("shrinkage"), 0.1, width = "75%",min = 0.0001)),
                                         column(width = 4, selectInput(inputId = ns("tipo_boosting"), label = labelInput("selectAlg"),selected = 1, width = "75%",
                                                                       choices =  c("gaussian", "laplace", "tdist")))))
  
  
  options.knn.pred <- fluidRow(column(width = 3,radioSwitch(id = ns("switch_scale_knn"), label = "escal",
                                                            names = c("si", "no"))),
                               column(width = 3, numericInput(ns("k_knn"), labelInput("kv"), min = 1,step = 1, value = 7,width="100%")),
                               column(width = 3, selectInput(inputId = ns("kernel_knn"), label = labelInput("selkernel") ,selected = 1, width="100%",
                                                             choices =  c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                                          "triweight", "cos","inv","gaussian"))),
                               column(width = 3,numericInput(ns("distance_knn"), labelInput("distknn"), min = 1,step = 1, value = 2) ))
  
  
  options.svm.pred <- fluidRow(column(width = 3, radioSwitch(id = ns("switch_scale_svm"), label = "escal",
                                                             names = c("si", "no"))),
                               column(width = 3),
                               column(width = 6, selectInput(inputId = ns("kernel_svm"), label = labelInput("selkernel"), selected = "radial", width="50%",
                                                             choices =  c("linear", "polynomial", "radial", "sigmoid"))))
  
  
  options.rd.pred <-  fluidRow(column(selectInput(inputId = ns("mode_rd"), label = labelInput("selectAlg"),selected = 0,
                                                  choices = list("ACP" = 0, "MCP" = 1)),width = 3),
                               column(width= 3, radioSwitch(id = ns("switch_scale_rd"), label = "escal",names = c("si", "no"))),
                               column(id = ns("colManualCom"),width = 3, numericInput(ns("ncomp_rd"), labelInput("ncomp"),value = 2, min = 0, "NULL", width = "100%")),
                               column(width = 3, radioSwitch(id = ns("permitir_ncomp"), label = "",
                                                             names = c("manual", "automatico"), val.def = FALSE)))
  
  
  
  options.nn.pred <-list(fluidRow(column(numericInput(ns("threshold_nn"),labelInput("threshold"),
                                                      min = 0, step = 0.01, value = 0.1), width = 3),
                                  column(numericInput(ns("stepmax_nn"),labelInput("stepmax"),
                                                      min = 100, step = 100, value = 5000), width = 3),
                                  column(sliderInput(inputId = ns("cant_capas_nn"), min = 1, max = 10,
                                                     label = labelInput("selectCapas"), value = 5), width = 5)),
                         fluidRow(lapply(1:10, function(i) tags$span(numericInput(ns(paste0("nn_cap_",i)), NULL,
                                                                                  min = 1, step = 1, value = 2),
                                                                     class = "mini-numeric-select"))))
  
  options.model <- list(selectInput(inputId = ns("sel.predic.var.nuevos"), label = labelInput("seleccionarPredecir"), choices =  "", width = "100%"),
                        radioGroupMulti(id = ns("selectModelsPred"), label = "selectMod",
                                        c("rl","rlr","dt","rf","boost","knn","svm","rd","nn")))
  
  tabs.models  <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(40),
                              tabs.content = list(list(codigo.monokai(ns("fieldPredNuevos"), height = "7vh"))))
  
  
  create.pred.model.panel <- div(id = ns("seccion3"), style= "display:none;",fluidRow(
    column(width = 1, actionButton(inputId = ns("btn_prev2"),
                                   label = NULL, icon = icon("backward"), style = btn_style) ),
    column(width = 10,tabBoxPrmdt(id = ns("BoxModel_New_Data"), opciones = tabs.models,
                             tabPanel(title = labelInput("seleParModel"),solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE, value = "crearModelo",
                                      options.model,
                                      conditionalPanel(condition =  "input.selectModelsPred == 'rl'",
                                                       options.rl.pred, ns = ns),
                                      conditionalPanel(condition =  "input.selectModelsPred == 'rlr'",
                                                       options.rlr.pred, ns = ns),
                                      conditionalPanel(condition =  "input.selectModelsPred == 'knn'",
                                                       options.knn.pred, ns = ns),
                                      conditionalPanel(condition =  "input.selectModelsPred == 'dt'",
                                                       options.dt.pred, ns = ns),
                                      conditionalPanel(condition =  "input.selectModelsPred == 'rf'",
                                                       options.rf.pred, ns = ns),
                                      conditionalPanel(condition =  "input.selectModelsPred == 'boost'",
                                                       options.boosting.pred, ns = ns),
                                      conditionalPanel(condition =  "input.selectModelsPred == 'svm'",
                                                       options.svm.pred, ns = ns),
                                      conditionalPanel(condition =  "input.selectModelsPred == 'nn'",
                                                       options.nn.pred, ns = ns),
                                      conditionalPanel(condition =  "input.selectModelsPred == 'rd'",
                                                       options.rd.pred, ns = ns),
                                      withLoader(verbatimTextOutput(ns("txtPredNuevos")), type = "html", loader = "loader4"),
                                      actionButton(ns("PredNuevosBttnModelo"), labelInput("generarM"), 
                                                   width  = "100%", style = "background-color:#CBB051;color:#fff;margin-top:9px;")))),
    column(width = 1, actionButton(inputId = ns("btn_next3"),
                                   label = NULL, icon = icon("forward"), style = btn_style_hidden) )
  ))
  
  
  
  
  tabs.models2  <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(40),
                               tabs.content = list(codigo.monokai(ns("fieldCodePredPN"), height = "7vh")))
  
  prediccion.pred.panel <- div(id = ns("seccion5"), style= "display:none;",fluidRow(
    column(width = 1, actionButton(inputId = ns("btn_prev4"),
                                   label = NULL, icon = icon("backward"), style = btn_style) ),
    column(width = 11,tabBoxPrmdt(id = ns("BoxPrediccion_New_Data"), opciones = tabs.models2,
                                  tabPanel(title = labelInput("predicnuevos"), value = "predicModelo",
                                           DT::dataTableOutput(ns("PrediTablePN")),
                                           hr(),
                                           downloadButton(ns("downloaDatosPred"), labelInput("descargar"), style = "width:100%"),
                                           actionButton(ns("predecirPromidat"), "preditc",style="display:none;"))
    ))))
  
  
  
  tagList(
    data.upload.panel.pred,
    transform.data.panel,
    create.pred.model.panel,
    data.upload.panel.pred2,
    prediccion.pred.panel
  )
}

#' new_data_predictions Server Function
#' @keywords internal
#' 
mod_new_data_predictions_server <- function(input, output, session, updateData, new.data){
  ns <- session$ns
  
  observeEvent(input$btn_next1,{
    shinyjs::hide("seccion1",anim = T)
    shinyjs::show("seccion2",anim = T)
  })
  
  observeEvent(input$btn_prev1,{
    shinyjs::show("seccion1",anim = T)
    shinyjs::hide("seccion2",anim = T)
  })
  
  observeEvent(input$btn_next2,{
    shinyjs::hide("seccion2",anim = T)
    shinyjs::show("seccion3",anim = T)
  })
  
  observeEvent(input$btn_prev2,{
    shinyjs::show("seccion2",anim = T)
    shinyjs::hide("seccion3",anim = T)
  })
  
  observeEvent(input$btn_next3,{
    shinyjs::hide("seccion3",anim = T)
    shinyjs::show("seccion4",anim = T)
  })
  
  observeEvent(input$btn_prev3,{
    shinyjs::show("seccion3",anim = T)
    shinyjs::hide("seccion4",anim = T)
  })
  
  observeEvent(input$btn_next4,{
    shinyjs::hide("seccion4",anim = T)
    shinyjs::show("seccion5",anim = T)
    #Realozar prediccion
    shinyjs::click("predecirPromidat")
  })
  
  observeEvent(input$btn_prev4,{
    shinyjs::show("seccion4",anim = T)
    shinyjs::hide("seccion5",anim = T)
  })
  
  
  default.values.inputs <- function(){
    
    #----------------rlr----------------
    updateSelectInput(session, "alpha.rlr",selected = 1)
    updateRadioSwitch(session,"switch_scale_rlr","TRUE")
    updateNumericInput(session,"log_landa",value = 2)
    updateRadioSwitch(session,"permitir_landa","FALSE")
    
    #----------------dt----------------
    updateNumericInput(session,inputId = "minsplit_dt", value = 20)
    updateNumericInput(session,inputId = "maxdepth_dt", value = 15)
    
    #----------------rf----------------
    updateNumericInput(session = session, inputId = "ntree_rf", value = 100)
    updateNumericInput(session,"mtry_rf",value = 1)
    
    #----------------boosting----------------
    updateSelectInput(session,inputId = "tipo_boosting", selected = "gaussian")
    updateNumericInput(session, inputId = "iter_boosting", value = 200)
    updateNumericInput(session, inputId = "shrinkage_boosting", value = 0.1)
    
    #---------------knn-----------------
    updateNumericInput(session, "k_knn", value = 7)
    updateSelectInput(session, "kernel_knn",selected = "optimal")
    updateRadioSwitch(session,"switch_scale_knn","TRUE")
    updateNumericInput(session, "distance_knn", value = 2)
    
    #---------------svm-----------------
    updateRadioSwitch(session,"switch_scale_svm","TRUE")
    updateSelectInput(session,"kernel_svm",selected = "radial")
    
    #---------------rd------------------
    updateSelectInput(session,"mode_rd",selected = 0)
    updateRadioSwitch(session,"switch_scale_rd","TRUE")
    updateNumericInput(session,"ncomp_rd", value = 2)
    updateRadioSwitch(session,"permitir_ncomp","FALSE")
    
    #---------------nn------------------
    updateSliderInput(session, "cant_capas_nn", value = 2)
    updateNumericInput(session, "threshold_nn", value = 0.1)
    updateNumericInput(session, "stepmax_nn", value = 5000)
    update_nn_layers_pn()
    
    
    isolate(datos <- new.data$datos.train)
    if(!is.null(datos)){
      updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames.empty(var.numericas(datos))))
      updateNumericInput(session, "k_knn", value = round(sqrt(nrow(datos))))
      updateNumericInput(session, "mtry_rf", value = round(sqrt(ncol(datos) -1)))
    }
    
  }
  
  
  # Updates neural network layers of new individuals
  update_nn_layers_pn <- function(){
    if(!is.null(input$cant_capas_nn)){
      for (i in 1:10) {
        if(i <= input$cant_capas_nn) {
          shinyjs::show(paste0("nn_cap_", i))
          updateNumericInput(session, paste0("nn_cap_", i), value = 2)
        } else {
          shinyjs::hide(paste0("nn_cap_", i))
        }
      }
    }
  }
  
  # When the number of neural network layers changes.
  observeEvent(c(input$cant_capas_nn), {
    update_nn_layers_pn()
  })
  
  
  
  # Download the data with the prediction
  output$downloaDatosPred <- downloadHandler(
    filename = function() {
      input$file3$name
    },
    content = function(file) {
      isolate({
        prediccion <- new.data$prediccion
        datos.nuevos.pred <- new.data$nuevos
        datos.nuevos.pred[, new.data$variable.predecir] <- as.vector(prediccion)
      })
      if(!is.null(prediccion)){
        write.csv(datos.nuevos.pred, file, row.names = input$rownameNPred2)
      }
    }
  )
  
  
  # Habilitada o deshabilitada el número de componenetes 
  observeEvent(input$permitir_ncomp, {
    if (as.logical(input$permitir_ncomp)) {
      shinyjs::enable("ncomp_rd")
    } else {
      shinyjs::disable("ncomp_rd")
    }
  })
  
  # When user press enable or disable the lambda
  observeEvent(input$permitir_landa, {
    if (as.logical(input$permitir_landa)) {
      shinyjs::enable("log_landa")
    } else {
      shinyjs::disable("log_landa")
    }
  })
  
  reset.data <- function(){
    isolate({
      new.data$originales.train <- NULL
      new.data$datos.train      <- NULL
      new.data$variable.predecir <- NULL
      new.data$nuevos <- NULL
      new.data$modelo <- NULL
      new.data$prediccion <- NULL
    })
  }
  
  
  reset.next.btns <- function(){
    shinyjs::hide("btn_next3",anim = TRUE)
    shinyjs::hide("btn_next4",anim = TRUE)
  }
  
  
  # Load Button Function 1
  observeEvent(input$loadButtonNPred, {
    tryCatch({
      shinyjs::hide("btn_next1",anim = TRUE)
      
      reset.data()
      
      rowname    <- input$rownameNPred
      ruta       <- input$file2
      sep        <- input$sepNPred
      dec        <- input$decNPred
      encabezado <- input$headerNPred
      deleteNA   <- input$deleteNAnPred
      
      new.data$originales.train <- carga.datos(
        rowname, ruta$datapath, sep, dec, encabezado, deleteNA)
      
      if(ncol(new.data$originales.train) <= 1) {
        showNotification("ERROR: Check Separators", duration = 10, type = "error")
        new.data$originales.train <- NULL
      } else {
        #Todo correcto
        new.data$datos.train <- new.data$originales.train
        default.values.inputs()
        reset.next.btns()
        shinyjs::show("btn_next1",anim = TRUE)
      }
    }, error = function(e) {
      reset.data()
      showNotification(paste0(tr("errorSeg"), e), type = "error")
    })
    
  },ignoreInit = TRUE)
  
  
  updateDataTable1_2 <- reactive({
    datos  <- new.data$datos.train
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
  })
  
  
  # Update data on table1
  output$tabladatos1 <- DT::renderDataTable({
    #Se necesita por problema con la reactividad
    input$btn_prev1
    updateDataTable1_2()
  }, server = T)
  
  # Update data on table2
  output$tabladatos2 <- DT::renderDataTable({
    input$btn_next1
    updateDataTable1_2()
  }, server = T)
  
  
  # Update Transform Table1
  output$transData1 <- renderUI({
    datos  <- new.data$originales.train
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
  
  
  #Función para realizar la transformación de datos
  transformar.datos <- function(datos){
    cod = ""
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
    
    return(datos)
  }
  
  
  # Transform Button Function
  observeEvent(input$transButton1, {
    
    datos  <- new.data$originales.train
    
    datos <- transformar.datos(datos)
    
    new.data$modelo <- NULL
    new.data$nuevos <- NULL
    new.data$prediccion <- NULL
    new.data$datos.train <- datos
    default.values.inputs()
    reset.next.btns()
  }, ignoreInit = TRUE)
  
  
  
  #Update model tab
  output$txtPredNuevos <- renderPrint({
    tryCatch({
      if(!is.null(new.data$modelo)){
        print(new.data$modelo)
      }
      else{
        cat(tr("noModel", updateData$idioma))
        updateAceEditor(session, "fieldPredNuevos", value = "")
      }
    }, error = function(e){
      showNotification(paste0("Error: ", e), duration = 10, type = "error")
      NULL
    } )
  })
  
  
  # When the user presses the generate model button
  observeEvent(input$PredNuevosBttnModelo,{
    
    tryCatch({
      shinyjs::runjs(code = "generating_model = true")
      shinyjs::hide(id = "btn_next3", anim = T)
      new.data$modelo <- NULL
      new.data$prediccion <- NULL
      
      variable.predecir <- input$sel.predic.var.nuevos
      new.data$variable.predecir <- variable.predecir
      datos.aprendizaje <- new.data$datos.train
      modelo.seleccionado  <- input$selectModelsPred
      
      gen.code <- ""
      
      new.data$modelo <- switch(modelo.seleccionado,
                                rl   = {
                                  gen.code <- codeRl(variable.predecir)
                                  rl_model(datos.aprendizaje,variable.predecir)
                                },
                                rlr  = {
                                  alpha <- as.numeric(input$alpha.rlr)
                                  standardize <- as.logical(input$switch_scale_rlr)
                                  gen.code <- codeRlr(variable.predecir,alpha,standardize)
                                  rlr_model(data = datos.aprendizaje, variable.pred = variable.predecir,
                                            alpha = alpha, standardize = standardize)
                                },
                                
                                dt  = {
                                  gen.code <- codeDt(variable.predecir,input$minsplit_dt,input$maxdepth_dt)
                                  dt_model(datos.aprendizaje, variable.predecir,
                                           minsplit = input$minsplit_dt,
                                           maxdepth = input$maxdepth_dt)
                                },
                                
                                rf  = {
                                  ntree <- input$ntree_rf
                                  mtry <- input$mtry_rf
                                  #Validacion tamaño del mtry
                                  tam <- ncol(datos.aprendizaje)
                                  if(mtry >= tam){
                                    mtry <- tam - 1
                                    updateNumericInput(session, "mtry_rf", value = mtry)
                                  }
                                  
                                  gen.code <- codeRf(variable.predecir,ntree, mtry)
                                  rf_model(datos.aprendizaje, variable.predecir, ntree, mtry)
                                },
                                
                                knn =  {
                                  scale <- as.logical(input$switch_scale_knn)
                                  kernel <- input$kernel_knn
                                  k <- input$k_knn
                                  distance <- input$distance_knn
                                  #Validacion tamaño del k
                                  tam <- nrow(datos.aprendizaje)
                                  if(k >= tam){
                                    k <- tam - 2
                                    updateNumericInput(session, "k_knn", value = k)
                                  }
                                  
                                  gen.code <- codeKnn(variable.predecir, scale, k, kernel,distance)
                                  kkn_model(datos.aprendizaje,variable.predecir, scale, k, kernel, distance)
                                },
                                
                                boost = {
                                  if(!is.null(calibrate_boosting(datos.aprendizaje))){
                                    n.trees <- input$iter_boosting
                                    distribution <- input$tipo_boosting
                                    shrinkage <- input$shrinkage_boosting
                                    gen.code <- codeBoost(variable.predecir, n.trees, distribution, shrinkage)
                                    boosting_model(datos.aprendizaje,variable.predecir, n.trees, distribution, shrinkage)
                                  }
                                  else{
                                    showNotification(tr("ErrorBsize"), duration = 10, type = "error")
                                    NULL
                                  }
                                },
                                svm = {
                                  scale <- as.logical(input$switch_scale_svm)
                                  kernel <- input$kernel_svm
                                  gen.code <- codeSvm(variable.predecir,scale,kernel)
                                  svm_model(datos.aprendizaje,variable.predecir, scale, kernel)
                                },
                                
                                rd = {
                                  scale <- as.logical(input$switch_scale_rd)
                                  modo.rd <- input$mode_rd
                                  gen.code <- codeRd(variable.predecir,modo.rd, scale)
                                  modelo.rd <- rd_model(datos.aprendizaje,variable.predecir, modo.rd, scale)
                                  ncomp <- NULL
                                  if (as.logical(input$permitir_ncomp) && !is.na(input$ncomp_rd)) {
                                    if(input$ncomp_rd >= 1 && input$ncomp_rd <= ncol(datos.aprendizaje)){
                                      ncomp <- input$ncomp_rd
                                    }
                                  }
                                  if(is.null(ncomp)){
                                    ncomp <- modelo.rd$optimal.n.comp
                                    updateNumericInput(session,"ncomp_rd", value = ncomp)
                                  }
                                  modelo.rd$ncomp_rd <- ncomp
                                  modelo.rd
                                },
                                nn = {
                                  threshold <- input$threshold_nn
                                  stepmax <- input$stepmax_nn
                                  cant.capas <- input$cant_capas_nn
                                  threshold <- ifelse(threshold == 0, 0.01, threshold)
                                  stepmax <- ifelse(stepmax < 100, 100, stepmax)
                                  hidden <- c(input$nn_cap_1,input$nn_cap_2,input$nn_cap_3,input$nn_cap_4,
                                              input$nn_cap_5,input$nn_cap_6,input$nn_cap_7,input$nn_cap_8,
                                              input$nn_cap_9,input$nn_cap_10)
                                  hidden <- hidden[1:cant.capas]
                                  gen.code <- codeNn(variable.predecir, hidden, threshold, stepmax)
                                  nn_model(datos.aprendizaje,variable.predecir, hidden, threshold, stepmax)
                                })
      
      
      updateAceEditor(session, "fieldPredNuevos", value = gen.code)
      
      shinyjs::show(id = "btn_next3", anim = T)
    },
    error =  function(e){
      shinyjs::hide(id = "btn_next3", anim = T)
      showNotification(paste0("Error: ", e), duration = 10, type = "error")
    },
    warning = function(w){
      if(input$selectModelsPred == "nn"){
        shinyjs::hide(id = "btn_next3", anim = T)
        showNotification(paste0(tr("nnWar")," (NN-01) : ",w), duration = 10, type = "warning")
      }
    },
    finally = {
      shinyjs::runjs(code = "generating_model = false")
    })
  }, ignoreInit = TRUE)
  
  
  
  # Load Button Function 2
  observeEvent(input$loadButtonNPred2, {
    tryCatch({
      
      new.data$prediccion <- NULL
      shinyjs::hide("btn_next4",anim = TRUE)
      
      rowname    <- input$rownameNPred2
      ruta       <- input$file3
      sep        <- input$sep.nPred2
      dec        <- input$dec.nPred2
      encabezado <- input$headerNPred2
      deleteNA   <- input$deleteNAnPred2
      
      new.data$nuevos <- carga.datos.np(
        rowname, ruta$datapath, sep, dec, encabezado)
      
      #Ignorar la variable a predecir a la hora de eliminar los NA
      new.data$nuevos[,new.data$variable.predecir] <- NULL
      new.data$nuevos <- accion.NAs(new.data$nuevos, deleteNA)
      new.data$nuevos[,new.data$variable.predecir] <- NA
      
      #Actualiza los datos según la configuración anterior
      new.data$nuevos <- transformar.datos(new.data$nuevos)
      
      if(ncol(new.data$nuevos) <= 1) {
        showNotification("ERROR: Check Separators", duration = 10, type = "error")
        new.data$nuevos <- NULL
      } else {
        #Todo correcto
        shinyjs::show("btn_next4",anim = TRUE)
      }
    }, error = function(e) {
      showNotification(paste0(tr("errorSeg"), e), type = "error")
    })
    
  },ignoreInit = TRUE)
  
  
  
  # Update data on table3
  output$tabladatos3 <- DT::renderDataTable({
    datos  <- new.data$nuevos
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
  
  
  
  #Update prediction tab
  output$PrediTablePN <- DT::renderDataTable({
    tryCatch({
      isolate({
        datos.nuevos.pred <- new.data$nuevos
        variable.predecir <- new.data$variable.predecir
      })
      prediccion <- new.data$prediccion
      if(!is.null(prediccion)){
        prediccion <- round(prediccion,updateData$decimals)
        #Eliminamos la antigua columna de la variable a predecir
        nombres <- colnames(datos.nuevos.pred)
        datos.nuevos.pred <- datos.nuevos.pred[!nombres %in% variable.predecir]
        
        #Colocamos de ultimo la columna con las predicciones
        datos.nuevos.pred[, variable.predecir] <- as.vector(prediccion)
        
        dttable.custom(datos.nuevos.pred, decimals = NULL)
      }
      else{
        updateAceEditor(session, "fieldCodePredPN", value = "")
        DT::datatable(data.frame())
      }
    }, error = function(e){
      showNotification(paste0("Error :", e), duration = 10, type = "error")
      DT::datatable(data.frame())
    })
  }, server = FALSE)
  
  
  # When the user enters the prediction panel
  observeEvent(input$predecirPromidat, {
    
    tryCatch({
      
      datos.prueba <- new.data$nuevos
      
      if(!is.null(new.data$nuevos)){
        modelo <- new.data$modelo
        
        if(!is.null(modelo)){
          
          variable.predecir <- input$sel.predic.var.nuevos
          new.data$variable.predecir <- variable.predecir
          modelo.seleccionado  <- input$selectModelsPred
          pred.code <- ""
          
          new.data$prediccion <- NULL
          new.data$prediccion <- switch(modelo.seleccionado,
                                        
                                        rl  =  {
                                          pred.code <- codeRlPred()
                                          rl_prediction(modelo, datos.prueba)
                                        },
                                        
                                        rlr =  {
                                          if (as.logical(input$permitir_landa) && !is.na(input$log_landa)) {
                                            log.landa <- input$log_landa
                                          }
                                          else{log.landa <- NULL}
                                          pred.code <- codeRlrPred("rlr.model", variable.predecir,log.landa)
                                          rlr_prediction(modelo, datos.prueba, variable.predecir,log.lambda = log.landa)
                                        },
                                        dt  = {
                                          pred.code <- codeDtPred()
                                          dt_prediction(modelo,datos.prueba)
                                        },
                                        
                                        
                                        rf  = {
                                          pred.code <- codeRfPred()
                                          rf_prediction(modelo, datos.prueba)
                                        },
                                        
                                        knn =  {
                                          pred.code <- codeKnnPred()
                                          kkn_prediction(modelo, datos.prueba)
                                        },
                                        
                                        boost = {
                                          pred.code <- codeBoostPred("boosting.model", input$iter_boosting)
                                          boosting_prediction(modelo, datos.prueba, input$iter_boosting)
                                        },
                                        svm = {
                                          pred.code <- codeSvmPred()
                                          svm_prediction(modelo, datos.prueba)
                                        },
                                        
                                        rd  =  {
                                          pred.code <- codeRdPred("rd.model",modelo$ncomp_rd)
                                          rd_prediction(modelo,datos.prueba, modelo$ncomp_rd)
                                        },
                                        nn = {
                                          pred.code <- codeNnPred()
                                          nn_prediction(modelo, datos.prueba)
                                        })
          
          updateAceEditor(session, "fieldCodePredPN", value = pred.code)
          
        }else{
          showNotification(paste0("Error :", tr("ErrorModelo")), duration = 10, type = "error")
        }
      }else{
        showNotification(paste0("Error :", tr("ErrorDatosPN")), duration = 10, type = "error")
      }
    },error =  function(e){
      showNotification(paste0("Error :", e), duration = 10, type = "error")
    })
  })
  
}

## To be copied in the UI
# mod_new_data_predictions_ui("new_data_predictions_ui_1")

## To be copied in the server
# callModule(mod_new_data_predictions_server, "new_data_predictions_ui_1")

