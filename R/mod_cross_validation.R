#' cross_validation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cross_validation_ui <- function(id){
  ns <- NS(id)
  title_comp <- list(conditionalPanel("input['cross_validation_ui_1-BoxCV'] == 'tabcvcvIndicesCat'",
                                      div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                                          tags$div(class="multiple-select-var",
                                                   selectInput(inputId = ns("cv.cat.sel"),label = NULL,
                                                               choices =  "", width = "100%")))),
                     conditionalPanel("input['cross_validation_ui_1-BoxCV'] == 'tabcvcvIndices3'",
                                      div(id = ns("row2"), shiny::h5(style = "float:left;margin-top: 15px;margin-right: 10px;", labelInput("selectCat"),class = "wrapper-tag"),
                                          tags$div(class="multiple-select-var",
                                                   selectInput(inputId = ns("cvcv_glo"),label = NULL,
                                                               choices = "")))))
  
  opc_knn <- list(div(col_6(numericInput(ns("kmax.knn"), labelInput("kv"), min = 1,step = 1, value = 7)),
                      col_6(selectInput(inputId = ns("kernel.knn.pred"), label = labelInput("selkernel"), selected = 1,
                                        choices = c("optimal", "rectangular", "triangular", "epanechnikov", 
                                                    "biweight", "triweight", "cos","inv","gaussian"))),
                      col_6(radioSwitchNP(ns("switch.scale.knn.pred"), "escal", c("si", "no"))),
                      col_6(numericInput(ns("distance_cvknn"), labelInput("distknn"), min = 1,step = 1, value = 2))))
  
  opc_svm <- list(div(col_6(radioSwitchNP(ns("switch.scale.svm.pred"), "escal", c("si", "no"))),
                      col_6(selectInput(inputId = ns("kernel.svm.pred"), label = labelInput("selkernel"),selected = "radial",
                                        choices = c("linear", "polynomial", "radial", "sigmoid")))))
  
  opc_rf  <- list(div(col_4(numericInput(ns("ntree.rf.pred"), labelInput("numTree"), 20, width = "100%", min = 0)),
                      col_4(numericInput(ns("mtry.rf.pred"),  labelInput("numVars"),1, width = "100%", min = 1)),
                      col_4(selectInput(inputId = ns("split.rf.pred"), label = labelInput("splitIndex"),selected = 1,
                                        choices =  list("gini" = "gini", "Entropia" = "information")))))
  
  opc_dt  <- list(div(col_4(numericInput(ns("minsplit.dt.pred"), labelInput("minsplit"), 20, width = "100%",min = 1)),
                      col_4(numericInput(ns("maxdepth.dt.pred"), labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1)),
                      col_4(selectInput(inputId = ns("split.dt.pred"), label = labelInput("splitIndex"),selected = 1,
                                        choices =  list("gini" = "gini", "Entropia" = "information")))))

  opc_potenciacion <- list(div(col_6(numericInput(ns("iter.boosting.pred"), labelInput("numTree"), 20, width = "100%",min = 1)),
                               col_6(numericInput(ns("maxdepth.boosting.pred"),labelInput("maxdepth"), 15, width = "100%",min = 1)),
                               col_6(numericInput(ns("minsplit.boosting.pred"),labelInput("minsplit"), 20, width = "100%",min = 1)),
                               col_6(selectInput(inputId = ns("coeflearn"), label = labelInput("selkernel"), selected = 1,
                                                 choices = c("gaussian", "laplace", "tdist")))))
  opc_rl  <- list(div())
  
  opc_rlr <- list(div(col_6(radioSwitchNP(ns("switch.scale.rlr.pred"), "escal", c("si", "no"))),
                      col_6(selectInput(inputId = ns("alpha.rlr.pred"),  label = labelInput("selectAlg"),selected = 1,
                                        choices = list("Ridge" = 0, "Lasso" = 1)))))
  
  opc_rd <- list(div(col_6(radioSwitchNP(ns("switch.scale.rd.pred"), "escal", c("si", "no"))),
                      col_6(selectInput(inputId = ns("kernel.rd.pred"), label = labelInput("selectAlg"),selected = 1,
                                        choices = list("ACP" = 0, "MCP" = 1)))))
  
  opc_nn <- list(div(col_4(numericInput(ns("threshold.nn"),labelInput("threshold"),
                                        min = 0,   step = 0.01, value = 0.05)),
                     col_4(numericInput(ns("stepmax_nn"),labelInput("stepmax"),
                                        min = 100, step = 100,  value = 10000)),
                     col_4(sliderInput(inputId = ns("cant.capas.nn.pred"), min = 1, max = 10,
                                       label = labelInput("selectCapas"), value = 3))),
                 div(id = ns("capasFila"),lapply(1:10, function(i) tags$span(
                   col_2(numericInput(ns(paste0("nn.cap.pred.",i)), NULL, min = 1, step = 1, value = 3),
                         class = "mini-numeric-select")))))
  
  tagList(
    tabBoxPrmdt(
      id = ns("BoxCV"), 
      tabPanel(title = p(labelInput("seleModel"),class = "wrapper-tag"), value = "tabCVsvmSModelo",
               div(
                 col_12(selectInput(inputId = ns("predic_var"), label = labelInput("seleccionarPredecir"), choices =  "", width = "100%"))               ),
               div(
                 col_12(checkboxGroupInput(inputId = ns("sel_models"), label = labelInput("seleModel"), choices =    c("knnl", "dtl", "rfl", "bl", "svml", "rl", "rlr", "rd"), width = "100%"))
               ),br(),br()),
      tabPanel(title = p(labelInput("seleParModel"),class = "wrapper-tag"), value = "tabCVsvmModelo",
               div(
                 col_12(selectInput(inputId = ns("sel_methods"), label = labelInput("selectMod"),
                                    choices =  "", width = "100%"))
               )
               , hr(style = "border-top: 2px solid #cccccc;" ),
               conditionalPanel(condition =  "input.sel_methods == 'knnl'",
                                opc_knn, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'svml'",
                                opc_svm, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'dtl'",
                                opc_dt, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'rfl'",
                                opc_rf, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'bl'",
                                opc_potenciacion, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'nn'",
                                opc_nn, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'rl'",
                                opc_rl, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'rlr'",
                                opc_rlr, ns = ns),
               conditionalPanel(condition =  "input.sel_methods == 'rd'",
                                opc_rd, ns = ns),
               hr(style = "border-top: 2px solid #cccccc;" ),
               actionButton(ns("btn_cv"), labelInput("generar"), width  = "100%" ),br(),br(),
               div(id = ns("texto"),
                   style = "display:block",withLoader(verbatimTextOutput(ns("txt_cv")), 
                                                      type = "html", loader = "loader4")),br(),br()),
      tabPanel(title = p(labelInput("indices"),class = "wrapper-tag"), value = "tabcvcvIndices3",
               div(col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("selectInd"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("cvcv_glo"),label = NULL,
                                                  choices =  "", width = "100%")))),
                   col_4(),
                   col_4(div(id = ns("row"), shiny::h5(style = "float:left;margin-top: 15px;", labelInput("tipoGrafico"),class = "wrapper-tag"),
                             tags$div(class="multiple-select-var",
                                      selectInput(inputId = ns("plot_type_p"),label = NULL,
                                                  choices =  "", width = "100%"))))),hr(),
               div(col_12(echarts4rOutput(ns("e_cv_ind"), width = "100%", height = "70vh")))),
      tabPanel(title = p(labelInput("tablaComp"),class = "wrapper-tag"),
               withLoader(DT::dataTableOutput(ns("TablaComp"), height="70vh"), 
                          type = "html", loader = "loader4"))
    )
    
  )
}


#' cross_validation Server Functions
#'
#' @noRd 
mod_cross_validation_server <- function(input, output, session, updateData, codedioma){
  ns <- session$ns
  
  M <- rv(MCs.cv = NULL, grafico = NULL, global = NULL, categories = NULL, times = 0)
  
  
  observeEvent(input$sel_models, {
    nombres        <- input$sel_models
    names(nombres) <- tr(nombres,codedioma$idioma)
    updateSelectInput(session, "sel_methods", choices = nombres, selected = nombres[1])
  })
  
  observeEvent(codedioma$idioma, {
    
    nombres <- list("knnl", "dtl", "rfl", "bl", "svml" , "rl", "rlr", "rd")
    names(nombres) <- tr(c("knnl", "dtl", "rfl", "bl", "svml", "rl", "rlr", "rd"),codedioma$idioma)
    
    modelos <- input$sel_models
    indices <- list(0, 1, 2, 3)
    names(indices) <- tr(c("RMSE", "MAE", "ER", "correlacion"),codedioma$idioma) 
    nombres_p <- list("barras", "lineas", "error")
    names(nombres_p) <- tr(c("grafBarras", "grafLineas", "grafError"),codedioma$idioma)
    
    updateSelectInput(session, "cvcv_glo", choices = indices, selected = 0)
    updateSelectInput(session, "plot_type_p", choices = nombres_p, selected = "barras")
    updateCheckboxGroupInput(session, "sel_models", choices = nombres, selected = modelos)
    updateSelectInput(session, "sel_methods", choices = tr(modelos, codedioma$idioma), selected = modelos[1])
  })
  
  observeEvent(c(updateData$datos, updateData$variable.predecir), {
    datos    <- updateData$datos
    variable <- updateData$variable.predecir
    M$MCs.cv  <- NULL
    M$grafico <- NULL
    M$global  <- NULL
    M$categories <- NULL
    M$times      <- 0
    defaul_param_values()
    if(!is.null(datos)){
      choices      <- as.character(unique(datos[, variable]))
      updateTextInput(session, "txt_cv", value = ' ' )
      updateSelectInput(session, "predic_var", choices = rev(colnames.empty(var.numericas(updateData$datos)))) # Variables categóricas 
    }
    
    output$txt_cv <- renderPrint({
      return(invisible(''))
    })
  })
  
  observeEvent(input$btn_cv, {
    output$txt_cv <- renderPrint({
      tryCatch({
        cant.vc   <- isolate(updateData$numValC)
        datos     <- isolate(updateData$datos)
        numGrupos <- isolate(updateData$numGrupos)
        grupos    <- isolate(updateData$grupos)
        variable  <- isolate(updateData$variable.predecir)
        var_      <- as.formula(paste0(variable, "~."))
        params    <- listar_parametros()
        models    <- isolate(input$sel_models)
        nombres   <- vector(mode = "character", length = length(models))
        MCs.cv    <- vector(mode = "list")
        
        if(length(models)<1){
          if(M$times != 0)
            showNotification("Debe seleccionar al menos un model")
        }
        
        for (model in 1:length(models)){
          MCs.cv[[paste0("MCs.",models[model])]] <- vector(mode = "list", length = cant.vc)
          nombres[model] <- paste0("MC.",models[model])
        }
        nombres <- nombres
        MCs.cv  <- MCs.cv
        for (i in 1:cant.vc){
          MC.cv <- vector(mode = "list", length = length(models))
          names(MC.cv) <- nombres
          for (model in 1:length(models)){
            MC.cv[[model]] <- vector(mode = "list", 4)
            names(MC.cv[[model]]) <- c("Raiz.Error.Cuadratico", "Error.Absoluto", "Error.Relativo", "Correlacion")
            
          }
      
        for (k in 1:numGrupos){
          muestra   <- grupos[[i]][[k]]
          ttraining <- datos[-muestra, ]
          ttesting  <- datos[muestra, ]
      
          for (j in 1:length(models)){
            modelo      <- switch (models[j],
                                   "knnl"  = {
                                     train.knn(var_,
                                               data  = ttraining,
                                               scale = as.logical(params$scal_kn),
                                               kernel = params$kernel_kn,
                                               kmax   = params$k_kn)},
                                   "svml"  = {
                                     train.svm(var_,
                                               data = ttraining,
                                               scale  = as.logical(params$scal_svm),
                                               kernel = params$kernel_svm)},
                                   "dtl"   = {
                                     train.rpart(var_,
                                                 data    = ttraining,
                                                 control = rpart.control(minsplit = params$minsplit_dt,
                                                                         maxdepth = params$maxdepth_dt),
                                                 parms   = list(split = params$tipo_dt))},
                                   "rfl"   = {
                                     train.randomForest(var_,
                                                        data  = ttraining,
                                                        mtry  = params$mtry,
                                                        ntree = params$ntree,
                                                        importance = TRUE,
                                                        parms   = list(split = params$tipo_rf))},
                                   "bl"    = {
                                     train.gbm(var_,
                                               data         = ttraining, 
                                               distribution = params$coeflearn_b, 
                                               n.trees      = params$iter, 
                                               interaction.depth = params$maxdepth_b , 
                                               n.minobsinnode    = params$minsplit_b)},
      
                                   "rl"    = {
                                     lm(as.formula(var_), 
                                        data   = ttraining)},
                                   "rlr"   = {
                                     rlr_model(data  = ttraining, 
                                               alpha = params$alpha, 
                                               variable.pred = variable,
                                               standardize   = as.logical(params$scal_rlr))
                                   },
                                   "rd"   = {
                                     rd_model(data = ttraining, variable.pred = variable,
                                              mode = params$kernel_rd, scale = as.logical(params$scal_rd))
                                     
                                   }
            )
            if(models[j] %not_in% c("rl", "rlr", "rd")){
              prediccion  <- predict(modelo, ttesting)$prediction
            }else{
                if(models[j] == "rl"){
                  prediccion  <- predict(modelo, ttesting)
                }
                if(models[j] == "rlr"){
                  prediccion  <- rlr_prediction(modelo, 
                                                ttesting, 
                                                variable)
                }
                if(models[j] == "rd"){
                  prediccion  <- rd_prediction(modelo, ttesting, params$ncomp_rd)
                }
              }
              
              MC          <- general_indices(ttesting[,variable], prediccion)
              MC.cv[[j]]  <- Map(c, MC.cv[[j]], MC)
          }
        }
      
        for (l in 1:length(MCs.cv)){
          MCs.cv[[l]][[i]] <-  sapply(MC.cv[[l]],mean)
        }
      }

        M$MCs.cv   <- MCs.cv
        resultados <- indices.cv( cant.vc, models, MCs.cv)
        M$grafico  <- resultados$grafico
        M$ea   <- resultados$ea
        M$er   <- resultados$er
        M$corr <- resultados$corr
        M$summary <- summary_indices_v(datos[[variable]])
        isolate(codedioma$code <- append(codedioma$code, cv_cv_code(variable, cant.vc, numGrupos)))
        print(MCs.cv)
        
        return(invisible(''))
        
      },error = function(e){
        M$MCs.cv  <- NULL
        M$grafico <- NULL
        M$ea   <- NULL
        M$er   <- NULL
        M$corr <- NULL
        M$times    <- 0 
        return(e)
        #return(invisible(''))
      })
    })
  })
 
  output$e_cv_ind  <-  renderEcharts4r({
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
                "barras" = return( resumen.barras(grafico, labels = c(label,tr(c("modelo","maximo","minimo", "q1", "q3"), idioma)) ,percent = p, vals = M$summary)),
                "error" = return( resumen.error(grafico, labels = c(label, tr(c("modelo","maximo","minimo", "q1", "q3"), idioma)),percent = p, vals = M$summary)),
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
  
  
  # Update Comparison Table
  output$TablaComp <- DT::renderDataTable({
    res      <- data.frame()
    idioma   <- codedioma$idioma
    global   <<- M$grafico
    tryCatch({
      global$name <-  tr(global$name,codedioma$idioma)
      for (i in 1:nrow(global)) {
        new <- data.frame(
          RMSE = global[i,"value"],
          MAE = M$ea[i], 
          ER = M$er[i]*100, 
          correlacion = M$corr[i]
        )
        
        row.names(new) <- global[i,"name"]
        res            <- rbind(res, new)
        
      }
      
      colnames(res)              <- tr(colnames(res), idioma)
      res[]                      <- lapply(res, as.numeric)
      res                        <- round(res, 5) 
      DT::datatable(res, selection = "none", editable = FALSE,
                    options = list(dom = "frtip", pageLength = 10, buttons = NULL))
    }, error = function(e) {
      showNotification(e, duration = 10)
      DT::datatable(data.frame(), selection = "none", editable = FALSE,
                    options = list(dom = "frtip", pageLength = 10, buttons = NULL))
    })
  },server = FALSE)
  
  
  #Actualiza la cantidad de capas ocultas (neuralnet)
  observeEvent(input$cant.capas.nn.pred, {
    if(!is.null(input$cant.capas.nn.pred)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn.pred) {
          shinyjs::show(paste0("nn.cap.pred.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.pred.", i))
        }
      }
    }
  })
  
  listar_parametros <- function(){
    isolate({
      k_kn         <-  input$kmax.knn 
      scal_kn      <-  input$switch.scale.knn.pred 
      kernel_kn    <-  input$kernel.knn.pred 
      tipo_dt      <-  input$split.dt.pred 
      minsplit_dt  <-  input$minsplit.dt.pred 
      maxdepth_dt  <-  input$maxdepth.dt.pred 
      mtry         <-  input$mtry.rf.pred 
      ntree        <-  input$ntree.rf.pred 
      tipo_rf      <-  input$split.rf.pred 
      scal_svm     <-  input$switch.scale.svm.pred 
      kernel_svm   <-  input$kernel.svm.pred 
      tipo_xgb     <-  input$boosterXgb.pred 
      maxdepth_xgb <-  input$maxdepthXgb 
      n.rounds     <-  input$nroundsXgb 
      threshold    <-  input$threshold.nn 
      stepmax      <-  input$stepmax_nn 
      capas.np     <- c(input$nn.cap.pred.1 , input$nn.cap.pred.2 ,
                        input$nn.cap.pred.3 , input$nn.cap.pred.4 ,
                        input$nn.cap.pred.5 , input$nn.cap.pred.6 ,
                        input$nn.cap.pred.7 , input$nn.cap.pred.8 ,
                        input$nn.cap.pred.9 , input$nn.cap.pred.10 )
      cant.capas   <-  input$cant.capas.nn.pred 
      capas.np     <-  as.vector(as.numeric(capas.np[1:cant.capas] ))
      scal_rlr     <-  input$switch.scale.rlr.pred 
      scal_rd      <-  input$switch.scale.rd.pred 
      alpha        <-  input$alpha.rlr.pred 
      kernel_rd    <-  input$kernel.rd.pred 
      iter         <-  input$iter.boosting.pred 
      maxdepth_b   <-  input$maxdepth.boosting.pred 
      minsplit_b   <-  input$minsplit.boosting.pred
      coeflearn_b  <-  input$coeflearn
      ncomp_rd     <-  sqrt(ncol(isolate(updateData$datos)))
    })
    return(list(k_kn        = k_kn,       scal_kn     = scal_kn,     kernel_kn    = kernel_kn, 
                tipo_dt     = tipo_dt,    minsplit_dt = minsplit_dt, maxdepth_dt  = maxdepth_dt, 
                mtry        = mtry,       ntree       = ntree,       scal_svm     = scal_svm, 
                kernel_svm  = kernel_svm, tipo_xgb    = tipo_xgb,    maxdepth_xgb = maxdepth_xgb,  
                n.rounds    = n.rounds,   threshold   = threshold,   stepmax      = stepmax, 
                capas.np    = capas.np,   scal_rlr    = scal_rlr,    alpha        = alpha, 
                iter        = iter,       maxdepth_b  = maxdepth_b,  minsplit_b   = minsplit_b, 
                coeflearn_b = coeflearn_b,scal_rd     = scal_rd,     kernel_rd    = kernel_rd, 
                ncomp_rd    = ncomp_rd,   tipo_rf     = tipo_rf))
  }
  
  
  defaul_param_values <- function(){
    updateSliderInput(session, "cant.capas.nn.pred", value = 3)
  }
}

## To be copied in the UI
# mod_cross_validation_ui("cross_validation_1")

## To be copied in the server
# mod_cross_validation_server("cross_validation_1")
