#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' 
app_server <- function( input, output, session ) {

  options(shiny.maxRequestSize = 209715200, width = 200, # 209715200 = 200 * 1024^2
          DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10, 
                            language = list(search = HTML('<i class="fa fa-search"></i>'),
                                            emptyTable = "", zeroRecords = "",
                                            paginate = list("previous" = HTML('<i class="fa fa-backward"></i>'),
                                                            "next" = HTML('<i class="fa fa-forward"></i>'),
                                                            "first" =HTML('<i class="fa fa-fast-backward"></i>'),
                                                            "last" = HTML('<i class="fa fa-fast-forward"></i>')) )))
  
  
  # REACTIVE VALUES -------------------------------------------------------------------------------------------------------
  eval(parse(text = "library('traineR')"))
  
  #updateData always has the same values of the global variables(datos, datos.prueba, datos.aprendizaje).
  updateData <- reactiveValues(originales   = NULL, datos = NULL, 
                               datos.prueba = NULL, datos.aprendizaje = NULL, 
                               variable.predecir = NULL, summary.var.pred = NULL, decimals = 2)
  
  codedioma <- reactiveValues(idioma = "es",
                              code   = list())
  
  modelos    <-  reactiveValues(rl  = NULL, rlr   = NULL, dt  = NULL, 
                                rf  = NULL, boost = NULL, knn = NULL, 
                                svm = NULL, rd    = NULL, nn  = NULL)
  
  modelos2    <-  rv(svm      = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     knn      = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     rd       = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     rl       = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     rlr      = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     boosting = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     rf       = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     nn       = list(n = 0, mcs = vector(mode = "list", length = 10)),
                     dt       = list(n = 0, mcs = vector(mode = "list", length = 10)))
  
  newCases   <-     rv(originales        = NULL, 
                       datos.prueba      = NULL, 
                       datos.aprendizaje = NULL,
                       m.seleccionado    = NULL,
                       modelo            = NULL,
                       prediccion        = NULL,
                       variable.predecir = NULL)
  
  updateData2 <- reactiveValues(originales   = NULL, datos = NULL, 
                               datos.prueba  = NULL, datos.aprendizaje = NULL, 
                               variable.predecir = NULL, decimals = 2)
  
  
  # Enable/disable on load data
  observe({
    if(is.null(updateData$datos) || ncol(updateData$datos) < 1) {
      addClass(class = "disabled", selector = 'a[href^="#shiny-tab-parte1"]')
      shinyjs::disable(selector = 'a[href^="#shiny-tab-parte1"]')
    }
    else{
      removeClass(class = "disabled", selector = 'a[href^="#shiny-tab-parte1"]')
      shinyjs::enable(selector = 'a[href^="#shiny-tab-parte1"]')
    }
    
    menu.selectors <- c('a[href^="#shiny-tab-parte2"]','a[href^="#shiny-tab-comparar"]',
                        'a[href^="#shiny-tab-poderPred"]')
    
    lapply(menu.selectors, function(i){
      if(is.null(updateData$datos.prueba) || ncol(updateData$datos.prueba) < 1) {
        addClass(class = "disabled", selector = i)
        shinyjs::disable(selector = i)
      } else {
        removeClass(class = "disabled", selector = i)
        shinyjs::enable(selector = i)
      }
      if(is.null(updateData$grupos) || (is.null(updateData$numValC) && updateData$numValC <= 1)) {
        shinyjs::disable(selector = 'a[href^="#shiny-tab-calibracion"]')
        shinyjs::disable(selector = 'a[href^="#shiny-tab-cv_cv"]')
      } else {
        shinyjs::enable(selector = 'a[href^="#shiny-tab-calibracion"]')
        shinyjs::enable(selector = 'a[href^="#shiny-tab-cv_cv"]')
        shinyjs::enable(selector = 'a[data-value=poderPred]')
      }
    })
  })
  
  
  # CHANGE LANGUAGE -------------------------------------------------------------------------------------------------------
  
  #' Update on Language
  observeEvent(input$idioma, {
    codedioma$idioma = input$idioma
    etiquetas <- names(translation)
    updateLabelInput(session, etiquetas, tr(etiquetas, input$idioma))
  })
  
  observeEvent(input$decimals_confg,{
    n <- input$decimals_confg
    if(is.numeric(n)){
      if(n >= 0 & n <= 20){
        updateData$decimals <- n
        updateData2$decimals <- n
      }
      else{
        updateNumericInput(session,inputId = "decimals_confg",value = 2)
        updateData$decimals <- 2
        updateData2$decimals <- 2
      }
    }
    else{
      updateData$decimals <- 2
      updateData2$decimals <- 2
    }
  })
  
  
  # Update Code
  observeEvent(c(codedioma$code, input$idioma), {
    codigo <- codedioma$code
    lg     <- input$idioma
    
    keys <- names(translation)
    
    for (k in keys) {
      codigo <- gsub(paste0(" ", k, "\n"), paste0(" ", tr(k, idioma = lg), "\n"), codigo, fixed = T)
    }
    
    codigo.completo <- paste0(
      "library(XLConnect)\n", "library(caret)\n",
      "library(traineR)\n", "library(glmnet)\n",
      "library(rpart.plot)\n", "library(htmltools)\n",
      "library(echarts4r)\n", "library(loadeR)\n\n"
    )
    for (cod in codigo) {
      codigo.completo <- paste0(codigo.completo, "\n", cod)
    }
    updateAceEditor(session, "fieldCode", value = codigo.completo)
  })
  
  output$btn_code <- downloadHandler(
    filename = "codigo.R",
    content = function(con) {
      write(input$fieldCode, con)
    }
  )
  
  # END THE SESSION -------------------------------------------------------------------------------------------------------
  
  # When the session closes
  onStop(function(){
    stopApp()
  })
  
  
  ###################################  Modules  ###############################
  #Carga de Datos
  loadeR::mod_carga_datos_server("carga_datos_ui_1", updateData,  modelos, codedioma, "regressoR")
  loadeR::mod_carga_datos_server("carga_datos_ui_2", updateData2, NULL,    codedioma, "discoveR")
  
  #Estadísticas Básicas
  loadeR::mod_r_numerico_server("r_numerico_ui_1",                 updateData, codedioma)
  loadeR::mod_normal_server("normal_ui_1",                         updateData, codedioma)
  loadeR::mod_dispersion_server("dispersion_ui_1",                 updateData, codedioma)
  loadeR::mod_distribuciones_server("distribuciones_ui_1",         updateData, codedioma)
  loadeR::mod_correlacion_server("correlacion_ui_1",               updateData, codedioma)
  callModule(mod_Predictive_Power_server, "Predictive_Power_ui_1", updateData, codedioma)
  
  # Aprendizaje Supervisado
  callModule(mod_l_regression_server,         "l_regression_ui_1",         updateData, modelos, codedioma, modelos2)
  callModule(mod_penalized_l_r_server,        "penalized_l_r_ui_1",        updateData, modelos, codedioma, modelos2)
  callModule(mod_regression_trees_server,     "regression_trees_ui_1",     updateData, modelos, codedioma, modelos2)
  callModule(mod_r_forest_server,             "r_forest_ui_1",             updateData, modelos, codedioma, modelos2)
  callModule(mod_boosting_server,             "boosting_ui_1",             updateData, modelos, codedioma, modelos2)
  callModule(mod_KNN_server,                  "KNN_ui_1",                  updateData, modelos, codedioma, modelos2)
  callModule(mod_SVM_server,                  "SVM_ui_1",                  updateData, modelos, codedioma, modelos2)
  callModule(mod_dimension_reduction_server,  "dimension_reduction_ui_1",  updateData, modelos, codedioma, modelos2)
  callModule(mod_neural_net_server,           "neural_net_ui_1",           updateData, modelos, codedioma, modelos2)

  # Comparación de Individuos
  callModule(mod_comparacion_server,     "comparacion_ui_1",     updateData, modelos, codedioma)
  callModule(mod_varerr_server,          "varerr_ui_1",          updateData, modelos, codedioma, modelos2)
  
  #Validación Cruzada
  callModule(mod_cv_knn_server,      "cv_knn_ui_1",      updateData, codedioma)
  callModule(mod_cv_svm_server,      "cv_svm_ui_1",      updateData, codedioma)
  callModule(mod_cv_dt_server,       "cv_dt_ui_1",       updateData, codedioma)
  callModule(mod_cv_rf_server,       "cv_rf_ui_1",       updateData, codedioma)
  callModule(mod_cv_boosting_server, "cv_boosting_ui_1", updateData, codedioma)
  callModule(mod_cv_rlr_server,      "cv_rlr_ui_1",      updateData, codedioma)
  callModule(mod_cv_rd_server,       "cv_rd_ui_1",       updateData, codedioma)
  callModule(mod_cv_rl_server,       "cv_rl_ui_1",       updateData, codedioma)
  callModule(mod_cross_validation_server,"cross_validation_ui_1", updateData, codedioma)
  
  # Predicción Ind. Nuevos
  callModule(mod_ind_nuevos_server, "ind_nuevos_ui_1", newCases, updateData2, codedioma)

  # About
  callModule(mod_information_page_server,     "information_page_ui_1", codedioma)
}
