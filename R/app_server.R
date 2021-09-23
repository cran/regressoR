#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' 
app_server <- function( input, output, session ) {

  options(shiny.maxRequestSize = 209715200, width = 200, # 209715200 = 200 * 1024^2
          DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10, scrollX = TRUE, 
                            language = list(search = HTML('<i class="fa fa-search"></i>'),
                                            info = "", emptyTable = "", zeroRecords = "",
                                            paginate = list("previous" = HTML('<i class="fa fa-backward"></i>'),
                                                            "next" = HTML('<i class="fa fa-forward"></i>'),
                                                            "first" =HTML('<i class="fa fa-fast-backward"></i>'),
                                                            "last" = HTML('<i class="fa fa-fast-forward"></i>')) )))
  
  
  # REACTIVE VALUES -------------------------------------------------------------------------------------------------------
  #updateData always has the same values of the global variables(datos, datos.prueba, datos.aprendizaje).
  updateData <- reactiveValues(originales = NULL, datos = NULL, 
                               datos.prueba = NULL, datos.aprendizaje = NULL, 
                               variable.predecir = NULL, summary.var.pred = NULL,
                               idioma = "es", decimals = 2)
  
  modelos    <-  reactiveValues(rl = NULL, rlr= NULL, dt = NULL, 
                                rf = NULL, boost = NULL, knn = NULL, 
                                svm = NULL, rd = NULL, nn = NULL)
  
  new.data <- reactiveValues(originales.train = NULL, datos.train = NULL, variable.predecir = NULL,
                             nuevos = NULL, modelo = NULL, prediccion = NULL)
  
  
  
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
    })
  })
  
  
  # CHANGE LANGUAGE -------------------------------------------------------------------------------------------------------
  
  # When the user changes the language
  observeEvent(input$idioma, {
    if(updateData$idioma != input$idioma){
      updateData$idioma <- input$idioma
    }
    updateLabelInput(session, cambiar.labels(), tr(cambiar.labels(), input$idioma))
  })
  
  observeEvent(input$decimals_confg,{
    n <- input$decimals_confg
    if(is.numeric(n)){
      if(n >= 0 & n <= 20){
        updateData$decimals <- n
      }
      else{
        updateNumericInput(session,inputId = "decimals_confg",value = 2)
        updateData$decimals <- 2
      }
    }
    else{
      updateData$decimals <- 2
    }
  })
  
  
  
  # END THE SESSION -------------------------------------------------------------------------------------------------------
  
  # When the session closes
  onStop(function(){
    stopApp()
  })
  
  
  ###################################  Modules  ###############################
  callModule(mod_carga_datos_server,"carga_datos_ui_1",updateData, modelos)
  callModule(mod_r_numerico_server, "r_numerico_ui_1",updateData)
  callModule(mod_normal_server, "normal_ui_1",updateData)
  callModule(mod_dispersion_server, "dispersion_ui_1", updateData)
  callModule(mod_distribuciones_server, "distribuciones_ui_1", updateData)
  callModule(mod_correlacion_server, "correlacion_ui_1", updateData)
  callModule(mod_Predictive_Power_server, "Predictive_Power_ui_1", updateData)
  callModule(mod_linear_regression_server, "linear_regression_ui_1",updateData,modelos)
  callModule(mod_penalized_Regression_server, "penalized_Regression_ui_1",updateData, modelos)
  callModule(mod_regression_trees_server, "regression_trees_ui_1",updateData, modelos)
  callModule(mod_random_forests_server, "random_forests_ui_1",updateData, modelos)
  callModule(mod_boosting_server, "boosting_ui_1",updateData, modelos)
  callModule(mod_KNN_server, "KNN_ui_1",updateData, modelos)
  callModule(mod_SVM_server, "SVM_ui_1",updateData, modelos)
  callModule(mod_dimension_reduction_server, "dimension_reduction_ui_1",updateData, modelos)
  callModule(mod_neural_networks_server, "neural_networks_ui_1",updateData, modelos)
  callModule(mod_model_comparison_server, "model_comparison_ui_1",updateData,modelos)
  callModule(mod_new_data_predictions_server, "new_data_predictions_ui_1", updateData, new.data)
  callModule(mod_information_page_server, "information_page_ui_1")
}
