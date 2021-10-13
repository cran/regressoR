#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`.
#' DO NOT REMOVE.
#' @import shiny
#' @import rlang
#' @import gbm
#' @import kknn
#' @import e1071
#' @import rpart
#' @import glmnet
#' @import shinyAce
#' @import rpart.plot
#' @import stats
#' @import echarts4r
#' @import shinycustomloader
#' @import htmltools
#' @importFrom utils head read.table write.csv
#' @importFrom grDevices adjustcolor hcl
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader dashboardSidebar
#' @importFrom graphics hist abline lines pairs par points polygon rect smoothScatter strwidth text
#' @importFrom DT tableHeader formatStyle
#' @importFrom randomForest randomForest
#' @importFrom pls pcr plsr MSEP RMSEP explvar R2 mvrValstats
#' @importFrom neuralnet neuralnet compute
#' @rawNamespace import(shinydashboard, except = c(dashboardHeader,dashboardPage,dashboardSidebar))
#' @rawNamespace import(shinyjs, except = c(runExample,colourInput))
#' @keywords internal
#' @noRd
app_ui <- function(request) {
  
  # MENU --------------------------------------------------------------------------------------------------------------------
  load.menu <- menuItem(labelInput("data"), tabName = "cargar", icon = icon("database"))
  
  
  statistics.menu <- menuItem(labelInput("basico"), tabName = "parte1", icon = icon("th-list"),
                              menuSubItem(labelInput("resumen"), tabName = "resumen", icon = icon("sort-numeric-down")),
                              menuSubItem(labelInput("normalidad"), tabName = "normalidad", icon = icon("chart-bar")),
                              menuSubItem(labelInput("dispersion"), tabName = "dispersion", icon = icon("chart-line")),
                              menuSubItem(labelInput("distribucion"), tabName = "distribucion", icon = icon("chart-area")),
                              menuSubItem(labelInput("correlacion"), tabName = "correlacion", icon = icon("table")),
                              menuItem(labelInput("poderpred"), tabName = "poderPred", icon = icon("rocket")))
  
  supervised.learning.menu    <- menuItem(labelInput("aprendizaje"), tabName = "parte2", icon = icon("th-list"),
                                          menuSubItem(labelInput("rl"),tabName = "rl",icon = icon("chart-line")),
                                          menuSubItem(labelInput("rlr"),tabName = "rlr",icon = icon("chart-line")),
                                          menuSubItem(labelInput("dt"),tabName = "dt",icon = icon("tree")),
                                          menuSubItem(labelInput("rf"),tabName = "rf",icon = icon("sitemap")),
                                          menuSubItem(labelInput("boost"),tabName = "boosting",icon = icon("superscript")),
                                          menuSubItem(labelInput("knn"),tabName = "knn",icon = icon("dot-circle")),
                                          menuSubItem(labelInput("svm"),tabName = "svm",icon = icon("chart-line")),
                                          menuSubItem(labelInput("rd"), tabName = "rd",icon = icon("chart-pie")),
                                          menuSubItem(labelInput("nn"),tabName = "nn",icon = icon("brain")))
  
  compare.menu <- menuItem(labelInput("comparacion"), tabName = "comparar", icon = icon("eye"))
  
  new.prediction.menu <- menuItem(labelInput("predicnuevos"), tabName = "predNuevos", icon = icon("table"))
  
  info.menu <- menuItem(labelInput("acercade"), tabName = "acercaDe", icon = icon("info"))
  
  menu.language <- tags$li(class = "nodisabled treeview",
                           tags$a(href = "#shiny-tab-tabdioma",
                                  tags$i(class="fa fa-cog"),
                                  labelInput("confg"),
                                  tags$i(class="fa fa-angle-left pull-right")),
                           tags$ul(class="treeview-menu", style="display: none;", `data-expanded`="Idioma",
                                   radioButtons('idioma', labelInput("selidioma"), 
                                                choiceNames = c(tr('espanol', 'es'), 'English'),
                                                choiceValues = c("es", "en")),
                                   tags$br(),
                                   numericInput("decimals_confg", label = labelInput("ndec"),
                                                min = 0, max = 20, value = 2,step = 1,width = "50%"),
                                   tags$br()))
  
  #Los sliderInput y colourpicker por un motivo imprevisto se tienen que inicializar
  #De lo contrario no se van a mostrar en algunas partes de la interfaz
  init.inputs <- tags$div(style = "display:none;",
                          sliderInput(inputId = "aux", min = 2, value = 2,
                                      label = "Cantidad de Clusters", max = 10),
                          colourInput(
                            "auxColor", NULL, value = "red", allowTransparent = T))
  
  # The side menu
  mi.menu <- sidebarMenu(id = "principal",
                         tags$div(id = "espacioMenu"),
                         load.menu,
                         statistics.menu,
                         supervised.learning.menu,
                         compare.menu,
                         new.prediction.menu,
                         info.menu,
                         hr(),
                         menu.language,
                         init.inputs)
  
  
  # HEAD HTML ---------------------------------------------------------------------------------------------------------------
  
  #Imports .css and .js, also decide the icon
  mi.head <- tags$head(
    tags$link(rel="icon", href="https://www.promidat.org/theme/image.php/formal_white/theme/1438713216/favicon"),
    useShinyjs())
  
  #The loading page generating model
  load.page <- conditionalPanel(condition="($('html').hasClass('shiny-busy')) && generating_model == true",
                                div(id = "loaderWrapper", div(id="loaderModel")))
  
  
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      title="PROMiDAT - RegressoR",
      shinydashboardPlus::dashboardHeader(
        controlbarIcon = icon("cogs"),
        title = HTML(paste0(
          '<span class = "logo-lg">
            <a href = "https://promidat.com" target = "_blank">
              <img src = "img/Logo.png" width = "100%" style = "padding-top:2px; padding-bottom:6px;">
            </a>
          </span>',
          '<img src= "img/logo_small.png" height = 50%, width = "120%">'
        ))
      ),
      dashboardSidebar(mi.menu),
      dashboardBody(mi.head,
                    load.page,
                    tabItems(
                      tabItem(tabName = "cargar",  mod_carga_datos_ui("carga_datos_ui_1")),
                      tabItem(tabName = "resumen",  mod_r_numerico_ui("r_numerico_ui_1")),
                      tabItem(tabName = "normalidad",  mod_normal_ui("normal_ui_1")),
                      tabItem(tabName = "dispersion",  mod_dispersion_ui("dispersion_ui_1")),
                      tabItem(tabName = "distribucion",  mod_distribuciones_ui("distribuciones_ui_1")),
                      tabItem(tabName = "correlacion",  mod_correlacion_ui("correlacion_ui_1")),
                      tabItem(tabName = "poderPred",  mod_Predictive_Power_ui("Predictive_Power_ui_1")),
                      tabItem(tabName = "rl",  mod_linear_regression_ui("linear_regression_ui_1")),
                      tabItem(tabName = "rlr",  mod_penalized_Regression_ui("penalized_Regression_ui_1")),
                      tabItem(tabName = "dt",  mod_regression_trees_ui("regression_trees_ui_1")),
                      tabItem(tabName = "rf",  mod_random_forests_ui("random_forests_ui_1")),
                      tabItem(tabName = "boosting",  mod_boosting_ui("boosting_ui_1")),
                      tabItem(tabName = "knn",  mod_KNN_ui("KNN_ui_1")),
                      tabItem(tabName = "svm",  mod_SVM_ui("SVM_ui_1")),
                      tabItem(tabName = "rd",  mod_dimension_reduction_ui("dimension_reduction_ui_1")),
                      tabItem(tabName = "nn",  mod_neural_networks_ui("neural_networks_ui_1")),
                      tabItem(tabName = "comparar",  mod_model_comparison_ui("model_comparison_ui_1")),
                      tabItem(tabName = "predNuevos",  mod_new_data_predictions_ui("new_data_predictions_ui_1")),
                      tabItem(tabName = "acercaDe",  mod_information_page_ui("information_page_ui_1"))
                    ))
    )
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path('www', app_sys('app/www'))
  add_resource_path('img', app_sys('app/img'))
  add_resource_path('lang', app_sys('app/lang'))
  
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'RegressoR'
    ),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
  
}

