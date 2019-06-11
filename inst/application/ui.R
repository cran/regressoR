
suppressMessages(suppressWarnings({
  library(regressoR)
  library(DT)
  library(gbm)
  library(kknn)
  library(shiny)
  library(e1071)
  library(rpart)
  library(knitr)
  library(glmnet)
  library(rattle)
  library(xtable)
  library(xgboost)
  library(shinyjs)
  library(ggplot2)
  library(stringr)
  library(forcats)
  library(shinyAce)
  library(corrplot)
  library(neuralnet)
  library(rpart.plot)
  library(randomForest)
  library(colourpicker)
  library(shinyWidgets)
  library(flexdashboard)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(dplyr)
  library(zip)
  library(pls)
}))

# Edit > Folding > Collapse All (is of much help to visualize in an orderly way the code).

#See the FULL PAGE section first

# These functions are not really useful for the user, so we keep them hidden so as not to confuse the user.
# These functions help us to build the ui.
labelInput <- regressoR:::labelInput
code_field <- regressoR:::code_field
infoBoxPROMiDAT <- regressoR:::infoBoxPROMiDAT
inputRadio <- regressoR:::inputRadio
radioButtonsTr <- regressoR:::radioButtonsTr
tabsOptions <- regressoR:::tabsOptions

# MENU --------------------------------------------------------------------------------------------------------------------

load.menu <- menuItem(labelInput("data"), tabName = "cargar", icon = icon("dashboard"))

statistics.menu <- menuItem(labelInput("basico"), tabName = "parte1", icon = icon("th-list"),
                              menuSubItem(labelInput("resumen"), tabName = "resumen", icon = icon("sort-numeric-asc")),
                              menuSubItem(labelInput("normalidad"), tabName = "normalidad", icon = icon("bar-chart")),
                              menuSubItem(labelInput("dispersion"), tabName = "dispersion", icon = icon("line-chart")),
                              menuSubItem(labelInput("distribucion"), tabName = "distribucion", icon = icon("area-chart")),
                              menuSubItem(labelInput("correlacion"), tabName = "correlacion", icon = icon("table")),
                              menuItem(labelInput("poderpred"), tabName = "poderPred", icon = icon("rocket")))

supervised.learning.menu    <- menuItem(labelInput("aprendizaje"), tabName = "parte2", icon = icon("th-list"),
                                         menuSubItem(labelInput("rll"),tabName = "rl",icon = icon("line-chart")),
                                         menuSubItem(labelInput("rlr"),tabName = "rlr",icon = icon("line-chart")),
                                         menuSubItem(labelInput("dtl"),tabName = "dt",icon = icon("tree")),
                                         menuSubItem(labelInput("rfl"),tabName = "rf",icon = icon("sitemap")),
                                         menuSubItem(labelInput("bl"),tabName = "boosting",icon = icon("superscript")),
                                         menuSubItem(labelInput("knnl"),tabName = "knn",icon = icon("dot-circle-o")),
                                         menuSubItem(labelInput("svml"),tabName = "svm",icon = icon("line-chart")),
                                         menuSubItem(labelInput("rd"), tabName = "rd",icon = icon("chart-pie")),
                                         menuSubItem(labelInput("nn"),tabName = "nn",icon = icon("brain")))

report.menu <- menuItem(labelInput("reporte"), tabName = "reporte", icon = icon("save-file",lib = "glyphicon"))

compare.menu <- menuItem(labelInput("comparacion"), tabName = "comparar", icon = icon("eye"))

new.prediction.menu <- menuItem(labelInput("predicnuevos"), tabName = "predNuevos", icon = icon("table"))

info.menu <- menuItem(labelInput("acercade"), tabName = "acercaDe", icon = icon("info"))

menu.language <- tags$li(class = "nodisabled treeview",
                       tags$a(href = "#shiny-tab-tabdioma",
                              tags$i(class="fa fa-language"),
                              labelInput("idioma"),
                              tags$i(class="fa fa-angle-left pull-right")),
                       tags$ul(class="treeview-menu", style="display: none;", `data-expanded`="Idioma",
                              radioButtons('idioma', labelInput("selidioma"), c('Español'='es', 'English'='en')),
                              tags$br()))

#Los sliderInput y colourpicker por un motivo imprevisto se tienen que inicializar
#De lo contrario no se van a mostrar en algunas partes de la interfaz
init.inputs <- tags$div(style = "display:none;",
                          sliderInput(inputId = "aux", min = 2, value = 2,
                                      label = "Cantidad de Clusters", max = 10),
                          colourpicker::colourInput(
                            "auxColor", NULL, value = "red", allowTransparent = T))

# The side menu
mi.menu <- sidebarMenu(id = "principal",
              tags$div(id = "espacioMenu"),
              load.menu,
              statistics.menu,
              supervised.learning.menu,
              compare.menu,
              new.prediction.menu,
              report.menu,
              info.menu,
              hr(),
              menu.language,
              init.inputs)

# HEAD HTML ---------------------------------------------------------------------------------------------------------------

#Imports .css and .js, also decide the icon
mi.head <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "style_regressor.css"),
  tags$link(rel="icon", href="http://www.promidat.org/theme/image.php/formal_white/theme/1438713216/favicon"),
  useShinyjs(),
  tags$script(src = "script_regressor.js"))

#The loading page
load.page <- conditionalPanel(condition="($('html').hasClass('shiny-busy'))",
                              div(id = "loaderWrapper", div(id="loader")))

# LOAD AND TRANSFORMATION PAGE---------------------------------------------------------------------------------------------

data.upload.panel <- tabPanel(title = labelInput("cargar"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                               checkboxInput('header', labelInput("header"), TRUE),
                               checkboxInput('rowname', labelInput("Rownames"), TRUE),
                               radioButtonsTr('sep', "separador", c(';', ',', '\t'), c("puntocoma", "coma", "tab")),
                               radioButtonsTr('dec', "separadordec", c(',', '.'), c("coma", "punto")),
                               switchInput(inputId = "deleteNA", onStatus = "success", offStatus = "danger", value = T, width = "100%",
                                           label = labelInput("eliminana"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"),
                               fileInput('file1', label =  labelInput("cargarchivo"), placeholder = "", buttonLabel =  labelInput("subir"), width = "100%",
                                         accept = c('text/csv', '.csv')),
                               actionButton("loadButton", labelInput("cargar"), width = "100%"),
                               br(),br(),
                               aceEditor("fieldCodeData", mode = "r", theme = "monokai", value = "", height = "13vh", readOnly = T))

tansform.data.panel <- tabPanel(title = labelInput("trans"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                   DT::dataTableOutput('transData'),
                                   br(),br(),
                                   actionButton("transButton", labelInput("aplicar"), width = "100%"),
                                   br(),br(),
                                   aceEditor("fieldCodeTrans", mode = "r", theme = "monokai", value = "", height = "10vh",  readOnly = T))

data.segment.panel <- tabPanel(title = labelInput("configuraciones"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                  fluidRow(column(id = "colSemilla",width = 6, numericInput("semilla", labelInput("semilla"), "NULL", width = "100%")), br(),
                                           column(width = 6, switchInput(inputId = "permitir.semilla", onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                                         label = "", onLabel = labelInput("habilitada"), offLabel = labelInput("deshabilitada"), labelWidth = "100%",
                                                                         inline = T,size = "large"))),
                                  selectInput(inputId = "sel.predic.var", label = labelInput("seleccionarPredecir"), choices =  "", width = "100%"),
                                  sliderInput("segmentacionDatosA", labelInput("propA"),width = "100%",
                                              min = 5, max = 95, value = 70, step = 5),
                                  sliderInput("segmentacionDatosT", labelInput("propP"), width = "100%",
                                              min = 5, max = 95, value = 30, step = 5),
                                  actionButton("segmentButton", labelInput("generar"), width = "100%"),
                                  br(),br(),
                                  aceEditor("fieldCodeSegment", mode = "r", theme = "monokai", value = "", height = "8vh",  readOnly = T))

show.data <- box(title = labelInput("data"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, type = 7, color = "#CBB051",
                     DT::DTOutput('contents'), hr(),
                     downloadButton("downloaDatos", labelInput("descargar"), width = "100%"))

show.learning.data <- box(title = labelInput("dataA"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, type = 7, color = "#CBB051",
                            DT::DTOutput('contentsAprend'), hr(),
                            downloadButton("downloaDatosA", labelInput("descargar"), width = "100%"))

show.test.data <- box(title = labelInput("dataP"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE, type = 7, color = "#CBB051",
                            DT::DTOutput('contentsPrueba'), hr(),
                            downloadButton("downloaDatosP", labelInput("descargar"), width = "100%"))

page.load.data <- tabItem(tabName = "cargar",
                               fluidRow(column(width = 5, tabBox(id ="tabs", title = NULL, width = 12,
                                                                 data.upload.panel,
                                                                 tansform.data.panel,
                                                                 data.segment.panel)),
                                        column(width = 7, show.data)),
                               conditionalPanel(condition = paste0("input.tabs == '", labelInput("configuraciones"),"'"),
                                                fluidRow(column(width = 6, show.learning.data),
                                                         column(width = 6, show.test.data))) )

# NUMERICAL SUMMARY PAGE --------------------------------------------------------------------------------------------------

full.summary.table <- box(title = labelInput("resumen"), status = "primary", width = 7, solidHeader = TRUE, collapsible = TRUE,
                               DT::dataTableOutput("resumen.completo"), hr(),
                               aceEditor("fieldCodeResum", mode = "r", theme = "monokai", value = "", height = "5vh",  readOnly = T))

variable.summary.table <- box(title = labelInput("resumenvar"), status = "primary", width = 5, solidHeader = TRUE, collapsible = TRUE,
                               selectInput(inputId = "sel.resumen", label = labelInput("selvar"), choices =  ""),
                               fluidRow(uiOutput("resumen")))

page.numerical.summary <- tabItem(tabName = "resumen",
                                   fluidRow(full.summary.table,
                                   variable.summary.table ))

# NORMALITY TEST PAGE -----------------------------------------------------------------------------------------------------

num.normal.plot.panel <- tabPanel(title = labelInput("plotnormal"), value = "tabNormalPlot", plotOutput('plot.normal', height = "65vh"))

cat.normal.plot.panel <- tabPanel(title = labelInput("normalidad"), value = "tabNormalCalc", DT::dataTableOutput('calculo.normal'))

boton.colores <- list(h4(labelInput("opciones")), hr(),
                      colourpicker::colourInput("col.normal", labelInput("selcolor"),value = "#00FF22AA", allowTransparent = T))

normality.code <- list(h4(labelInput("codigo")), hr(),
                          conditionalPanel("input.BoxNormal == 'tabNormalCalc'",
                                           code_field("run.calc.normal", "fieldCalcNormal", height = "20vh")),
                          conditionalPanel("input.BoxNormal == 'tabNormalPlot'",
                                           code_field("run.normal", "fieldCodeNormal", height = "25vh")))

tabs.normal <- tabsOptions(heights = c(33, 63), tabs.content = list(boton.colores, normality.code))

normal.options <-  tags$div(class = "multiple-select-var", selectInput(inputId = "sel.normal", label = NULL, choices =  ""))

page.test.normality <- tabItem(tabName = "normalidad",
                                  tabBox(id = "BoxNormal",
                                         width = 12, title = normal.options,
                                         num.normal.plot.panel,
                                         cat.normal.plot.panel,
                                         tabs.normal))

# DISPERSION PAGE ---------------------------------------------------------------------------------------------------------

tabs.dispersion  <-  tabsOptions(heights = c(30, 39),
                                 tabs.content = list(list(h4(labelInput("opciones")), hr(),
                                                     colourpicker::colourInput("col.disp", labelInput("selcolor"),
                                                                               value = "#FF0000AA",allowTransparent = T)),
                                                     list(h4(labelInput("codigo")), hr(),
                                                          column(width = 12, code_field("run.disp", "fieldCodeDisp", height = "7vh")))))

#dispersion.code <- column(width = 12, code_field(runid = "run.disp", fieldid = "fieldCodeDisp", height = "8vh"))

dispersion.data <- column(width = 4, DT::dataTableOutput('mostrar.disp.zoom'), hr(), plotOutput('plot.disp.zoom', height = "41vh"))

dispersion.options <- fluidRow(h4(style = "float:left;font-size: 20px;", labelInput("selvars")),
                                tags$div(class="multiple-select-var",style = "width:60%;",
                                         selectizeInput("select.var", NULL, multiple = T, choices = c(""),
                                                        options = list(maxItems = 3))))

dispersion.plot <- tabPanel(title = labelInput("dispersion"), value = "tabDisp",
                               fluidRow(column(width = 8, plotOutput('plot.disp', height = "65vh",
                                        brush = brushOpts(id = "zoom.disp", resetOnNew = TRUE))),
                                        dispersion.data))

page.dispersion<- tabItem(tabName = "dispersion",
                            tabBox(id = "BoxDisp", width = NULL, title = dispersion.options,
                                   dispersion.plot,
                                   tabs.dispersion))

# DISTRIBUTIONS PAGE ------------------------------------------------------------------------------------------------------

distribution.options <- list(h4(labelInput("opciones")), hr(), colourpicker::colourInput("col.dist", labelInput("selcolor"), value = "#FF0000AA", allowTransparent = T))

distribution.codes.fields <- list(h4(labelInput("codigo")), hr(),
                                      conditionalPanel(condition = "input.tabDyA == 'numericas'",
                                                       code_field("run.dya.num","fieldCodeNum", height = "7vh")),
                                      conditionalPanel(condition = "input.tabDyA == 'categoricas'",
                                                       code_field("run.dya.cat","fieldCodeCat", height = "7vh")))

code.distributions <- list(h4(labelInput("codigo")), hr(),
                              tabBox(id = "tabCodeDyA", width = NULL, title = labelInput("codedist"),
                                     tabPanel(title = labelInput("numericas"),
                                              aceEditor("fieldFuncNum",mode = "r",theme = "monokai",value = "",height = "285px",readOnly = T)),
                                     tabPanel(title = labelInput("categoricas"),
                                              aceEditor("fieldFuncCat",mode = "r",theme = "monokai",value = "",height = "165px",readOnly = T))))

distribution.tabs <- tabsOptions(buttons = list(icon("gear"), icon("terminal"), icon("info"), icon("code")),
                                   widths = c(50, 100, 100, 100), heights = c(30, 35, 48, 80),
                                   tabs.content = list(distribution.options,
                                                       distribution.codes.fields,
                                                       list(DT::dataTableOutput("mostrarAtipicos")),
                                                       code.distributions))

variable.selector.distribution <- tags$div(class = "multiple-select-var",
                                            conditionalPanel(condition = "input.tabDyA == 'numericas'",
                                                             selectInput(inputId = "sel.distribucion.num",label = NULL,choices =  "")),
                                            conditionalPanel(condition = "input.tabDyA == 'categoricas'",
                                                             selectInput(inputId = "sel.distribucion.cat",label = NULL,choices =  "")))

numerical.distribution.results <- tabPanel(title = labelInput("numericas"), value = "numericas", 
                                              plotOutput('plot.num', height = "70vh"),
                                              actionButton(inputId="distribucion_numerica",label = "",style="display:none;"))

categorical.distribution.results <- tabPanel(title = labelInput("categoricas"), value = "categoricas",plotOutput('plot.cat', height = "70vh"))

page.distributions <- tabItem(tabName = "distribucion",
                                 tabBox(id = "tabDyA", width = NULL,
                                        title =  variable.selector.distribution,
                                        numerical.distribution.results,
                                        categorical.distribution.results,
                                        distribution.tabs))

# CORRELATIONS PAGE -------------------------------------------------------------------------------------------------------

cor.options <-list(h4(labelInput("opciones")), hr(),
                    selectInput(inputId = "cor.metodo", label = labelInput("selmetodo"),
                                choices =  c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
                    selectInput(inputId = "cor.tipo", label = labelInput("seltipo"), choices =  c("lower", "upper", "full")))

cor.code <- list(h4(labelInput("codigo")), hr(),
                   aceEditor("fieldModelCor", height = "6vh", mode = "r", theme = "monokai", value = "", readOnly = T),
                   code_field("run.code.cor","fieldCodeCor", height = "7vh"))

cor.tabs <- tabsOptions(heights = c(48, 63),
                        tabs.content = list(cor.options, cor.code))

correlation.plot <- tabPanel(title = labelInput("correlacion"), value = "correlacion", plotOutput('plot.cor', height = "70vh"))

results.table.correlations <- tabPanel(title = labelInput("resultados"), value = "cor.salida", verbatimTextOutput("txtcor"))

page.correlations <- tabItem(tabName = "correlacion",
                                tabBox(id = "tabCor", width = NULL,
                                       correlation.plot,
                                       results.table.correlations,
                                       cor.tabs))

# PREDICTIVE POWER PAGE ---------------------------------------------------------------------------------------------------

code.power.num <- list(h4(labelInput("codigo")), hr(),
                         code_field(runid = "run.code.poder.num", fieldid = "fieldCodePoderNum", height = "16vh"))


tabs.power.num <- tabsOptions(buttons = list(icon("terminal")), widths = 100, heights = 55,
                                                      tabs.content = list(code.power.num))

power.plot.pairs <- tabPanel(title = labelInput('pares'), value = "predpares",
                             plotOutput('plot.pairs.poder', height = "55vh"))

pagina.poder <- tabItem(tabName = "poderPred",
                        tabBox(id = "BoxPodPred", width = NULL,
                               power.plot.pairs,
                               tabs.power.num))

# RL PAGE -----------------------------------------------------------------------------------------------------------------

rl.code  <- list(fluidRow(column(width = 9,h4(labelInput("codigo"))),
                            column(width = 2,br(),actionButton("runRl", label = labelInput("ejecutar"), icon = icon("play")))),
                   hr(),
                   conditionalPanel("input.BoxRl == 'tabRlModelo'",
                                    aceEditor("fieldCodeRl", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRl == 'tabRlCoef'",
                                  aceEditor("fieldCodeRlCoef", mode = "r", theme = "monokai",
                                            value = "", height = "10vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRl == 'tabRlPred'",
                                    aceEditor("fieldCodeRlPred", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRl == 'tabRlDisp'",
                                    aceEditor("fieldCodeRlDisp", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRl == 'tabRlIndex'",
                                    aceEditor("fieldCodeRlIG", mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled")))

tabs.rl  <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(95),
                        tabs.content = list(rl.code))

generate.rl.panel <- tabPanel(title = labelInput("generatem"),value = "tabRlModelo",
                             verbatimTextOutput("txtRl"))

coefficients.rl.panel <- tabPanel(title = labelInput("coeff"), value = "tabRlCoef",
                                  DT::dataTableOutput("rlCoefTable"))

prediccion.rl.panel <- tabPanel(title = labelInput("predm"), value = "tabRlPred",
                                DT::dataTableOutput("rlPrediTable"))

disp.rl.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRlDisp",
                          plotOutput('plot.rl.disp', height = "55vh"))

rl.general.index.panel <- tabPanel(title = labelInput("indices"), value = "tabRlIndex",
                                       br(),
                                       fluidRow(tableOutput('indexdfrl')),
                                       br(),
                                       fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                       br(),
                                       fluidRow(tableOutput('indexdfrl2')))


page.rl <- tabItem(tabName = "rl",
                     tabBox(id = "BoxRl", width = NULL, height ="80%",
                            generate.rl.panel,
                            coefficients.rl.panel,
                            prediccion.rl.panel,
                            disp.rl.panel,
                            rl.general.index.panel,
                            tabs.rl))

# RLR PAGE ----------------------------------------------------------------------------------------------------------------

rlr.options <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                              column(width = 2,br(),actionButton("runRlr", label = labelInput("ejecutar"), icon = icon("play")))),
                     hr(),
                     fluidRow(column(selectInput(inputId = "alpha.rlr", label = labelInput("selectAlg"), selected = 1,
                                                 choices = list("Ridge" = 0, "Lasso" = 1)),width = 6),
                              column(br(), switchInput(inputId = "switch.scale.rlr", onStatus = "success", offStatus = "danger", value = T,
                                                 label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=6)),
                     fluidRow(column(id = "colManualLanda",width = 5, numericInput("landa", labelInput("landa"),value = 2, "NULL", width = "100%")), br(),
                              column(width = 6, switchInput(inputId = "permitir.landa", onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                            label = "", onLabel = "Manual", offLabel = labelInput("automatico"), labelWidth = "100%"),
                                     style = "padding-top: 5px;")))

rlr.code  <- list(fluidRow(column(width = 9, h4(labelInput("codigo")))),
                   hr(),
                   conditionalPanel("input.BoxRlr == 'tabRlrModelo'",
                                    aceEditor("fieldCodeRlr", mode = "r", theme = "monokai",
                                              value = "", height = "8vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRlr == 'tabRlrLanda'",
                                    aceEditor("fieldCodeRlrLanda", mode = "r", theme = "monokai",
                                              value = "", height = "8vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRlr == 'tabRlrPosibLanda'",
                                    aceEditor("fieldCodeRlrPosibLanda", mode = "r", theme = "monokai",
                                              value = "", height = "8vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRlr == 'tabRlrCoeff'",
                                    aceEditor("fieldCodeRlrCoeff", mode = "r", theme = "monokai",
                                              value = "", height = "8vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRlr == 'tabRlrPred'",
                                    aceEditor("fieldCodeRlrPred", mode = "r", theme = "monokai",
                                              value = "", height = "10vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRlr == 'tabRlrDisp'",
                                    aceEditor("fieldCodeRlrDisp", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRlr == 'tabRlrIndex'",
                                    aceEditor("fieldCodeRlrIG", mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled")))

tabs.rlr  <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(80, 95),
                        tabs.content = list(rlr.options, rlr.code))

generate.rlr.panel <- tabPanel(title = labelInput("generatem"),value = "tabRlrModelo",
                             verbatimTextOutput("txtRlr"))

posib.landa.rlr.panel <- tabPanel(title = labelInput("posibLanda"),value = "tabRlrPosibLanda",
                            plotOutput('plot.rlr.posiblanda', height = "55vh"))

coeff.rlr.panel <- tabPanel(title = labelInput("coeff"),value = "tabRlrCoeff",
                              verbatimTextOutput("txtRlrCoeff"))

landa.rlr.panel <- tabPanel(title = labelInput("gcoeff"),value = "tabRlrLanda",
                            plotOutput('plot.rlr.landa', height = "55vh"))

prediccion.rlr.panel <- tabPanel(title = labelInput("predm"), value = "tabRlrPred",
                                DT::dataTableOutput("rlrPrediTable"))

disp.rlr.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRlrDisp",
                          plotOutput('plot.rlr.disp', height = "55vh"))

rlr.general.index.panel <- tabPanel(title = labelInput("indices"), value = "tabRlrIndex",
                                        br(),
                                        fluidRow(tableOutput('indexdfrlr')),
                                        br(),
                                        fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                        br(),
                                        fluidRow(tableOutput('indexdfrlr2')))


page.rlr <- tabItem(tabName = "rlr",
                     tabBox(id = "BoxRlr", width = NULL, height ="80%",
                            generate.rlr.panel,
                            posib.landa.rlr.panel,
                            landa.rlr.panel,
                            coeff.rlr.panel,
                            prediccion.rlr.panel,
                            disp.rlr.panel,
                            rlr.general.index.panel,
                            tabs.rlr))

# KNN PAGE ----------------------------------------------------------------------------------------------------------------

knn.options <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                              column(width = 2,br(),actionButton("runKnn", label = labelInput("ejecutar"), icon = icon("play")))),
                     hr(),
                     fluidRow(column(numericInput("kmax.knn", labelInput("kmax"), min = 1,step = 1, value = 7), width = 6),
                              column(selectInput(inputId = "kernel.knn", label = labelInput("selkernel"),selected = 1,
                                     choices = c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                 "triweight", "cos","inv","gaussian")),width = 6)),
                     fluidRow(column(br(),switchInput(inputId = "switch.scale.knn", onStatus = "success", offStatus = "danger", value = T,
                                     label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=6),
                              column(width=6, numericInput("distance.knn", labelInput("distknn"), min = 1,step = 1, value = 2))) )

knn.code <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxKnn == 'tabKknModelo'",
                                    aceEditor("fieldCodeKnn", mode = "r", theme = "monokai", value = "", height = "4vh", readOnly = F)),
                   conditionalPanel("input.BoxKnn == 'tabKknPred'",
                                    aceEditor("fieldCodeKnnPred", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxKnn == 'tabKknDisp'",
                                    aceEditor("fieldCodeKnnDisp", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxKnn == 'tabKknIndex'",
                                    aceEditor("fieldCodeKnnIG", mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled")))

tabs.knn <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(80, 95),
                        tabs.content = list(knn.options, knn.code))

generate.knn.panel <- tabPanel(title = labelInput("generatem"), value = "tabKknModelo",
                             verbatimTextOutput("txtknn"))

prediccion.knn.panel <- tabPanel(title = labelInput("predm"), value = "tabKknPred",
                                 DT::dataTableOutput("knnPrediTable"))

disp.knn.panel <- tabPanel(title = labelInput("dispersion"), value = "tabKknDisp",
                                       plotOutput('plot.knn.disp', height = "55vh"))

general.index.knn.panel <- tabPanel(title = labelInput("indices"), value = "tabKknIndex",
                                        br(),
                                        fluidRow(tableOutput('indexdfknn')),
                                        br(),
                                        fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                        br(),
                                        fluidRow(tableOutput('indexdfknn2')))

page.knn <- tabItem(tabName = "knn",
                      tabBox(id = "BoxKnn", width = NULL, height ="80%",
                             generate.knn.panel,
                             prediccion.knn.panel,
                             disp.knn.panel,
                             general.index.knn.panel,
                             tabs.knn))

# SVM PAGE ----------------------------------------------------------------------------------------------------------------

svm.options <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                              column(width = 2,br(),actionButton("runSvm", label = labelInput("ejecutar"), icon = icon("play")))),
                     hr(),
                     conditionalPanel("input.BoxSvm != 'tabSvmPlot'",
                                      fluidRow(column(br(),switchInput(inputId = "switch.scale.svm", onStatus = "success", offStatus = "danger", value = T,
                                                                       label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width = 6),
                                              column(selectInput(inputId = "kernel.svm", label = labelInput("selkernel"), selected = "radial",
                                                                 choices =  c("linear", "polynomial", "radial", "sigmoid")), width=6))))

svm.code <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxSvm == 'tabSvmModelo'",
                                    aceEditor("fieldCodeSvm", mode = "r", theme = "monokai", value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxSvm == 'tabSvmDisp'",
                                    aceEditor("fieldCodeSvmDisp", mode = "r", theme = "monokai",
                                              value = "", height = "6vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxSvm == 'tabSvmPred'",
                                    aceEditor("fieldCodeSvmPred", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxSvm == 'tabSvmIndex'",
                                    aceEditor("fieldCodeSvmIG", mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled")))

tabs.svm <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(60, 95),
                        tabs.content = list(svm.options, svm.code))

generate.svm.panel <- tabPanel(title = labelInput("generatem"), value = "tabSvmModelo",
                              verbatimTextOutput("txtSvm"))

disp.svm.panel <- tabPanel(title = labelInput("dispersion"), value = "tabSvmDisp",
                           plotOutput('plot.svm.disp', height = "55vh"))

prediction.svm.panel <- tabPanel(title = labelInput("predm"), value = "tabSvmPred",
                                 DT::dataTableOutput("svmPrediTable"))

general.index.svm.panel <- tabPanel(title = labelInput("indices"), value = "tabSvmIndex",
                                        br(),
                                        fluidRow(tableOutput('indexdfsvm')),
                                        br(),
                                        fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                        br(),
                                        fluidRow(tableOutput('indexdfsvm2')))

page.svm <- tabItem(tabName = "svm",
                      tabBox(id = "BoxSvm", width = NULL, height ="80%",
                             generate.svm.panel,
                             prediction.svm.panel,
                             disp.svm.panel,
                             general.index.svm.panel,
                             tabs.svm))

# RD PAGE -----------------------------------------------------------------------------------------------------------------

rd.options  <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                              column(width = 2,br(),actionButton("runRd", label = labelInput("ejecutar"), icon = icon("play")))),
                     hr(),
                     fluidRow(column(selectInput(inputId = "modo.rd", label = labelInput("selectAlg"),selected = 0,
                                                 choices = list("ACP" = 0, "MCP" = 1)),width = 6),
                              column(br(), switchInput(inputId = "switch.scale.rd", onStatus = "success", offStatus = "danger", value = T,
                                                       label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=6)),
                     fluidRow(column(id = "colManualCom",width = 6, numericInput("ncomp.rd", labelInput("ncomp"),value = 2, min = 1, width = "100%")), br(),
                              column(width = 6, switchInput(inputId = "permitir.ncomp", onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                            label = "", onLabel = "Manual", offLabel = labelInput("automatico"), labelWidth = "100%"))))

rd.code   <- list(fluidRow(column(width = 9,h4(labelInput("codigo")))),
                    hr(),
                    conditionalPanel("input.BoxRd == 'tabRdModelo'",
                                     aceEditor("fieldCodeRd", mode = "r", theme = "monokai",
                                               value = "", height = "8vh", readOnly = F, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRd == 'tabRdRMSE'",
                                     aceEditor("fieldCodeRdRMSE", mode = "r", theme = "monokai",
                                               value = "", height = "14vh", readOnly = F, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRd == 'tabRdPlotPred'",
                                     aceEditor("fieldCodeRdPlotPred", mode = "r", theme = "monokai",
                                               value = "", height = "14vh", readOnly = F, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRd == 'tabRdPlotVarPred'",
                                     aceEditor("fieldCodeRdPlotVarPred", mode = "r", theme = "monokai",
                                               value = "", height = "14vh", readOnly = F, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRd == 'tabRdPred'",
                                     aceEditor("fieldCodeRdPred", mode = "r", theme = "monokai",
                                               value = "", height = "10vh", readOnly = F, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRd == 'tabRdDisp'",
                                     aceEditor("fieldCodeRdDisp", mode = "r", theme = "monokai",
                                               value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                    conditionalPanel("input.BoxRd == 'tabRdIndex'",
                                     aceEditor("fieldCodeRdIG", mode = "r", theme = "monokai",
                                               value = "", height = "22vh", readOnly = F, autoComplete = "enabled")))

tabs.rd  <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(80, 95),
                        tabs.content = list(rd.options, rd.code))

generate.rd.panel <- tabPanel(title = labelInput("generatem"),value = "tabRdModelo",
                             verbatimTextOutput("txtRd"))

rmse.rd.panel <- tabPanel(title = labelInput("RMSE"),value = "tabRdRMSE",
                          plotOutput('plot.rd.rmse', height = "55vh"))

plot.pred.rd.panel <- tabPanel(title = labelInput("RdPred"), value = "tabRdPlotPred",
                               plotOutput('plot.rd.pred', height = "55vh"))

panel.plot.var.pred.rd <- tabPanel(title = labelInput("RdVarPred"), value = "tabRdPlotVarPred",
                                   plotOutput('plot.rd.var.pred', height = "55vh"))

prediction.rd.panel <- tabPanel(title = labelInput("predm"), value = "tabRdPred",
                                DT::dataTableOutput("rdPrediTable"))

disp.rd.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRdDisp",
                          plotOutput('plot.rd.disp', height = "55vh"))

general.index.rd.panel <- tabPanel(title = labelInput("indices"), value = "tabRdIndex",
                                       br(),
                                       fluidRow(tableOutput('indexdfrd')),
                                       br(),
                                       fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                       br(),
                                       fluidRow(tableOutput('indexdfrd2')))

page.rd <- tabItem(tabName = "rd",
                     tabBox(id = "BoxRd", width = NULL, height ="80%",
                            generate.rd.panel,
                            rmse.rd.panel,
                            plot.pred.rd.panel,
                            panel.plot.var.pred.rd,
                            prediction.rd.panel,
                            disp.rd.panel,
                            general.index.rd.panel,
                            tabs.rd))

# DT PAGE ------------------------------------------------------------------------------------------------------------

dt.options <- list(fluidRow(column(width = 9, h4(labelInput("opciones"))),
                              column(width = 2, br(),actionButton("runDt", label = labelInput("ejecutar"), icon = icon("play")))),
                     hr(),
                     fluidRow(column(numericInput("minsplit.dt", labelInput("minsplit"), 2, width = "100%",min = 1), width = 6),
                              column(numericInput("maxdepth.dt", labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1),width = 6)))

dt.code <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxDt == 'tabDtModelo'",
                                    aceEditor("fieldCodeDt", mode = "r", theme = "monokai",
                                              value = "", height = "7vh", readOnly = F, autoComplete = "enabled")),
                  conditionalPanel("input.BoxDt == 'tabDtPlot'",
                                   aceEditor("fieldCodeDtPlot", mode = "r", theme = "monokai",
                                             value = "", height = "7vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxDt == 'tabDtPred'",
                                    aceEditor("fieldCodeDtPred", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                  conditionalPanel("input.BoxDt == 'tabDtDisp'",
                                   aceEditor("fieldCodeDtDisp", mode = "r", theme = "monokai",
                                             value = "", height = "7vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxDt == 'tabDtIndex'",
                                    aceEditor("fieldCodeDtIG", mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled")),
                  conditionalPanel("input.BoxDt == 'tabDtReglas'",
                                   aceEditor("fieldCodeDtRule", mode = "r", theme = "monokai",
                                             value = "", height = "4vh", readOnly = F, autoComplete = "enabled")))

tabs.dt <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(80, 95),
                        tabs.content = list(dt.options, dt.code))

generate.dt.panel <- tabPanel(title = labelInput("generatem"), value = "tabDtModelo",
                             verbatimTextOutput("txtDt"))

plot.dt <- tabPanel(title = labelInput("garbol"), value = "tabDtPlot",
                     plotOutput('plot.dt', height = "55vh"))

prediction.dt.panel <- tabPanel(title = labelInput("predm"), value = "tabDtPred",
                                 DT::dataTableOutput("dtPrediTable"))

disp.dt.panel <- tabPanel(title = labelInput("dispersion"), value = "tabDtDisp",
                           plotOutput('plot.dt.disp', height = "55vh"))

general.index.dt.panel <- tabPanel(title = labelInput("indices"),value = "tabDtIndex",
                                       br(),
                                       fluidRow(tableOutput('indexdfdt')),
                                       br(),
                                       fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                       br(),
                                       fluidRow(tableOutput('indexdfdt2')))

rules.dt.panel <- tabPanel(title = labelInput("reglas"),value = "tabDtReglas",
                            verbatimTextOutput("rulesDt"))

page.dt <- tabItem(tabName = "dt",
                     tabBox(id = "BoxDt", width = NULL, height ="80%",
                            generate.dt.panel,
                            plot.dt,
                            prediction.dt.panel,
                            disp.dt.panel,
                            general.index.dt.panel,
                            rules.dt.panel,
                            tabs.dt))

# RF PAGE ------------------------------------------------------------------------------------------------------------

rf.options <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                             column(width = 2,br(),actionButton("runRf", label = labelInput("ejecutar"), icon = icon("play")))),
                    hr(),
                    conditionalPanel("input.BoxRf != 'tabRfRules'",
                                      fluidRow(column(numericInput("ntree.rf", labelInput("numTree"), 20, width = "100%", min = 0), width = 6),
                                               column(numericInput("mtry.rf",labelInput("numVars"),1, width = "100%", min = 1), width=6))),
                     conditionalPanel("input.BoxRf == 'tabRfRules'",
                                      numericInput("rules.rf.n",labelInput("ruleNumTree"),1, width = "100%", min = 1)))

rf.code  <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxRf == 'tabRfModelo'",
                                    aceEditor("fieldCodeRf", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRf == 'tabRfImp'",
                                    aceEditor("fieldCodeRfPlot", mode = "r", theme = "monokai",
                                              value = "", height = "28vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRf == 'tabRfPred'",
                                    aceEditor("fieldCodeRfPred", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRf == 'tabRfDisp'",
                                    aceEditor("fieldCodeRfDisp", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRf == 'tabRfIndex'",
                                    aceEditor("fieldCodeRfIG", mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxRf == 'tabRfRules'",
                                    aceEditor("fieldCodeRfRules", mode = "r", theme = "monokai",
                                              value = "", height = "4vh", readOnly = F, autoComplete = "enabled")))

tabs.rf  <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(65, 95),
                        tabs.content = list(rf.options, rf.code))

generate.rf.panel <- tabPanel(title = labelInput("generatem"),value = "tabRfModelo",
                             verbatimTextOutput("txtRf"))

plot.rf <- tabPanel(title = labelInput("varImp"), value = "tabRfImp",
                    plotOutput('plot.rf', height = "55vh"))

prediction.rf.panel <- tabPanel(title = labelInput("predm"), value = "tabRfPred",
                                DT::dataTableOutput("rfPrediTable"))

disp.rf.panel <- tabPanel(title = labelInput("dispersion"), value = "tabRfDisp",
                           plotOutput('plot.rf.disp', height = "55vh"))

general.index.rf.panel <- tabPanel(title = labelInput("indices"), value = "tabRfIndex",
                                       br(),
                                       fluidRow(tableOutput('indexdfrf')),
                                       br(),
                                       fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                       br(),
                                       fluidRow(tableOutput('indexdfrf2')))

rf.rules.panel <- tabPanel(title = labelInput("reglas"), value = "tabRfRules",
                      verbatimTextOutput("rulesRf"))

page.rf <- tabItem(tabName = "rf",
                     tabBox(id = "BoxRf", width = NULL, height ="80%",
                            generate.rf.panel,
                            plot.rf,
                            prediction.rf.panel,
                            disp.rf.panel,
                            general.index.rf.panel,
                            rf.rules.panel,
                            tabs.rf))

# BOOSTING PAGE ------------------------------------------------------------------------------------------------------

b.options <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                            column(width = 2,br(),actionButton("runBoosting", label = labelInput("ejecutar"), icon = icon("play")))),
                   hr(),
                   fluidRow(column(numericInput("iter.boosting", labelInput("numTree"), 20, width = "100%",min = 1), width = 6),
                            column(numericInput("shrinkage.boosting", labelInput("shrinkage"), 0.01, width = "100%",min = 0.001, step = 0.001), width=6)),
                   fluidRow(column(selectInput(inputId = "tipo.boosting", label = labelInput("selectAlg"),selected = 1,
                                               choices =  c("gaussian", "laplace", "tdist")), width = 6)))

b.code  <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxB == 'tabBModelo'",
                                    aceEditor("fieldCodeBoosting", mode = "r", theme = "monokai",
                                              value = "", height = "5vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxB == 'tabBImp'",
                                    aceEditor("fieldCodeBoostingPlotImport", mode = "r", theme = "monokai",
                                              value = "", height = "10vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxB == 'tabBPred'",
                                    aceEditor("fieldCodeBoostingPred", mode = "r", theme = "monokai",
                                              value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                  conditionalPanel("input.BoxB == 'tabBDisp'",
                                   aceEditor("fieldCodeBoostingDisp", mode = "r", theme = "monokai",
                                             value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxB == 'tabBIndex'",
                                    aceEditor("fieldCodeBoostingIG", mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled")))

tabs.b  <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(50,100), heights = c(63, 95),
                        tabs.content = list(b.options, b.code))

generate.b.panel <- tabPanel(title = labelInput("generatem"), value = "tabBModelo",
                              verbatimTextOutput("txtBoosting"))

plot.boosting.import <- tabPanel(title = labelInput("varImp"), value = "tabBImp",
                    plotOutput('plot.boosting.import', height = "55vh"))

prediction.b.panel <- tabPanel(title = labelInput("predm"), value = "tabBPred",
                                 DT::dataTableOutput("boostingPrediTable"))

disp.boosting.panel <- tabPanel(title = labelInput("dispersion"), value = "tabBDisp",
                           plotOutput('plot.boosting.disp', height = "55vh"))

general.index.b.panel <- tabPanel(title = labelInput("indices"),value = "tabBIndex",
                                             br(),
                                             fluidRow(tableOutput('indexdfb')),
                                             br(),
                                             fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                             br(),
                                             fluidRow(tableOutput('indexdfb2')))

pagina.boosting <- tabItem(tabName = "boosting",
                           tabBox(id = "BoxB", width = NULL, height ="80%",
                                  generate.b.panel,
                                  plot.boosting.import,
                                  prediction.b.panel,
                                  disp.boosting.panel,
                                  general.index.b.panel,
                                  tabs.b))

# NN PAGE ------------------------------------------------------------------------------------------------------------

nn.options <- list(fluidRow(column(width = 9,h4(labelInput("opciones"))),
                              column(width = 2,br(),actionButton("runNn", label = labelInput("ejecutar"), icon = icon("play")))),
                    hr(),
                    fluidRow(column(numericInput("threshold.nn",labelInput("threshold"),
                                                 min = 0, step = 0.01, value = 0.05), width = 6),
                             column(numericInput("stepmax.nn",labelInput("stepmax"),
                                                 min = 100, step = 100, value = 5000), width = 6)),
                    fluidRow(column(sliderInput(inputId = "cant.capas.nn", min = 1, max = 10,
                                                 label = labelInput("selectCapas"), value = 2), width = 12)),
                    fluidRow(lapply(1:10, function(i) tags$span(numericInput(paste0("nn.cap.",i), NULL,
                                                                    min = 1, step = 1, value = 2),
                                                                 class = "mini-numeric-select"))))

nn.code <- list(h4(labelInput("codigo")), hr(),
                   conditionalPanel("input.BoxNn == 'tabNnModelo'",
                                    aceEditor("fieldCodeNn", mode = "r", theme = "monokai", value = "", height = "22vh", readOnly = F)),
                  conditionalPanel("input.BoxNn == 'tabNnPlot'",
                                   aceEditor("fieldCodeNnPlot", mode = "r", theme = "monokai", value = "", height = "9vh", readOnly = F)),
                   conditionalPanel("input.BoxNn == 'tabNnPred'",
                                    aceEditor("fieldCodeNnPred", mode = "r", theme = "monokai",
                                              value = "", height = "10vh", readOnly = F, autoComplete = "enabled")),
                  conditionalPanel("input.BoxNn == 'tabNnDisp'",
                                   aceEditor("fieldCodeNnDisp", mode = "r", theme = "monokai",
                                             value = "", height = "3vh", readOnly = F, autoComplete = "enabled")),
                   conditionalPanel("input.BoxNn == 'tabNnIndex'",
                                    aceEditor("fieldCodeNnIG", mode = "r", theme = "monokai",
                                              value = "", height = "22vh", readOnly = F, autoComplete = "enabled")))

tabs.nn <- tabsOptions(buttons = list(icon("gear"),icon("code")), widths = c(75,100), heights = c(95, 95),
                        tabs.content = list(nn.options, nn.code))

plot.nn <- tabPanel(title = labelInput("redPlot"), value = "tabNnPlot",
                    plotOutput('plot.nn', height = "55vh"))

generate.nn.panel <- tabPanel(title = labelInput("generatem"), value = "tabNnModelo",
                              verbatimTextOutput("txtnn"))

prediction.nn.panel <- tabPanel(title = labelInput("predm"), value = "tabNnPred",
                                 DT::dataTableOutput("nnPrediTable"))

disp.nn.panel <- tabPanel(title = labelInput("dispersion"), value = "tabNnDisp",
                           plotOutput('plot.nn.disp', height = "55vh"))

general.index.nn.panel <- tabPanel(title = labelInput("indices"), value = "tabNnIndex",
                                       br(),
                                       fluidRow(tableOutput('indexdfnn')),
                                       br(),
                                       fluidRow(column(width = 12, align="center", tags$h3(labelInput("resumenVarPre")))),
                                       br(),
                                       fluidRow(tableOutput('indexdfnn2')))

page.nn  <- tabItem(tabName = "nn",
                      tabBox(id = "BoxNn", width = NULL, height ="80%",
                             generate.nn.panel,
                             plot.nn,
                             prediction.nn.panel,
                             disp.nn.panel,
                             general.index.nn.panel,
                             tabs.nn))

# MODEL COMPARISON PAGE ---------------------------------------------------------------------------------------------------

model.selector <- checkboxGroupButtons("select.models", labelInput("selectMod"), c(" ---- " = "NoDisponible"),
                                         size = "sm", status = "primary",
                                         checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                          no = icon("remove", lib = "glyphicon")))

comparison.options <- list(fluidRow(column(width = 10,h4(labelInput("opciones")))),
                             hr(),
                             fluidRow(column(model.selector, width = 12)))


tabs.comparison  <- tabsOptions(buttons = list(icon("gear")), widths = c(100), heights = c(88),
                                 tabs.content = list(comparison.options))

table.comparison.panel <- tabPanel(title = labelInput("tablaComp"),
                                    DT::dataTableOutput("TablaComp", height="70vh"))

page.comparison <- tabItem(tabName = "comparar",
                              tabBox(id = "BoxCom", width = NULL, height ="80%",
                                     table.comparison.panel,
                                     tabs.comparison))



# NEW PREDICTIONS PAGE ----------------------------------------------------------------------------------------------------

# Data display

show.data.pred <- box(title = labelInput("data"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                          DT::DTOutput('contentsPred'), type = 7, color = "#CBB051")

show.data.pred2 <- box(title = labelInput("data"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                           DT::DTOutput('contentsPred2'), type = 7, color = "#CBB051")

show.data.pred3 <- box(title = labelInput("data"), status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                           DT::DTOutput('contentsPred3'), type = 7, color = "#CBB051")

# Loading and transforming data

data.upload.panel.pred <- tabPanel(title = labelInput("cargarDatos"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                               fluidRow(column(width = 5,
                               checkboxInput('headerNPred', labelInput("header"), TRUE),
                               checkboxInput('rownameNPred', labelInput("Rownames"), TRUE),
                               radioButtonsTr('sepNPred', 'separador', c(';', ',', '\t'), c("puntocoma", "coma", "tab")),
                               radioButtonsTr('decNPred',"separadordec", c(',', '.'), c("coma", "punto")),
                               switchInput(inputId = "deleteNAnPred", onStatus = "success", offStatus = "danger", value = T, width = "100%",
                                           label = labelInput("eliminana"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"),
                               fileInput('file2', label = labelInput("cargarchivo"), placeholder = "", buttonLabel =  labelInput("subir"), width = "100%",
                                         accept = c('text/csv', '.csv')),
                               actionButton("loadButtonNPred", labelInput("cargar"), width = "100%")),
                               column(width = 7, show.data.pred)))


tansform.data.panel <- tabPanel(title = labelInput("transDatos"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                   fluidRow(column(width = 5,
                                                   DT::dataTableOutput('transDataPredN'),
                                                   br(),br(),
                                                   actionButton("transButtonPredN", labelInput("aplicar"), width = "100%")),
                                   column(width = 7, show.data.pred2)))

data.upload.panel.pred2 <- tabPanel(title = labelInput("cargarNuev"), width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                                    fluidRow(column(width = 5,
                                                    checkboxInput('headerNPred2', labelInput("header"), TRUE),
                                                    checkboxInput('rownameNPred2',  labelInput("Rownames"), TRUE),
                                                    radioButtonsTr('sep.nPred2', 'separador', c(';', ',', '\t'), c("puntocoma", "coma", "tab")),
                                                    radioButtonsTr('dec.nPred2', "separadordec", c(',', '.'), c("coma", "punto")),
                                                    switchInput(inputId = "deleteNAnPred2", onStatus = "success", offStatus = "danger", value = T, width = "100%",
                                                                label = labelInput("eliminana"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"),
                                                    fileInput('file3', label = labelInput("cargarchivo"), placeholder = "", buttonLabel = labelInput("subir"), width = "100%",
                                                              accept = c('text/csv', '.csv')),
                                                    actionButton("loadButtonNPred2", labelInput("cargar"), width = "100%")),
                                             column(width = 7, show.data.pred3)))

# Model Options

options.rl.pred <- list() # Vacio

options.rlr.pred <- fluidRow(column(selectInput(inputId = "alpha.rlr.pred", label = labelInput("selectAlg"),selected = 1,
                                                 choices = list("Ridge" = 0, "Lasso" = 1)),width = 3),
                              column(br(), switchInput(inputId = "switch.scale.rlr.pred", onStatus = "success", offStatus = "danger", value = T,
                                                       label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=3),
                              column(id = "colManualLanda.pred",width = 3, numericInput("landa.pred", labelInput("landa"),value = 2, min = 0, "NULL", width = "100%")), br(),
                              column(width = 3, switchInput(inputId = "permitir.landa.pred", onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                            label = "", onLabel = "Manual", offLabel = labelInput("automatico"), labelWidth = "100%")))

options.rd.pred <-  fluidRow(column(selectInput(inputId = "mode.rd.pred", label = labelInput("selectAlg"),selected = 0,
                                                choices = list("ACP" = 0, "MCP" = 1)),width = 3),
                             column(br(), switchInput(inputId = "switch.scale.rd.pred", onStatus = "success", offStatus = "danger", value = T,
                                                      label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%"), width=3),
                             column(id = "colManualCom.pred",width = 3, numericInput("ncomp.rd.pred", labelInput("ncomp"),value = 2, min = 0, "NULL", width = "100%")), br(),
                             column(width = 3, switchInput(inputId = "permitir.ncomp.pred", onStatus = "success", offStatus = "danger", value = F, width = "100%",
                                                           label = "", onLabel = "Manual", offLabel = labelInput("automatico"), labelWidth = "100%")))

options.knn.pred <- fluidRow(column(width = 3, br() , switchInput(inputId = "switch.scale.knn.pred", onStatus = "success", offStatus = "danger", value = T,
                                                              label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%", width = "100%")),
                              column(width = 3, numericInput("kmax.knn.pred", labelInput("kmax"), min = 1,step = 1, value = 7,width="100%")),
                              column(width = 3, selectInput(inputId = "kernel.knn.pred", label = labelInput("selkernel") ,selected = 1, width="100%",
                                                                      choices =  c("optimal", "rectangular", "triangular", "epanechnikov", "biweight",
                                                                                   "triweight", "cos","inv","gaussian"))),
                              column(width = 3,numericInput("distance.knn.pred", labelInput("distknn"), min = 1,step = 1, value = 2) ))

options.svm.pred <- fluidRow(column(width = 6, br(), switchInput(inputId = "switch.scale.svm.pred", onStatus = "success", offStatus = "danger", value = T,
                                                           label = labelInput("escal"), onLabel = labelInput("si"), offLabel = labelInput("no"), labelWidth = "100%", width = "100%")),
                         column(width = 6, selectInput(inputId = "kernel.svm.pred", label = labelInput("selkernel"), selected = "radial", width="100%",
                                                           choices =  c("linear", "polynomial", "radial", "sigmoid"))))

options.dt.pred <- fluidRow(column(width = 6, numericInput("minsplit.dt.pred", labelInput("minsplit"), 20, width = "100%",min = 1)),
                             column(width = 6, numericInput("maxdepth.dt.pred", labelInput("maxdepth"), 15, width = "100%",min = 0, max = 30, step = 1)))

options.rf.pred <- fluidRow(column(width = 6, numericInput("ntree.rf.pred", labelInput("numTree"), 20, width = "100%", min = 0)),
                             column(width = 6, numericInput("mtry.rf.pred",labelInput("numVars"),1, width = "100%", min = 1)))

options.boosting.pred <- list(fluidRow(column(width = 4, numericInput("iter.boosting.pred", labelInput("numTree"), 500, width = "100%",min = 1)),
                                   column(width = 4, numericInput("shrinkage.boosting.pred",labelInput("shrinkage"), 0.01, width = "100%",min = 0.0001)),
                                   column(width = 4, selectInput(inputId = "tipo.boosting.pred", label = labelInput("selectAlg"),selected = 1, width = "100%",
                                                                 choices =  c("gaussian", "laplace", "tdist")))))

options.nn.pred <-list(fluidRow(column(numericInput("threshold.nn.pred",labelInput("threshold"),
                                                     min = 0, step = 0.01, value = 0.05), width = 4),
                                 column(numericInput("stepmax.nn.pred",labelInput("stepmax"),
                                                     min = 100, step = 100, value = 5000), width = 4),
                                 column(sliderInput(inputId = "cant.capas.nn.pred", min = 1, max = 10,
                                                    label = labelInput("selectCapas"), value = 10), width = 4)),
                        fluidRow(lapply(1:10, function(i) tags$span(numericInput(paste0("nn.cap.pred.",i), NULL,
                                                                                 min = 1, step = 1, value = 2),
                                                                    class = "mini-numeric-select"))))

options.model <- list(selectInput(inputId = "sel.predic.var.nuevos", label = labelInput("seleccionarPredecir"), choices =  "", width = "100%"),
                        radioGroupButtons("selectModelsPred", labelInput("selectMod"), 
                                          list("<span data-id=\"rll\"></span>" = "rl",
                                               "<span data-id=\"rlr\"></span>" = "rlr",
                                               "<span data-id=\"knnl\"></span>" = "knn",
                                               "<span data-id=\"dtl\"></span>" = "dt",
                                               "<span data-id=\"rfl\"></span>" = "rf",
                                               "<span data-id=\"bl\"></span>" = "boosting",
                                               "<span data-id=\"svml\"></span>" = "svm",
                                               "<span data-id=\"rd\"></span>" = "rd",
                                               "<span data-id=\"nn\"></span>" = "nn"),
                                          size = "sm", status = "primary",individual = FALSE, justified = FALSE, selected = "knn",
                                          checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                           no = icon("remove", lib = "glyphicon"))))

create.pred.model.panel <- tabPanel(title = labelInput("seleParModel"),solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE, value = "crearModelo",
                                    options.model,
                                    conditionalPanel(condition =  "input.selectModelsPred == 'rl'",
                                                     options.rl.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'rlr'",
                                                     options.rlr.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'knn'",
                                                     options.knn.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'dt'",
                                                     options.dt.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'rf'",
                                                     options.rf.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'boosting'",
                                                     options.boosting.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'svm'",
                                                     options.svm.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'nn'",
                                                     options.nn.pred),
                                    conditionalPanel(condition =  "input.selectModelsPred == 'rd'",
                                                     options.rd.pred),
                                    verbatimTextOutput("txtPredNuevos"),
                                    actionButton("PredNuevosBttnModelo", labelInput("generarM"), width  = "100%", style = "background-color:#CBB051;color:#fff;margin-top:9px;"))


tabs.models  <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(40),
                       tabs.content = list(list(aceEditor("fieldPredNuevos", mode = "r", theme = "monokai", value = "", height = "20vh", readOnly = F))))

tabs.models2  <- tabsOptions(buttons = list(icon("code")), widths = c(100), heights = c(40),
                             tabs.content = list(aceEditor("fieldCodePredPN", mode = "r", theme = "monokai",
                                                           value = "", height = "20vh", readOnly = F, autoComplete = "enabled")))

prediccion.pred.panel <- tabPanel(title = labelInput("predicnuevos"), value = "predicModelo",
                                 DT::dataTableOutput("PrediTablePN"),
                                 hr(),
                                 downloadButton("downloaDatosPred", labelInput("descargar"), style = "width:100%"),
                                 actionButton("predecirPromidat", "preditc"))

page.new.predictions <- tabItem(tabName = "predNuevos",
                                      tabBox(id = "BoxModelo", width = NULL, height ="80%",
                                             data.upload.panel.pred,
                                             tansform.data.panel,
                                             create.pred.model.panel,
                                             data.upload.panel.pred2,
                                             prediccion.pred.panel,
                                             conditionalPanel(condition =  "input.BoxModelo == 'crearModelo'", tabs.models),
                                             conditionalPanel(condition =  "input.BoxModelo == 'predicModelo'", tabs.models2)))

# REPORT PAGE -------------------------------------------------------------------------------------------------------------

header.report.panel <- column(width = 5, box(title = labelInput("reporte"), width = 12,
                                              textInput("textTitulo", value = "Sin Titulo", width = "100%", label = labelInput("titulo")),
                                              textInput("textNombre", value = "PROMiDAT", width = "100%", label = labelInput("nombre")),
                                              downloadButton("descargar", labelInput("descargar"), class = "center-button")))

report.code.panel <- column(width = 7,box(title = labelInput("codreporte"), width = 12, height = "50vh",status = "primary", solidHeader = TRUE,
                                             collapsible = TRUE, aceEditor("fieldCodeReport", mode="markdown", value='', height = "43vh")))

report.output.panel <- fluidRow(column(width = 12, box(title = labelInput("salida"), width = 12, height = "35vh", verbatimTextOutput("txtreport"))))


page.generate.report <- tabItem(tabName = "reporte",
                                  fluidRow(header.report.panel ,
                                  report.code.panel),
                                  report.output.panel)

# INFORMATION PAGE --------------------------------------------------------------------------------------------------------

page.info <- tabItem(tabName = "acercaDe",
                       img(src="Logo.png", style="padding-bottom:20px;margin-left: auto;margin-right: auto;display: block;width: 50%;"),
                       infoBoxPROMiDAT(labelInput("copyright"), "PROMiDAT S.A.", icon = icon("copyright")),
                       infoBoxPROMiDAT(labelInput("info"), tags$a( href="https://www.promidat.com/", style = "color:white;",
                                                                   target = "_blank", "https://www.promidat.com"), icon = icon("info")),
                       infoBoxPROMiDAT(labelInput("version"), "1.1.7", icon = icon("file-code-o")))

# FULL PAGE ---------------------------------------------------------------------------------------------------------------

# This is the complete page that was built by pieces.
# If you want to see the construction of a specific page, go to the section
# corresponding.

shinyUI(
  dashboardPagePlus(
  title="PROMiDAT - RegressoR",
  dashboardHeaderPlus(
    title = tags$a(href="http://promidat.com", target = "_blank",
                   img(src="Logo2.png", height=55, width="100%",
                       id="imgPromidat"))),
  dashboardSidebar(mi.menu),
  dashboardBody(mi.head,
                load.page,
                tabItems(page.load.data,
                         page.numerical.summary,
                         page.test.normality,
                         page.distributions,
                         page.dispersion,
                         page.correlations,
                         pagina.poder,
                         page.rl,
                         page.rlr,
                         page.knn,
                         page.svm,
                         page.rd,
                         page.dt,
                         page.rf,
                         pagina.boosting,
                         page.nn,
                         page.comparison,
                         page.new.predictions,
                         page.generate.report,
                         page.info) )))
