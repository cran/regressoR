

# Edit > Folding > Collapse All (is of much help to visualize in an orderly way the code).

# The server for RegressoR
shinyServer(function(input, output, session){
  
  #If you want to see the package running, run regressoR::init_regressor()
  
  # The following variables belong to the server environment,
  # mean the following:
  # variable.predecir = The name of the variable to predict
  # datos = The full dataset
  # datos.prueba = The test dataset partition
  # datos.aprendizaje = The learning dataset partition
  # real.val = The values of the variable to predict (test data)
  
  # there are more variables in the "global" file but these are the most important ones.
  
  # SERVER UTILITY FUNCTIONS ----------------------------------------------------------------------------------------------
  
  # Update the different tables in the "shiny" application
  update_table <- function(x = c("datos", "datos.aprendizaje", "datos.prueba")){
    if(any("datos" %in% x)){ # Change data table
      output$contents <- render_table_data(datos, editable = T, server=F)
    }
    if(any("datos.aprendizaje" %in% x)){ # Change learning data table
      output$contentsAprend <- render_table_data(datos.aprendizaje, editable=T, scrollY="15vh", server=F)
    }
    if(any("datos.prueba" %in% x)){ # Change test data table
      output$contentsPrueba <- render_table_data(datos.prueba, editable = T, scrollY="15vh", server=F)
    }
  }

  # Close a menu in the "shiny" application according to your tabName
  close_menu <- function(tabname = NA, valor = T) {
    select <- paste0("a[href^='#shiny-tab-", tabname, "']")
    if(valor){
      shinyjs::hide(selector = "ul.menu-open")
      shinyjs::disable(selector = select)
    } else {
      shinyjs::enable(selector = select)
    }
  }

  # Common validation for all models
  validate_data <- function(print = TRUE) {
    if (is.null(variable.predecir) & print) {
      showNotification(translate("tieneVP"), duration = 10, type = "error")
    }
    if (is.null(datos) & print) {
      showNotification(translate("tieneD"), duration = 10, type = "error")
    }
    if (is.null(datos.aprendizaje) & print) {
      showNotification(translate("tieneDAP"), duration = 10, type = "error")
    }
    return(!is.null(datos) & !is.null(variable.predecir) & !is.null(datos.aprendizaje))
  }
  
  # INITIAL SETTINGS ------------------------------------------------------------------------------------------------------

  source("Utilities.R", local = TRUE, echo = FALSE )
  options_regressor(exe.envir = environment())
  
  clean_report()
  
  options(shiny.maxRequestSize = 209715200, width = 200, # 209715200 = 200 * 1024^2
          DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10, scrollX = TRUE, 
                            language = list(search = HTML('<i class="fa fa-search"></i>'),
                                            info = "", emptyTable = "", zeroRecords = "",
                                            paginate = list("previous" = HTML('<i class="fa fa-backward"></i>'),
                                                            "next" = HTML('<i class="fa fa-forward"></i>'),
                                                            "first" =HTML('<i class="fa fa-fast-backward"></i>'),
                                                            "last" = HTML('<i class="fa fa-fast-forward"></i>')) )))

  # The initial menu form
  shinyjs::disable(selector = 'a[href^="#shiny-tab-parte1"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-parte2"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-comparar"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-poderPred"]')
  shinyjs::disable(selector = 'a[href^="#shiny-tab-parte1"]')

  # Some code fields that are not parameter-dependent
  updateAceEditor(session, "fieldCodeResum", value = code_summary())
  updateAceEditor(session, "fieldModelCor" , value = cor_model())
  updateAceEditor(session, "fieldFuncNum"  , extract_code("numerical_distribution"))
  updateAceEditor(session, "fieldFuncCat"  , extract_code("categorical_distribution"))

  # REACTIVE VALUES -------------------------------------------------------------------------------------------------------

  updatePlot <- reactiveValues(calc.normal = default_calc_normal(), 
                               normal      = NULL, 
                               disp        = NULL,
                               cor         = NULL, 
                               dya.num     = NULL, 
                               dya.cat     = NULL, 
                               poder.pred  = NULL,
                               poder.cat   = NULL,
                               poder.num   = NULL, 
                               poder.dens  = NULL, 
                               tablaCom    = FALSE)

  disp.ranges <- reactiveValues(x = NULL, y = NULL)

  # PAGE TO LOAD AND TRANSFORM DATA ---------------------------------------------------------------------------------------

  # Executes the data upload code
  upload_data <- function(codigo.carga = "") {
    tryCatch({
      isolate(exe(codigo.carga))
      if(ncol(datos) <= 1) {
        showNotification(translate("errorCData"), duration = 10, type = "error")
        return(NULL)
      }
      new_report(datos.originales, input$file1$name)
    },
    error = function(e) {
      showNotification(paste0("Error: ", e), duration = 10, type = "error")
      datos <<- NULL
      datos.originales <<- NULL
      return(NULL)
    })
  }

  # Executes data cleanup code (cleaning of raw data)
  clean_data <- function(){
    if (any(is.na(datos))) {
      tryCatch({
        codigo.na <- paste0(code_NA(deleteNA = input$deleteNA), "\n", "datos <- datos.originales")
        isolate(exe(codigo.na))
        
        insert_report("na.delete", "Imputaci\u00F3n de Datos", codigo.na,"\nhead(datos)\nstr(datos)")
        
      }, error = function(e) {
        showNotification(paste0("Error (NA): ", e), duration = 10, type = "error")
        datos <<- NULL
        datos.originales <<- NULL
        return(NULL)
      })
    } else {
      codigo.na <- ""
    }
    return(codigo.na)
  }

  # Use and show the codes to transform the data
  transformar.datos <- function() {
    var.noactivas <- c()
    code.res <- "datos <- datos.originales \n"
    for (var in colnames(datos.originales)) {
      if (input[[paste0("box", var, contador)]]) {
        if (input[[paste0("sel", var, contador)]] == "categorico" & class(datos.originales[, var]) %in% c("numeric", "integer")) {
          code.res <- paste0(code.res, code_transf(var, "categorico"), "\n")
        }
        if (input[[paste0("sel", var, contador)]] == "numerico" & !(class(datos.originales[, var]) %in% c("numeric", "integer"))) {
          code.res <- paste0(code.res, code_transf(var, "numerico"), "\n")
        }
        if (input[[paste0("sel", var, contador)]] == "disyuntivo") {
          code.res <- paste0(code.res, code_transf(var, "disyuntivo"), "\n")
        }
      } else {
        var.noactivas <- c(var.noactivas, var)
      }
    }
    
    isolate(exe(code.res))
    code.res <- paste0(code.res, "\n")
    if (length(var.noactivas) > 0) {
      isolate(exe(code_deactivate(var.noactivas)))
      code.res <- paste0(code.res, code_deactivate(var.noactivas))
    }

    new_section_report()
    insert_report("transformar.datos","Transformando Datos", code.res,"\nstr(datos)")
    return(code.res)
  }

  # Update the different selectors
  aqualize_selecctors <- function() {
    updateSelectizeInput(session, "sel.normal", choices = colnames_empty(var_numerical(datos)))
    updateSelectizeInput(session, "select.var", choices = colnames_empty(var_numerical(datos)))
    updateSelectInput(session, "sel.distribucion.num", choices = colnames_empty(var_numerical(datos)))
    updateSelectInput(session, "sel.distribucion.cat", choices = colnames_empty(var_categorical(datos)))
    updateSelectInput(session, "sel.resumen", choices = colnames_empty(datos))
    updateSelectInput(session, "sel.predic.var", choices = rev(colnames_empty(var_numerical(datos))))
  }

  # Executes the code of correlations
  run_cor_model <- function() {
    tryCatch({
      isolate(exe(text = cor_model()))
      output$txtcor <- renderPrint(print(correlacion))
    }, error = function(e) {
      return(datos <- NULL)
    })
  }

  # Clears model data
  delete_models <- function(flag.datos = TRUE) {
    if (flag.datos) {
      datos.prueba <<- NULL
      datos.aprendizaje <<- NULL
      variable.predecir <<- NULL
      real.val <<- NULL
    }

    IndicesM <<- list()

    rm(list = nombres.modelos, envir = options_regressor()$exe.envir)
    nombres.modelos <<- c()

    updateCheckboxGroupButtons(session, inputId = "select.models",
                               choices = c(" ---- " = "NoDisponible"),
                               size = "sm", status = "primary",
                               checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                no = icon("remove", lib = "glyphicon")))

    updateSelectInput(session,"kernel.knn", selected = "optimal")
  }

  # When the load data button is pressed
  observeEvent(input$loadButton, {
    codigo.carga <- code_load(row.names = input$rowname, 
                              path = input$file1$datapath,
                              sep = input$sep, 
                              sep.dec = input$dec, 
                              header = input$header)

    upload_data(codigo.carga)

    codigo.na <- clean_data()

    updateAceEditor(session, "fieldCodeData", value = paste0(codigo.carga, "\n", codigo.na))

    aqualize_selecctors()

    run_cor_model()

    delete_models()

    close_menu("parte1"   , is.null(datos))
    close_menu("parte2"   , is.null(datos.aprendizaje))
    close_menu("comparar" , is.null(datos.aprendizaje))
    close_menu("poderPred", is.null(datos.aprendizaje))

    update_table()
  }, priority = 4)

  # When the button to transform data is pressed
  observeEvent(input$transButton, {
    code.res <- transformar.datos()

    updateAceEditor(session, "fieldCodeTrans", value = code.res)

    aqualize_selecctors()

    run_cor_model()

    delete_models()

    close_menu("parte1"   , is.null(datos))
    close_menu("parte2"   , is.null(datos.aprendizaje))
    close_menu("comparar" , is.null(datos.aprendizaje))
    close_menu("poderPred", is.null(datos.aprendizaje))

    update_table()
  }, priority = 4)

  # Show the select box of the panel to transform data
  update_trans <- eventReactive(input$loadButton, {
    contador <<- contador + 1
    if(!is.null(datos) && ncol(datos) > 0){
      res <- data.frame(Variables = colnames(datos), Tipo = c(1:ncol(datos)), Activa = c(1:ncol(datos)))
      res$Tipo <- sapply(colnames(datos), function(i)
        paste0('<select id="sel', i, contador, '"> <option value="categorico">',translate("categorico"),'</option>',
               '<option value="numerico" ', ifelse(class(datos[, i]) %in% c("numeric", "integer"),
                                                   ' selected="selected"', ""),'>', translate("numerico"),
               '</option> <option value="disyuntivo">',translate("disyuntivo"),'</option> </select>'))
      res$Activa <- sapply(colnames(datos), function(i) paste0('<input type="checkbox" id="box', i, contador, '" checked/>'))
    } else {
      res <- as.data.frame(NULL)
      showNotification(translate("tieneCData"), duration = 10, type = "error")
    }
    return(res)
  })

  # Change the table with the options of the transform panel
  output$transData <- DT::renderDT({sketch <- htmltools::withTags(table(tags$thead(tags$tr(tags$th(tags$span(`data-id` = "variables", "Variables")),
                                                                                           tags$th(tags$span(`data-id` = "tipo", "Tipo")),
                                                                                           tags$th(tags$span(`data-id` = "activa", "Activa"))))))
                                    DT::datatable(update_trans(),
                                          escape = FALSE, selection = "none", container = sketch,
                                          options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollY = "45vh"), rownames = F,
                                          callback = JS("table.rows().every(function(i, tab, row) {
                                                        var $this = $(this.node());
                                                        $this.attr('id', this.data()[0]);
                                                        $this.addClass('shiny-input-checkbox');});
                                                        Shiny.unbindAll(table.table().node());
                                                        Shiny.bindAll(table.table().node());"))
                                    }, server = FALSE)

  # The download of full data
  output$downloaDatos <- downloadHandler(
    filename = function() {
      input$file1$name
    },
    content = function(file) {
      write.csv(datos, file, row.names = input$rowname)
    }
  )

  # SPLIT DATA PAGE -------------------------------------------------------------------------------------------------------

  # Executes data segmentation code
  segmentar.datos <- function(codigo) {
    tryCatch({
      isolate(exe(codigo))
      updateAceEditor(session, "fieldCodeSegment", value = codigo)
    }, error = function(e) {
      showNotification(paste0(translate("errorSeg"), e), duration = 15, type = "error")
    })
  }

  # When the segment data button is pressed
  observeEvent(input$segmentButton, {
    if(input$sel.predic.var != ""){
      codigo <- partition_code("datos", input$segmentacionDatosA,
                               input$sel.predic.var,
                               input$semilla,
                               input$permitir.semilla)

      semilla       <<- input$permitir.semilla
      knn.stop.excu <<- FALSE
      rf.stop.excu  <<- FALSE

      segmentar.datos(codigo)

      new_section_report()
      insert_report("segmentar.datos","Datos de Aprendizaje",codigo, "\nhead(datos.aprendizaje)", interpretation = FALSE)
      insert_report("segmentar.datos","Datos de Prueba","head(datos.prueba)", add = TRUE, interpretation = FALSE)

      delete_models(FALSE)

      # change model codes
      deafult_codigo_rl()
      deafult_codigo_rlr()
      default_codigo_knn(k.def = TRUE)
      default_codigo_svm()
      deafult_codigo_rd()
      default_codigo_dt()
      deafult_codigo_rf(rf.def = TRUE)
      deault_codigo_boosting()
      default_codigo_nn()

    } else {
      showNotification(translate("tieneSVP"), duration = 15, type = "error")
    }

    close_menu("parte2",    is.null(datos.aprendizaje))
    close_menu("comparar",  is.null(datos.aprendizaje))
    close_menu("poderPred", is.null(datos.aprendizaje))
    
    update_table(c("datos.aprendizaje", "datos.prueba"))
    
  },priority = 5)

  # When user press enable or disable the seed
  observeEvent(input$permitir.semilla, {
    if (input$permitir.semilla) {
      shinyjs::enable("semilla")
    } else {
      shinyjs::disable("semilla")
    }
  })

  # When the data provider bar changes (Learning Data)
  observeEvent(input$segmentacionDatosA, {
    updateSliderInput(session, "segmentacionDatosT", value = 100 - input$segmentacionDatosA)
  })

  # When the data provider bar changes (Test Data)
  observeEvent(input$segmentacionDatosT, {
    updateSliderInput(session, "segmentacionDatosA", value = 100 - input$segmentacionDatosT)
  })

  # Download the learning table
  output$downloaDatosA <- downloadHandler(
    filename = function(){
      paste0("(",translate("dataA"),")",input$file1$name)
    },
    content = function(file) {
      write.csv(datos.aprendizaje, file, row.names = input$rowname)
    }
  )

  # Download the test table
  output$downloaDatosP <- downloadHandler(
    filename = function() {
      paste0("(",translate("dataP"),")",input$file1$name)
    },
    content = function(file) {
      write.csv(datos.prueba, file, row.names = input$rowname)
    }
  )

  # SUMMARY PAGE ----------------------------------------------------------------------------------------------------------

  # Change the table with the summary on the summary page
  output$resumen.completo <- DT::renderDataTable({ insert_report("resumen","Resumen Num\u00E9rico", "summary(datos)")
                                                   data.frame(unclass(summary(datos)), check.names = FALSE, stringsAsFactors = FALSE)
                                                 }, options = list(dom = "ft", scrollX = TRUE), rownames = F)

  # Change summary tables by variable
  output$resumen <- renderUI({
    if (input$sel.resumen %in% colnames(var_numerical(datos))){
      numerical_summary(datos, input$sel.resumen)
    }else{
      categorical_summary(datos, input$sel.resumen)
    }
  })

  # NORMALITY TEST PAGE ---------------------------------------------------------------------------------------------------

  # Show the graph of the normality test page
  observeEvent(c(input$loadButton, input$transButton), {
   output$plot.normal <- renderPlot({
      tryCatch({
        cod.normal <<- updatePlot$normal
        res <- isolate(exe(cod.normal))
        updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
        insert_report(paste0("normalidad.", input$sel.normal), "Test de Normalidad", cod.normal)
        return(res)
      }, error = function(e){
        if(ncol(var_numerical(datos)) <= 0){
          error_variables( T)
        } else {
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })

  # Change the code in the code field
  observeEvent(input$run.normal, {
    updatePlot$normal <- input$fieldCodeNormal
  })

  # Executes the code when parameters change
  observeEvent(c(input$sel.normal, input$col.normal), {
    updatePlot$normal <- normal_default(data = "datos", vars = input$sel.normal, color = input$col.normal, translate("curvanormal"))
  })

  # Show the comparative table of the normality test page
  observeEvent(c(input$loadButton, input$transButton), {
    output$calculo.normal <- DT::renderDT({
      tryCatch({
        codigo <- updatePlot$calc.normal
        res    <- isolate(exe(codigo))
        updateAceEditor(session, "fieldCalcNormal", value = codigo)
        fisher    <- translate("fisher")
        asimetria <- translate("asimetria")
        sketch = htmltools::withTags(table(tags$thead(tags$tr(tags$th(), tags$th(fisher), tags$th(asimetria)))))
        DT::datatable(res, selection = 'none', container = sketch, options = list(dom = 'frtip', scrollY = "40vh"))
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  # Run the comparison table
  observeEvent(input$run.calc.normal, {
    updatePlot$calc.normal <- input$fieldCalcNormal
  })

  # DISPERSION PAGE -------------------------------------------------------------------------------------------------------

  # Show the scatter plot
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.disp <- renderPlot({
      tryCatch({
        cod.disp <<- updatePlot$disp
        updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
        if(!is.null(cod.disp) && cod.disp != "") {
          insert_report(paste0("dispersion.", paste(input$select.var, collapse = ".")), "Dispersi\u00F3n", cod.disp)
        }
        return(isolate(exe(cod.disp)))
      }, error = function(e) {
        if(ncol(var_numerical(datos)) <= 1){
          error_variables( T)
        } else {
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })

  # Show the zoom graph
  output$plot.disp.zoom <- renderPlot({
    tryCatch({
      cod.disp <<- updatePlot$disp
      res <- isolate(exe(cod.disp))
      res <- res + coord_cartesian(xlim = disp.ranges$x, ylim = disp.ranges$y, expand = FALSE)
      return(res)
    }, error = function(e) {
      return(NULL)
    })
  })

  # Show the table with the dispersion values
  output$mostrar.disp.zoom <- DT::renderDataTable({
    tryCatch({
      return(brushedPoints(datos[, input$select.var], input$zoom.disp))
    }, error = function(e) {
      return(NULL)
    })
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh", pageLength = nrow(datos)))

  # When a zoom area is selected
  observe({
    brush <- input$zoom.disp
    if (!is.null(brush)) {
      disp.ranges$x <- c(brush$xmin, brush$xmax)
      disp.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      disp.ranges$x <- NULL
      disp.ranges$y <- NULL
    }
  })

  # Change the graphic code
  observeEvent(input$run.disp, {
    updatePlot$disp <- input$fieldCodeDisp
  })

  # Executes the code when parameters change
  observeEvent(c(input$select.var, input$col.disp), {
    if (length(input$select.var) < 2) {
      updatePlot$disp <- ""
    } else {
      updatePlot$disp <<- default_disp(data = "datos", vars = input$select.var, color = input$col.disp)
    }
  })

  # DISTRIBUTION PAGE -----------------------------------------------------------------------------------------------------

  # Show the graph of numerical distribution
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.num <- renderPlot({
      tryCatch({
        cod.dya.num <<- updatePlot$dya.num
        res <- isolate(exe(cod.dya.num))
        updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
        insert_report(paste0("dya.num.", input$sel.distribucion.num), "Distribuci\u00F3n y atipicidad", cod.dya.num)
        
        return(res)
      }, error = function(e) {
        if (ncol(var_numerical(datos)) == 0){
          error_variables( T)
        }else{
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })

  # Execute the code of the numerical chart
  observeEvent(input$run.dya.num, {
    updatePlot$dya.num <- input$fieldCodeNum
  })

  # Executes the code when parameters change
  observeEvent(c(input$sel.distribucion.num, input$col.dist), {
    updatePlot$dya.num <<- def_code_num(data = "datos", color = input$col.dist,
                                        variable = input$sel.distribucion.num)
  })
  
  # Creates the atypical table
  observeEvent(c(input$distribucion_numerica), {
    output$mostrarAtipicos <- DT::renderDataTable({
      atipicos <- boxplot.stats(datos[, input$sel.distribucion.num])
      datos <- datos[datos[, input$sel.distribucion.num] %in% atipicos$out, input$sel.distribucion.num, drop = F]
      datos <- datos[order(datos[, input$sel.distribucion.num]), , drop = F]
      datatable(datos, options = list(dom = 't', scrollX = TRUE, scrollY = "28vh",pageLength = nrow(datos))) %>%
        formatStyle(1, color = "white", backgroundColor = "#CBB051", target = "row")
    })
  })
  
  # Show the graph of categorical distribution
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.cat <- renderPlot({
      tryCatch({
        cod.dya.cat <<- updatePlot$dya.cat
        res <- isolate(exe(cod.dya.cat))
        updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
        insert_report(paste0("dya.cat.", input$sel.distribucion.cat), "Distribuci\u00F3n", cod.dya.cat)
        return(res)
      }, error = function(e) {
        if (ncol(var_categorical(datos)) == 0){
          error_variables( T)
        }else{
          showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
          return(NULL)
        }
      })
    })
  })

  # Change the code of the categorical graphic
  observeEvent(input$run.dya.cat, {
    updatePlot$dya.cat <- input$fieldCodeCat
  })

  # Executes the code when parameters change
  observeEvent(input$sel.distribucion.cat, {
    updatePlot$dya.cat <<- def_code_cat(variable = input$sel.distribucion.cat)
  })

  # CORRELATION PAGE ------------------------------------------------------------------------------------------------------

  # Show the correlation graph
  observeEvent(c(input$loadButton, input$transButton, input$fieldModelCor), {
    output$plot.cor <- renderPlot({
      tryCatch({
        cod.cor <<- updatePlot$cor
        res <- isolate(exe(cod.cor))
        updateAceEditor(session, "fieldCodeCor", value = cod.cor)
        insert_report("correlacion", "Correlaci\u00F3n", cor_model(),"\n", cod.cor)
        return(res)
      }, error = function(e) {
        if (ncol(var_numerical(datos)) == 0){
          error_variables( T)
        }else{
          showNotification(paste0("ERROR EN Correlacion: ", e),
                           duration = 10,
                           type = "error")
          return(NULL)
        }
      })
    })
  })

  # Change the graphic code
  observeEvent(input$run.code.cor, {
    updatePlot$cor <- input$fieldCodeCor
  })

  # Executes the code when parameters change
  observeEvent(c(input$cor.metodo, input$cor.tipo), {
    updatePlot$cor <- correlations_plot(method = input$cor.metodo, type = input$cor.tipo)
  })

  # PREDICTIVE POWER PAGE -------------------------------------------------------------------------------------------------

  # Show the graph of numerical predictive power
  observeEvent(input$segmentButton,{
    output$plot.pairs.poder <- renderPlot({
      tryCatch({
        cod.poder.num <<- updatePlot$poder.num
        updateAceEditor(session, "fieldCodePoderNum", value = cod.poder.num)
        if (ncol(var_numerical(datos)) >= 2) {
          if(ncol(var_numerical(datos)) <= 25){
            res <- isolate(exe(cod.poder.num))
            insert_report("poder.num","Poder Predictivo Variables Num\u00E9ricas", cod.poder.num)
            return(res)
          }else{
            showNotification(translate("bigPlot"), duration = 10, type = "message")
            return(NULL)
          }
        }else{
          error_variables( T)
        }
      }, error = function(e) {
        showNotification(paste0("Error en Poder Predictivo: ", e),
                         duration = 10,
                         type = "error")
        return(NULL)
      })
    })
  })

  # Execute the graphic code
  observeEvent(input$run.code.poder.num, {
    if(input$fieldCodePoderNum != "") {
      updatePlot$poder.num <- input$fieldCodePoderNum
    } else {
      updatePlot$poder.num <- pairs_power()
    }
  })

  # Change the graphic code
  observeEvent(input$segmentButton,{
    updatePlot$poder.num <- pairs_power()
  }, priority = 3)
  
  # RL PAGE ---------------------------------------------------------------------------------------------------------------
  
  # When the rl model is generated
  observeEvent(input$runRl, {
    if (validate_data()) { # Si se tiene los datos entonces :
      rl_full()
    }
  })
  
  # Upgrade code fields to default version
  deafult_codigo_rl <- function(){
    # Se acualiza el codigo del modelo
    codigo <- rl_model(variable.pred = variable.predecir)
    
    updateAceEditor(session, "fieldCodeRl", value = codigo)
    cod.rl.modelo <<- codigo
    
    #Se genera el codigo de los coeficientes
    codigo <- rl_coeff()
    updateAceEditor(session, "fieldCodeRlCoef", value = codigo)
    
    
    # Se genera el codigo de la prediccion
    codigo <- rl_prediction()
    updateAceEditor(session, "fieldCodeRlPred", value = codigo)
    cod.rl.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- disp_models("prediccion.rl", translate("rll"), variable.predecir)
    updateAceEditor(session, "fieldCodeRlDisp", value = codigo)
    
    # Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeRlIG", value = codigo)
    cod.rl.ind <<- codigo
  }
  
  # Cleans the data according to the process where the error is generated
  clean_rl <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        modelo.rl <<- NULL
        output$txtRl <- renderPrint(invisible(""))
        remove_report_elem("modelo.rl")
        remove_report_elem("disp.rl")
      }, {
        prediccion.rl <<- NULL
        remove_report_elem("pred.rl")
        output$rlPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.rl <<- rep(0, 10)
        remove_report_elem("ind.rl")
      })
    }
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_rl <- function(){
    tryCatch({ # Se corren los codigo
      output$plot.rl.disp <- renderPlot(exe(input$fieldCodeRlDisp))
      insert_report("disp.rl", "Dispersi\u00F3n del Modelo Regresi\u00F3n Lineal", input$fieldCodeRlDisp)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rl(2)
      showNotification(paste0("Error (RL-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Displays model coefficients
  coefficients_rl <- function(){
    tryCatch({ # Se corren los codigo
      isolate(exe(input$fieldCodeRlCoef))
      
      output$rlCoefTable <- regressoR::render_table_data(df.rl[,c(1,4)], server = FALSE)
      insert_report("coeff.rl", "Coeficientes del Modelo Regresi\u00F3n Lineal", input$fieldCodeRlCoef, "\ndf.rl")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rl(1)
      showNotification(paste0("Error (RL-01) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  rl_full <- function(){
    execute_rl()
    execute_rl_pred()
    execute_rl_ind()
  }
  
  # Generates the model
  execute_rl <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rl.modelo))
      output$txtRl <- renderPrint(print(summary(modelo.rl)))
      
      insert_report("modelo.rl","Generaci\u00F3n del Modelo Regresi\u00F3n Lineal", cod.rl.modelo,"\nsummary(modelo.rl)")
      coefficients_rl()

      nombres.modelos <<- c(nombres.modelos, "modelo.rl")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rl(1)
      showNotification(paste0("Error (RL-01) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generate the prediction
  execute_rl_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rl.pred))
      
      output$rlPrediTable <- DT::renderDataTable(tb_predic(real.val, prediccion.rl), server = FALSE)
      
      insert_report("pred.rl", "Predicci\u00F3n del Modelo Regresi\u00F3n Lineal",cod.rl.pred,
                    "\nkt(head(tb_predic(real.val, prediccion.rl)$x$data[,-1]))", interpretation = FALSE)
      
      plot_disp_rl()
      
      nombres.modelos <<- c(nombres.modelos, "prediccion.rl")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rl(2)
      showNotification(paste0("Error (RL-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_rl_ind <- function() {
    if(exists("prediccion.rl") && !is.null(prediccion.rl)){
      tryCatch({ # Se corren los codigo
        isolate(exe(input$fieldCodeRlCoef))
        isolate(exe(cod.rl.ind))
        
        indices.rl <- general_indices(datos.prueba[,variable.predecir], prediccion.rl)
        
        insert_report("ind.rl","\u00CDndices Generales del Modelo Regresi\u00F3n Lineal", cod.rl.ind,
                      "\nkt(general_indices(datos.prueba[,'", variable.predecir, "'], prediccion.rl))\n",
                      "indices.rl<- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.rl)\n",
                      "IndicesM[['rll']] <- indices.rl")
        
        df <- cbind(as.data.frame(indices.rl), r2)
        df <- df[,c(1,2,3,5,4)]
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("R2"), translate("correlacion"))
        output$indexdfrl <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfrl2 <- render_index_table(df2)
        
        nombres.modelos <<- c(nombres.modelos, "indices.rl")
        IndicesM[["rll"]] <<- indices.rl
        update_comparative_selector()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_rl(3)
        showNotification(paste0("Error (RL-03) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # RLR PAGE --------------------------------------------------------------------------------------------------------------
  
  # When the rlr model is generated
  observeEvent(input$runRlr, {
    if (validate_data()) { # If you have the data then :
      rlr_full()
    }
  })
  
  # When the user changes the parameters
  observeEvent(c(input$alpha.rlr, input$switch.scale.rlr, input$landa, input$permitir.landa), {
    if (validate_data(print = FALSE)) {
      options_regressor(rlr.alpha = input$alpha.rlr)
      deafult_codigo_rlr()
    }
  })
  
  # When user press enable or disable the lambda
  observeEvent(input$permitir.landa, {
    if (input$permitir.landa) {
      shinyjs::enable("landa")
    } else {
      shinyjs::disable("landa")
    }
  })

  # Upgrade code fields to default version
  deafult_codigo_rlr <- function(){
    landa <- NULL
    
    if (input$permitir.landa) {
      if (!is.na(input$landa)) {
        landa <- input$landa
      }
    }
    
    # The model code is updated
    codigo <- rlr_model(variable.pred = variable.predecir,
                         model.var = paste0("modelo.rlr.", rlr_type()),
                         cv.var = paste0("cv.glm.", rlr_type()),
                         alpha = input$alpha.rlr,
                         standardize = input$switch.scale.rlr)

    updateAceEditor(session, "fieldCodeRlr", value = codigo)
    cod.rlr.modelo <<- codigo

    # The code of the possible landa is generated
    codigo <- paste0("plot(cv.glm.", rlr_type(),")")
    updateAceEditor(session, "fieldCodeRlrPosibLanda", value = codigo)
    cod.select.landa <<- codigo

    # The code that prints the coefficients is generated
    codigo <- coef_lambda(variable.pred = variable.predecir,
                          model.var = paste0("modelo.rlr.", rlr_type()),
                          lambda = landa,
                          cv.var = paste0("cv.glm.", rlr_type()))
    
    updateAceEditor(session, "fieldCodeRlrCoeff", value = codigo)
    
    # The code of the coefficients is generated with the best lambda
    codigo <- plot_coef_lambda(model.var = paste0("modelo.rlr.", rlr_type()),
                               lambda = landa,
                               cv.var = paste0("cv.glm.", rlr_type()))
    
    updateAceEditor(session, "fieldCodeRlrLanda", value = codigo)
    
    # The prediction code is generated
    codigo <- rlr_prediction(variable.pred = variable.predecir,
                             model.var = paste0("modelo.rlr.", rlr_type()),
                             pred.var = paste0("prediccion.rlr.", rlr_type()),
                             lambda = landa,
                             cv.var =  paste0("cv.glm.", rlr_type()))
    
    updateAceEditor(session, "fieldCodeRlrPred", value = codigo)
    cod.rlr.pred <<- codigo

    # The dispersion code is generated
    codigo <- disp_models(paste0("prediccion.rlr.",rlr_type()), translate("rlr"), variable.predecir)
    updateAceEditor(session, "fieldCodeRlrDisp", value = codigo)

    # The index code is generated
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeRlrIG", value = codigo)
    cod.rlr.ind <<- codigo
  }

  # Cleans the data according to the process where the error is generated
  clean_rlr <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        modelo.rlr <<- NULL
        output$txtRlr <- renderPrint(invisible(""))
        remove_report_elem(paste0("modelo.rlr.",rlr_type()))
        remove_report_elem(paste0("disp.rlr.",rlr_type()))
        remove_report_elem(paste0("landa.rlr.",rlr_type()))
      }, {
        prediccion.rlr <<- NULL
        remove_report_elem(paste0("pred.rlr.",rlr_type()))
        output$rlrPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.rlr <<- rep(0, 10)
        remove_report_elem(paste0("ind.rlr",rlr_type()))
      })
    }
  }

  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_rlr <- function(){
    tryCatch({ # Se corren los codigo
      codigo <- input$fieldCodeRlrDisp
      output$plot.rlr.disp <- renderPlot(isolate(exe(codigo)))
      insert_report(paste0("disp.rlr.",rlr_type()), paste0("Dispersi\u00F3n del Modelo Regresi\u00F3n Penalizada (",rlr_type(),")"), codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-02) : ", e), duration = 15, type = "error")
    })
  }

  # Show the graph of the possible lambda
  plot_posib_landa_rlr <- function(){
    tryCatch({ # Se corren los codigo
      output$plot.rlr.posiblanda <- renderPlot(exe("plot(cv.glm.",rlr_type(),")"))
      insert_report(paste0("posib.landa.rlr.",rlr_type()), paste0("Posible lambda (",rlr_type(),")"),cod.select.landa,"\nplot(cv.glm.",rlr_type(),")")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-01) : ", e), duration = 15, type = "error")
    })
  }

  # Displays coefficients as text
  print_coeff <- function(){
    tryCatch({ # Se corren los codigo
      codigo <- input$fieldCodeRlrCoeff
      output$txtRlrCoeff <- renderPrint(print(isolate(exe(codigo))))
      insert_report(paste0("coeff.landa.rlr.",rlr_type()),paste0("Coeficientes (",rlr_type(),")"), codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-01) : ", e), duration = 15, type = "error")
    })
  }
  
  # Show the graph of the coefficients
  plot_coeff <- function(){
    tryCatch({ # Se corren los codigo
      landa <- NULL
      
      if (input$permitir.landa) {
        if (!is.na(input$landa)) {
          landa <- input$landa
        }
      }
      
      codigo <- plot_coef_lambda(model.var = paste0("modelo.rlr.", rlr_type()),
                                 lambda = landa,
                                 cv.var = paste0("cv.glm.", rlr_type()))
      
      #codigo <- input$fieldCodeRlrLanda
      output$plot.rlr.landa <- renderPlot(isolate(exe(codigo)))
      insert_report(paste0("gcoeff.landa.rlr.",rlr_type()),paste0("Coeficientes y lamdas (",rlr_type(),")"),codigo)
    },
    error = function(e){ # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-01) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  rlr_full <- function(){
    execute_rlr()
    execute_rlr_pred()
    execute_rlr_ind()
  }
   
  # Generates the model
  execute_rlr <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rlr.modelo))
      isolate(tipo <- rlr_type())
      output$txtRlr <- renderPrint(print(exe("modelo.rlr.",tipo)))

      insert_report(paste0("modelo.rlr.",tipo),paste0("Generaci\u00F3n del Modelo Regresi\u00F3n Penalizada (",rlr_type(),")"),
                    cod.rlr.modelo,"\nmodelo.rlr.",tipo)

      plot_posib_landa_rlr()
      print_coeff()
      plot_coeff()
      
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.rlr.",tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(1)
      showNotification(paste0("Error (R/L-01) : ",e), duration = 15, type = "error")
    })
  }

  # Generate the prediction
  execute_rlr_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rlr.pred))
      isolate(tipo <- rlr_type())
      output$rlrPrediTable <- DT::renderDataTable(tb_predic(real.val, exe("prediccion.rlr.",tipo)), server = FALSE)

      insert_report(paste0("pred.rlr.",tipo), paste0("Predicci\u00F3n del Modelo Regresi\u00F3n Penalizada (",rlr_type(),")"),
                    cod.rlr.pred,"\nkt(head(tb_predic(real.val, prediccion.rlr.",tipo,")$x$data[,-1]))", interpretation = FALSE)

      plot_disp_rlr()
      nombres.modelos <<- c(nombres.modelos, "prediccion.rlr")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rlr(2)
      showNotification(paste0("Error (R/L-02) : ", e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_rlr_ind <- function() {
    if(exists(paste0("prediccion.rlr.",rlr_type()))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rlr.ind))

        indices.rlr <- general_indices(datos.prueba[,variable.predecir], exe("prediccion.rlr.",rlr_type()))
        
        insert_report(paste0("ind.rlr.",rlr_type()),paste0("\u00CDndices Generales del Modelo Regresi\u00F3n Penalizada (",rlr_type(),")"),
                      cod.rlr.ind, "\nkt(general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.rlr.",rlr_type(),"))\n",
                      "indices.rlr <- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.rlr.",rlr_type(),")\n",
                      "IndicesM[['rlr-",rlr_type(),"']] <- indices.rlr")
        
        df <- as.data.frame(indices.rlr)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfrlr <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfrlr2 <- render_index_table(df2)

        IndicesM[[paste0("rlr-",rlr_type())]] <<- indices.rlr
        update_comparative_selector()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_rlr(3)
        showNotification(paste0("Error (R/L-03) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # KNN PAGE --------------------------------------------------------------------------------------------------------------

  # When the knn model is generated
  observeEvent(input$runKnn, {
    if (validate_data()) { # Si se tiene los datos entonces :
      knn_full()
    }
  })

  # When the user changes the parameters
  observeEvent(c(input$switch.scale.knn, input$kmax.knn, input$kernel.knn, input$distance.knn), {
    if (validate_data(print = FALSE) & knn.stop.excu) {
      default_codigo_knn()
    }else{
      knn.stop.excu <<- TRUE
    }
  })

  # Upgrade code fields to default version
  default_codigo_knn <- function(k.def = FALSE) {
    if(!is.null(datos.aprendizaje) & k.def){
      k.value <- ifelse(k.def, round(sqrt(nrow(datos.aprendizaje))), input$kmax.knn)
      updateNumericInput(session,"kmax.knn",value = k.value)
    }else{
      k.value <- input$kmax.knn
    }

    # Se acualiza el codigo del modelo
    codigo <- kkn_model(variable.pred = variable.predecir,
                        scale = input$switch.scale.knn,
                        kmax = k.value,
                        kernel = input$kernel.knn,
                        model.var = paste0("modelo.knn.", input$kernel.knn),
                        distance = input$distance.knn)
    
    updateAceEditor(session, "fieldCodeKnn", value = codigo)
    cod.knn.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- kkn_prediction(variable.pred = variable.predecir,
                             model.var = paste0("modelo.knn.", input$kernel.knn), 
                             pred.var  = paste0("prediccion.knn.", input$kernel.knn))
    
    updateAceEditor(session, "fieldCodeKnnPred", value = codigo)
    cod.knn.pred <<- codigo

    # Se genera el codigo de la dispersion
    codigo <- disp_models(paste0("prediccion.knn.", input$kernel.knn), translate("knnl"), variable.predecir)
    updateAceEditor(session, "fieldCodeKnnDisp", value = codigo)

    # Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeKnnIG", value = codigo)
    cod.knn.ind <<- codigo
  }

  # Cleans the data according to the process where the error is generated
  clean_knn <- function(capa = NULL) {
    for (i in capa:3) {
      switch(i, {
        exe("modelo.knn.",input$kernel.knn," <<- NULL")
        output$txtknn <- renderPrint(invisible(""))
        remove_report_elem(paste0("modelo.knn.",input$kernel.knn))
      }, {
        exe("prediccion.knn.",input$kernel.knn," <<- NULL")
        remove_report_elem(paste0("pred.knn.",input$kernel.knn))
        output$knnPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("indices.knn.",input$kernel.knn," <<- NULL")
        remove_report_elem(paste0("ind.knn.",input$kernel.knn))
      })
    }
  }

  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_knn <- function(){
    tryCatch({ # Se corren los codigo
      codigo <- input$fieldCodeKnnDisp
      isolate(kernel <- input$kernel.knn)
      output$plot.knn.disp <- renderPlot(exe(codigo))
      insert_report(paste0("disp.knn.",kernel), paste0("Dispersi\u00F3n del Modelo KNN (",kernel,")"), codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_knn(2)
      showNotification(paste0("Error (KNN-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  knn_full <- function() {
    execute_knn()
    execute_knn_pred()
    execute_knn_ind()
  }

  # Generates the model
  execute_knn <- function() {
    tryCatch({
      exe(cod.knn.modelo)
      isolate(kernel <- input$kernel.knn)
      updateAceEditor(session, "fieldCodeKnn", value = cod.knn.modelo)
      output$txtknn <- renderPrint(exe("modelo.knn.",kernel))
      insert_report(paste0("modelo.knn.",kernel), paste0("Generaci\u00F3n del Modelo KNN (",kernel,")"),cod.knn.modelo,"\nmodelo.knn.", kernel)

      nombres.modelos <<- c(nombres.modelos, paste0("modelo.knn.",kernel))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_knn(1)
      showNotification(paste0("Error (KNN-01) : ", e), duration = 15, type = "error")
    }
    )
  }

  # Generate the prediction
  execute_knn_pred <- function() {
    tryCatch({ # Se corren los codigo
      exe(cod.knn.pred)
      isolate(kernel <- input$kernel.knn)
      
      # Cambia la tabla con la prediccion de knn
      output$knnPrediTable <- DT::renderDataTable(tb_predic(real.val, exe("prediccion.knn.",kernel)), server = FALSE)
      insert_report(paste0("pred.knn.",kernel), paste0("Predicci\u00F3n del Modelo KNN (",kernel,")"), 
                    cod.knn.pred,"\nkt(head(tb_predic(real.val, prediccion.knn.",kernel,")$x$data[,-1]))", interpretation = FALSE)

      plot_disp_knn()
      nombres.modelos <<- c(nombres.modelos, paste0("prediccion.knn.",kernel))
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_knn(2)
      showNotification(paste0("Error (KNN-02) : ", e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_knn_ind <- function(){
    if(exists(paste0("prediccion.knn.",input$kernel.knn))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.knn.ind))
        isolate(kernel <- input$kernel.knn)
        
        indices.knn <- general_indices(datos.prueba[,variable.predecir], exe("prediccion.knn.",kernel))
        #eval(parse(text = paste0("indices.knn.",kernel, "<<- indices.knn")))

        insert_report(paste0("ind.knn.",kernel), paste0("\u00CDndices del Modelo KNN (",kernel,")"),
                      cod.knn.ind, "\nkt(general_indices(datos.prueba[,'",variable.predecir,"'] ,prediccion.knn.",kernel,"))\n",
                      "indices.knn <- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.knn.",kernel,")\n",
                      "IndicesM[['knnl-",kernel,"']] <- indices.knn")
        
        df <- as.data.frame(indices.knn)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfknn <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfknn2 <- render_index_table(df2)

        #nombres.modelos <<- c(nombres.modelos, paste0("indices.knn.",kernel))
        IndicesM[[paste0("knnl-",kernel)]] <<- indices.knn
        update_comparative_selector()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_knn(3)
        showNotification(paste0("Error (KNN-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # SVM PAGE --------------------------------------------------------------------------------------------------------------

  # When the knn model is generated
  observeEvent(input$runSvm, {
    if (validate_data()) { # Si se tiene los datos entonces :
      svm_full()
    }
  })

  # When the user changes the parameters
  observeEvent(c(input$switch.scale.svm, input$kernel.svm), {
    if (validate_data(print = FALSE)){
      default_codigo_svm()
    }
  })

  # Upgrade code fields to default version
  default_codigo_svm <- function() {
    # Se acualiza el codigo del modelo
    codigo <- svm_model(variable.pred = variable.predecir,
                        model.var = paste0("modelo.svm.",input$kernel.svm),
                        scale = input$switch.scale.svm,
                        kernel = input$kernel.svm)

    updateAceEditor(session, "fieldCodeSvm", value = codigo)
    cod.svm.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- svm_prediction(variable.pred = variable.predecir,
                             model.var = paste0("modelo.svm.",input$kernel.svm),
                             pred.var = paste0("prediccion.svm.",input$kernel.svm))
    updateAceEditor(session, "fieldCodeSvmPred", value = codigo)
    cod.svm.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- disp_models(paste0("prediccion.svm.",input$kernel.svm), translate("svml"), variable.predecir)
    updateAceEditor(session, "fieldCodeSvmDisp", value = codigo)

    # Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeSvmIG", value = codigo)
    cod.svm.ind <<- codigo
  }

  # Cleans the data according to the process where the error is generated
  clean_svm <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        exe("modelo.svm.",input$kernel.svm,"<<- NULL")
        output$txtSvm <- renderPrint(invisible(""))
        remove_report_elem(paste0("modelo.svm.",input$kernel.svm))
      }, {
        exe("prediccion.svm.",input$kernel.svm,"<<- NULL")
        remove_report_elem(paste0("pred.svm.",input$kernel.svm))
        output$svmPrediTable <- DT::renderDataTable(NULL)
      }, {
        exe("indices.svm.",input$kernel.svm,"<<- NULL")
        remove_report_elem(paste0("ind.svm.",input$kernel.svm))
      })
    }
  }

  # Execute model, prediction and indices
  svm_full <- function() {
    execute_svm()
    execute_svm_pred()
    execute_svm_ind()
  }

  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_svm <- function(){
    tryCatch({ # Se corren los codigo
      isolate(kernel <- input$kernel.svm)
      codigo <- input$fieldCodeSvmDisp
      output$plot.svm.disp <- renderPlot(exe(codigo))
      insert_report(paste0("disp.svm.", kernel), paste0("Dispersi\u00F3n del Modelo SVM (",kernel,")"), codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_svm(2)
      showNotification(paste0("Error (SVM-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Generates the model
  execute_svm <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.svm.modelo))
      isolate(kernel <- input$kernel.svm)
      output$txtSvm <- renderPrint(exe("print(modelo.svm.",kernel,")"))
      updateAceEditor(session, "fieldCodeSvm", value = cod.svm.modelo)

      insert_report(paste0("modelo.svm.",kernel), paste0("Generaci\u00F3n del Modelo SVM (",kernel,")"), cod.svm.modelo, "\nmodelo.svm.", kernel)

      nombres.modelos <<- c(nombres.modelos, paste0("modelo.svm.", kernel))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_svm(1)
      showNotification(paste0("Error (SVM-01) : ",e), duration = 15, type = "error")
    })
  }

  # Generate the prediction
  execute_svm_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.svm.pred))
      isolate(kernel <- input$kernel.svm)
      
      # Cambia la tabla con la prediccion de knn
      output$svmPrediTable <- DT::renderDataTable(exe("tb_predic(real.val, prediccion.svm.",kernel,")"),server = FALSE)
      insert_report(paste0("pred.svm.",input$kernel.svm), paste0("Predicci\u00F3n del Modelo SVM (",kernel,")"), 
                    cod.svm.pred,"\nkt(head(tb_predic(real.val, prediccion.svm.",kernel,")$x$data[,-1]))",interpretation = FALSE)

      nombres.modelos <<- c(nombres.modelos, paste0("prediccion.svm.",kernel))

      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_svm(2)
      showNotification(paste0("Error (SVM-02) : ",e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_svm_ind <- function(){
    if(exists(paste0("prediccion.svm.",input$kernel.svm))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.svm.ind))
        isolate(kernel <- input$kernel.svm)
        
        indices.svm <- general_indices(datos.prueba[,variable.predecir], exe("prediccion.svm.",kernel))
        #eval(parse(text =paste0("indices.svm.",kernel, "<<- indices.svm")))

        insert_report(paste0("ind.svm.",kernel), paste0("\u00CDndices Generales del modelo SVM (",kernel,")"),
                      cod.svm.ind, "\nkt(general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.svm.",kernel,"))\n",
                      "indices.svm <- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.svm.",kernel,")\n",
                      "IndicesM[['svml-",kernel,"']] <- indices.svm")
        
        df <- as.data.frame(indices.svm)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfsvm <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfsvm2 <- render_index_table(df2)
        
        plot_disp_svm()
        #nombres.modelos <<- c(nombres.modelos, paste0("indices.svm.",kernel))
        IndicesM[[paste0("svml-",kernel)]] <<- indices.svm
        update_comparative_selector()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_svm(3)
        showNotification(paste0("Error (SVM-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # DT PAGE ---------------------------------------------------------------------------------------------------------------

  #  When the dt model is generated
  observeEvent(input$runDt, {
    if (validate_data()) { # Si se tiene los datos entonces :
      dt_full()
    }
  })

  # When the user changes the parameters
  observeEvent(c(input$minsplit.dt, input$maxdepth.dt), {
    if (validate_data(print = FALSE)){
      default_codigo_dt()
    }
  })

  # Upgrade code fields to default version
  default_codigo_dt <- function() {

    # Se acualiza el codigo del modelo
    codigo <- dt_model(variable.pred =  variable.predecir,
                        minsplit = input$minsplit.dt,
                        maxdepth = input$maxdepth.dt)

    updateAceEditor(session, "fieldCodeDt", value = codigo)
    cod.dt.modelo <<- codigo

    # Cambia el codigo del grafico del árbol
    updateAceEditor(session, "fieldCodeDtPlot", value = dt_plot())

    # Se genera el codigo de la prediccion
    codigo <- dt_prediction()
    updateAceEditor(session, "fieldCodeDtPred", value = codigo)
    cod.dt.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- disp_models("prediccion.dt", translate("dtl"), variable.predecir)
    updateAceEditor(session, "fieldCodeDtDisp", value = codigo)

    # Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeDtIG", value = codigo)
    cod.dt.ind <<- codigo
  }

  # Shows the graph of the tree
  plot_tree <- function(){
    tryCatch({
      output$plot.dt <- renderPlot(isolate(exe(input$fieldCodeDtPlot)))
      cod <- ifelse(input$fieldCodeDtPlot == "", dt_plot(), input$fieldCodeDtPlot)
      insert_report("modelo.dt.graf", "\u00C1rboles de Decisi\u00F3n", cod)
    },
    error = function(e){
      output$plot.dt <- renderPlot(NULL)
      remove_report_elem("modelo.dt.graf")
    })
  }

  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_dt <- function(){
    tryCatch({ # Se corren los codigo
      output$plot.dt.disp <- renderPlot(exe(input$fieldCodeDtDisp))
      insert_report("disp.dt", "Dispersi\u00F3n del Modelo \u00C1rboles de Decisi\u00F3n", input$fieldCodeDtDisp)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_dt(2)
      showNotification(paste0("Error (DT-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Shows the rules of the tree
  show_dt_rules <- function(){
    output$rulesDt <- renderPrint(rattle::asRules(modelo.dt))
    updateAceEditor(session, "fieldCodeDtRule", paste0("asRules(modelo.dt)"))
    insert_report("modelo.dt.rules", "Reglas del Modelo \u00C1rboles de Decisi\u00F3n", "rattle::asRules(modelo.dt)")
  }

  # Cleans the data according to the process where the error is generated
  clean_dt <- function(capa = NULL) {
    for (i in capa:3) {
      switch(i, {
        modelo.dt <<- NULL
        output$txtDt <- renderPrint(invisible(""))
        output$plot.dt <- renderPlot(NULL)
        remove_report_elem("modelo.dt")
        remove_report_elem("modelo.dt.graf")
        remove_report_elem("disp.dt")
      }, {
        prediccion.dt <<- NULL
        remove_report_elem("pred.dt")
        output$dtPrediTable <- DT::renderDataTable(NULL)
      }, {
        indices.dt <<- rep(0, 10)
        remove_report_elem("ind.dt")
      })
    }
  }

  # Execute model, prediction and indices
  dt_full <- function() {
    execute_dt()
    execute_dt_pred()
    execute_dt_ind()
  }

  # Generates the model
  execute_dt <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.dt.modelo))
      output$txtDt <- renderPrint(print(modelo.dt))
      insert_report("modelo.dt", "Generaci\u00F3n del modelo \u00C1rboles de Decisi\u00F3n", cod.dt.modelo, "\nmodelo.dt")
      plot_tree()
      show_dt_rules()
      nombres.modelos <<- c(nombres.modelos, "modelo.dt")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_dt(1)
      showNotification(paste0("Error (DT-01) : ",e), duration = 15, type = "error")
    })
  }

  # Generate the prediction
  execute_dt_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.dt.pred))
      # Cambia la tabla con la prediccion de dt
      output$dtPrediTable <- DT::renderDataTable(tb_predic(real.val, prediccion.dt),server = FALSE)

      insert_report("pred.dt", "Predicci\u00F3n del Modelo \u00C1rboles de Decisi\u00F3n", 
                    cod.dt.pred,"\nkt(head(tb_predic(real.val, prediccion.dt)$x$data[,-1]))",interpretation = FALSE)

      plot_disp_dt()
      nombres.modelos <<- c(nombres.modelos, "prediccion.dt")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_dt(2)
      showNotification(paste0("Error (DT-02) : ",e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_dt_ind <- function() {
    if(exists("prediccion.dt") && !is.null(prediccion.dt)){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.dt.ind))
        
        indices.dt <- general_indices(datos.prueba[,variable.predecir], prediccion.dt)
        
        insert_report("ind.dt", "\u00CDndices Generales del Modelo \u00C1rboles de Decisi\u00F3n",
                      cod.dt.ind,"\nkt(general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.dt))\n",
                      "indices.dt <- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.dt)\n",
                      "IndicesM[['dtl']] <- indices.dt")

        df <- as.data.frame(indices.dt)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfdt <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfdt2 <- render_index_table(df2)
        
        IndicesM[["dtl"]] <<- indices.dt
        update_comparative_selector()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_dt(3)
        showNotification(paste0("Error (DT-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # RD PAGE ---------------------------------------------------------------------------------------------------------------
  
  #  When the dt model is generated
  observeEvent(input$runRd, {
    if (validate_data()) { # Si se tiene los datos entonces :
      rd_full()
    }
  })
  
  # When the user changes the parameters
  observeEvent(c(input$modo.rd, input$switch.scale.rd, input$ncomp.rd, input$permitir.ncomp), {
    if (validate_data(print = FALSE)) {
      deafult_codigo_rd()
    }
  })
  
  # Habilitada o deshabilitada el número de componenetes 
  observeEvent(input$permitir.ncomp, {
    if (input$permitir.ncomp) {
      shinyjs::enable("ncomp.rd")
    } else {
      shinyjs::disable("ncomp.rd")
    }
  })
  
  # Upgrade code fields to default version
  deafult_codigo_rd <- function(){
    ncomp <- NULL
    if (input$permitir.ncomp) {
      if(!is.na(input$ncomp.rd) && input$ncomp.rd >= 0) {
        ncomp <- input$ncomp.rd
      }
    }
    
    options_regressor(rd.mode = input$modo.rd)
    
    # Se acualiza el codigo del modelo
    codigo <- rd_model(variable.pred = variable.predecir,
                       model.var = paste0("modelo.rd.",rd_type()),
                       n.comp = "n.comp.rd",
                       scale = input$switch.scale.rd)
    
    updateAceEditor(session, "fieldCodeRd", value = codigo)
    cod.rd.modelo <<- codigo
    
    # Se genera el codigo del plot de RMSE
    codigo <- extract_code("plot_RMSE")
    updateAceEditor(session, "fieldCodeRdRMSE", value = codigo)
    
    # Se genera el codigo del plot de predictoras
    codigo <- extract_code("plot_pred_rd")
    updateAceEditor(session, "fieldCodeRdPlotPred", value = codigo)
    
    # Se genera el codigo del plot de predictoras
    codigo <- extract_code("plot_var_pred_rd")
    updateAceEditor(session, "fieldCodeRdPlotVarPred", value = codigo)
    
    # Se genera el codigo de la prediccion
    codigo <- rd_prediction(model.var = paste0("modelo.rd.",rd_type()),
                            pred.var = paste0("prediccion.rd.",rd_type()),
                            n.comp  = "n.comp.rd",
                            ncomp = ncomp)
    updateAceEditor(session, "fieldCodeRdPred", value = codigo)
    cod.rd.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- disp_models(paste0("prediccion.rd.",rd_type()), translate("rd"), variable.predecir)
    updateAceEditor(session, "fieldCodeRdDisp", value = codigo)
    
    # Se genera el codigo de la indices 
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeRdIG", value = codigo)
    cod.rd.ind <<- codigo
  }
  
  # Cleans the data according to the process where the error is generated
  clean_rd <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        modelo.rd <<- NULL
        output$txtRd <- renderPrint(invisible(""))
        remove_report_elem(paste0("modelo.rd.",rd_type()))
        remove_report_elem(paste0("rmse.rd.",rd_type()))
        remove_report_elem(paste0("plot.pred.rd.",rd_type())) 
        remove_report_elem(paste0("plot.var.pred.rd.",rd_type()))
      }, {
        prediccion.rd <<- NULL
        remove_report_elem(paste0("pred.rd.",rd_type()))
        remove_report_elem(paste0("disp.rd.",rd_type())) 
        output$rdPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.rd <<- rep(0, 10)
        remove_report_elem(paste0("ind.rd",rd_type()))
      })
    }
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_rd <- function(){
    tryCatch({ # Se corren los codigo
      codigo <- input$fieldCodeRdDisp
      output$plot.rd.disp <- renderPlot(isolate(exe(codigo)))
      insert_report(paste0("disp.rd.",rd_type()),
                    paste0("Dispersi\u00F3n del Modelo Reducci\u00F3n de Dimensiones (",rd_type(),")"),
                    codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rd(2)
      showNotification(paste0("Error (R/L-02) : ", e), duration = 15, type = "error")
    })
  }
  
  plot_rmse_rd <- function(){
    tryCatch({ # Se corren los codigo
      isolate(tipo <- rd_type())
      ncomp <- n.comp.rd
      if (input$permitir.ncomp) {
        if(!is.na(input$ncomp.rd) && input$ncomp.rd >= 0) {
          ncomp <- input$ncomp.rd
        }
      }
      output$plot.rd.rmse <- renderPlot(exe("plot_RMSE(modelo.rd.",tipo,",",ncomp,")"))
      insert_report(paste0("rmse.rd.",tipo),
                    "Error RMSE seg\u00fan N\u00famero de Componentes",
                    paste0("plot_RMSE(modelo.rd.",tipo,",",ncomp,")"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rd(1)
      showNotification(paste0("Error (RD-01) : ", e), duration = 15, type = "error")
    })
  }
  
  rd_plot_pred <- function(){
    tryCatch({ # Se corren los codigo
      isolate(tipo <- rd_type())
      ncomp <- n.comp.rd
      if (input$permitir.ncomp) {
        if(!is.na(input$ncomp.rd) && input$ncomp.rd >= 0) {
          ncomp <- input$ncomp.rd
        }
      }
      output$plot.rd.pred <- renderPlot(exe("plot_pred_rd(modelo.rd.",tipo,",",ncomp,")"))
      insert_report(paste0("plot.pred.rd.",tipo), "Gr\u00e1fico de varianza explicada en los predictores",
                    paste0("plot_pred_rd(modelo.rd.",tipo,",",ncomp,")"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rd(1)
      showNotification(paste0("Error (RD-01) : ", e), duration = 15, type = "error")
    })
  }
  
  rd_plot_var_pred <- function(){
    tryCatch({ # Se corren los codigo
      isolate(tipo <- rd_type())
      ncomp <- n.comp.rd
      if (input$permitir.ncomp) {
        if(!is.na(input$ncomp.rd) && input$ncomp.rd >= 0) {
          ncomp <- input$ncomp.rd
        }
      }
      output$plot.rd.var.pred <- renderPlot(exe("plot_var_pred_rd(modelo.rd.",tipo,",",ncomp,")"))
      insert_report(paste0("plot.var.pred.rd.",tipo), "Gr\u00e1fico de varianza explicada en la variable a predecir",
                    paste0("plot_var_pred_rd(modelo.rd.",tipo,",",ncomp,")"))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rd(1)
      showNotification(paste0("Error (RD-01) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  rd_full <- function(){
    execute_rd()
    execute_rd_pred()
    execute_rd_ind()
  }
  
  # Generates the model
  execute_rd <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rd.modelo))
      isolate(tipo <- rd_type())
      output$txtRd <- renderPrint(print(exe("summary(modelo.rd.",tipo,")")))
      
      insert_report(paste0("modelo.rd.",tipo), paste0("Generaci\u00f3n del Modelo Reducci\u00f3n de Dimensiones(",tipo,")"),
                    cod.rd.modelo, "\nsummary(modelo.rd.",tipo,")")
      
      plot_rmse_rd()
      rd_plot_pred()
      rd_plot_var_pred()
      
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.rd.",tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rd(1)
      showNotification(paste0("Error (RD-01) : ",e), duration = 15, type = "error")
    })
  }
  
  # Generate the prediction
  execute_rd_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(tipo <- rd_type())
      
      if (input$permitir.ncomp && input$ncomp.rd >= 0) {
        if (input$ncomp.rd > exe("modelo.rd.",tipo,"$ncomp")) {
          stop(tr("errRDnCom") , call. = FALSE)
        }
      }
      
      cod.rd.pred <- gsub("n.comp.rd", as.character(n.comp.rd), cod.rd.pred)
      updateAceEditor(session, "fieldCodeRdPred", value = cod.rd.pred)
      
      
      isolate(exe(cod.rd.pred))
      output$rdPrediTable <- DT::renderDataTable(tb_predic(real.val,exe("prediccion.rd.",tipo)), server = FALSE)
      
      insert_report(paste0("pred.rd.",tipo),
                    paste0("Predicci\u00f3n del Modelo Reducci\u00f3n de Dimensiones(",tipo,")"), 
                    cod.rd.pred, "\nkt(head(tb_predic(real.val, prediccion.rd.",tipo,")$x$data[,-1]))")
      
      plot_disp_rd()
      nombres.modelos <<- c(nombres.modelos, "prediccion.rd")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rd(2)
      showNotification(paste0("Error (RD-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Generates the indices
  execute_rd_ind <- function() {
    if(exists(paste0("prediccion.rd.",rd_type()))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rd.ind))
        indices.rd <- general_indices(datos.prueba[,variable.predecir], exe("prediccion.rd.",rd_type()))
        
        insert_report(paste0("ind.rd.",rd_type()),"\u00cdndices Generales del Modelo Reducci\u00f3n de Dimensiones",
                      cod.rd.ind, 
                      "\nkt(general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.rd.",rd_type(),"))\n",
                      "indices.rd <- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.rd.",rd_type(),")\n",
                      "IndicesM[['rd-",rd_type(),"']] <- indices.rd")
        
        df <- as.data.frame(indices.rd)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfrd <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfrd2 <- render_index_table(df2)
        
        IndicesM[[paste0("rd-",rd_type())]] <<- indices.rd
        update_comparative_selector()
      },
      error = function(e){ # Regresamos al estado inicial y mostramos un error
        clean_rd(3)
        showNotification(paste0("Error (RD-03) : ",e), duration = 15, type = "error")
      })
    }
  }
  
  # RF PAGE ---------------------------------------------------------------------------------------------------------------

  # When the rf model is generated
  observeEvent(input$runRf, {
    if (validate_data()) { # Si se tiene los datos entonces :
      rf_full()
    }
  })

  # When the user changes the parameters
  observeEvent(c(input$ntree.rf,input$mtry.rf), {
    if (validate_data(print = FALSE) & rf.stop.excu) {
      deafult_codigo_rf()
    }else{
      rf.stop.excu <<- TRUE
    }
  })

  # When user change the rule selector
  observeEvent(input$rules.rf.n,{
    if(validate_data(print = FALSE)){
        show_rf_rules(input$rules.rf.n)
    }
  })

  # Upgrade code fields to default version
  deafult_codigo_rf <- function(rf.def = FALSE){
    if(!is.null(datos.aprendizaje) & rf.def){
      mtry.value <- ifelse(rf.def, round(sqrt(ncol(datos.aprendizaje))), input$mtry.rf)
      updateNumericInput(session,"mtry.rf",value = mtry.value)
    }else{
      mtry.value <- input$mtry.rf
    }

    # Se acualiza el codigo del modelo
    codigo <- rf_model(variable.pred = variable.predecir,
                        ntree = input$ntree.rf,
                        mtry = mtry.value)

    updateAceEditor(session, "fieldCodeRf", value = codigo)
    cod.rf.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- rf_prediction(variable.pred = variable.predecir)
    updateAceEditor(session, "fieldCodeRfPred", value = codigo)
    cod.rf.pred <<- codigo
    
    # Se genera el codigo de la dispersion
    codigo <- disp_models("prediccion.rf", translate("rfl"), variable.predecir)
    updateAceEditor(session, "fieldCodeRfDisp", value = codigo)

    # Cambia el codigo del grafico de rf
    updateAceEditor(session, "fieldCodeRfPlot", value = extract_code("importance_plot_rf"))

    # Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeRfIG", value = codigo)
    cod.rf.ind <<- codigo
  }

  # Cleans the data according to the process where the error is generated
  clean_rf <- function(capa = NULL){
    for(i in capa:3){
      switch(i, {
        modelo.rf <<- NULL
        output$txtRf <- renderPrint(invisible(""))
        remove_report_elem("modelo.rf")
        remove_report_elem("modelo.rf.graf")
        remove_report_elem("disp.rf")
      }, {
        prediccion.rf <<- NULL
        remove_report_elem("pred.rf")
        output$rfPrediTable <- DT::renderDataTable(NULL)
      },{
        indices.rf <<- rep(0, 10)
        remove_report_elem("ind.rf")
      })
    }
  }

  # Shows the chart of importance
  plotear_rf_imp <- function() {
    tryCatch({
      output$plot.rf <- renderPlot(isolate(importance_plot_rf(modelo.rf,translate("impVarA"),translate("impVarRSS"))))
      cod <- ifelse(input$fieldCodeRfPlot == "", extract_code("importance_plot_rf"), input$fieldCodeRfPlot)
      insert_report("modelo.rf.graf", "Importancia de las Variables", cod,
                    "\nimportance_plot_rf(modelo.rf,'",translate('impVarA'),"','",translate('impVarRSS'),"')")
      
    }, error = function(e) {
      output$plot.rf <- renderPlot(NULL)
      remove_report_elem("modelo.rf.graf")
    })
  }

  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_rf <- function(){
    tryCatch({ # Se corren los codigo
      output$plot.rf.disp <- renderPlot(exe(input$fieldCodeRfDisp))
      insert_report("disp.rf", "Dispersi\u00F3n del Modelo RF", input$fieldCodeRfDisp)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rf(2)
      showNotification(paste0("Error (RF-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Show Rules
  show_rf_rules <- function(n){
    output$rulesRf <- renderPrint({
      tryCatch({
          updateAceEditor(session,"fieldCodeRfRules",paste0("printRandomForests(modelo.rf, ",n,", format='VB')"))
          printRandomForests(modelo.rf, n, format='VB')
        },error = function(e){
          stop(translate("NoDRule"))
      })
    })
    insert_report(paste0("modelo.rf.rules.", n),paste0("Reglas del \u00C1rbol #",n),
                  "printRandomForests(modelo.rf, ",n,")")
  }

  # Execute model, prediction and indices
  rf_full <- function(){
    execute_rf()
    execute_rf_pred()
    execute_rf_ind()
  }

  # Generates the model
  execute_rf <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rf.modelo))
      output$txtRf <- renderPrint(print(modelo.rf))

      insert_report("modelo.rf","Generaci\u00F3n del Modelo Bosques Aleatorios",
                    cod.rf.modelo,"\nmodelo.rf")

      plotear_rf_imp()
      plot_disp_rf()
      show_rf_rules(input$rules.rf.n)
      nombres.modelos <<- c(nombres.modelos, "modelo.rf")
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rf(1)
      showNotification(paste0("Error (RF-01) : ",e), duration = 15, type = "error")
    })
  }

  # Generate the prediction
  execute_rf_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.rf.pred))

      output$rfPrediTable <- DT::renderDataTable(tb_predic(real.val, prediccion.rf), server = FALSE)

      insert_report("pred.rf","Predicci\u00F3n del Modelo Bosques Aleatorios",
                    cod.rf.pred,"\nkt(head(tb_predic(real.val, prediccion.rf)$x$data[,-1]))",interpretation = FALSE)

      nombres.modelos <<- c(nombres.modelos, "prediccion.rf")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_rf(2)
      showNotification(paste0("Error (RF-02) : ", e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_rf_ind <- function() {
    if(exists("prediccion.rf") && !is.null(prediccion.rf)){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.rf.ind))

        indices.rf <- general_indices(datos.prueba[,variable.predecir], prediccion.rf)

        insert_report("ind.rf","\u00CDndices Generales del Modelo Bosques Aleatorios",
                      cod.rf.ind, "\nkt(general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.rf))\n",
                      "indices.rf <- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.rf)\n",
                      "IndicesM[['rfl']] <- indices.rf")

        df <- as.data.frame(indices.rf)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfrf <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfrf2 <- render_index_table(df2)

        nombres.modelos <<- c(nombres.modelos, "indices.rf")
        IndicesM[["rfl"]] <<- indices.rf
        update_comparative_selector()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_rf(3)
        showNotification(paste0("Error (RF-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # BOOSTING PAGE ---------------------------------------------------------------------------------------------------------

  # When the boosting model is generated
  observeEvent(input$runBoosting, {
    if (validate_data()){ # Si se tiene los datos entonces :
      boosting_full()
    }
  })

  # When user change the rule selector
  observeEvent(input$rules.b.n,{
    if(validate_data(print = FALSE)){
      mostrar.reglas.boosting(input$rules.b.n)
    }
  })

  # When the user changes the parameters
  observeEvent(c(input$iter.boosting, input$nu.boosting, input$tipo.boosting, input$shrinkage.boosting, input$maxdepth.boosting), {
    if (validate_data(print = FALSE)){
      deault_codigo_boosting()
    }
  })

  # Upgrade code fields to default version
  deault_codigo_boosting <- function() {
    # Se acualiza el codigo del modelo
    codigo <- boosting_model(variable.pred = variable.predecir,
                             model.var = paste0("modelo.boosting.",input$tipo.boosting),
                             n.trees = input$iter.boosting,
                             distribution = input$tipo.boosting,
                             shrinkage = input$shrinkage.boosting)

    updateAceEditor(session, "fieldCodeBoosting", value = codigo)
    cod.b.modelo <<- codigo

    # Se genera el codigo de la prediccion
    codigo <- boosting_prediction(variable.pred = variable.predecir, 
                                  model.var = paste0("modelo.boosting.",input$tipo.boosting),
                                  pred.var = paste0("prediccion.boosting.",input$tipo.boosting),
                                  n.trees = input$iter.boosting)
    
    updateAceEditor(session, "fieldCodeBoostingPred", value = codigo)
    cod.b.pred <<- codigo

    # Se genera el codigo de la dispersion
    codigo <- disp_models(paste0("prediccion.boosting.",input$tipo.boosting), translate("bl"), variable.predecir)
    updateAceEditor(session, "fieldCodeBoostingDisp", value = codigo)
    
    # Cambia el codigo del grafico de importancia
    updateAceEditor(session, "fieldCodeBoostingPlotImport", value = boosting_importance_plot(paste0("modelo.boosting.",input$tipo.boosting)))

    # Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeBoostingIG", value = codigo)
    cod.b.ind <<- codigo
  }

  # Cleans the data according to the process where the error is generated
  clean_boosting <- function(capa = NULL) {
    for (i in capa:3) {
      switch(i, {
        exe("modelo.boosting.",input$tipo.boosting," <<- NULL")
        output$txtBoosting <- renderPrint(invisible(""))
        output$plot.boosting.import <- renderPlot(NULL)
        remove_report_elem(paste0("modelo.b.",input$tipo.boosting))
        remove_report_elem(paste0("modelo.b.error.",input$tipo.boosting))
        remove_report_elem(paste0("modelo.b.imp.",input$tipo.boosting))
      }, {
        exe("prediccion.boosting.",input$tipo.boosting," <<- NULL")
        remove_report_elem(paste0("pred.b.",input$tipo.boosting))
        output$boostingPrediTable <- DT::renderDataTable(NULL)
      },{
        exe("indices.boosting.",input$tipo.boosting," <<- NULL")
        remove_report_elem(paste0("ind.b.",input$tipo.boosting))
      })
    }
  }

  # Shows the chart of importance
  plotear_boosting_imp <- function() {
    tryCatch({
      codigo <- input$fieldCodeBoostingPlotImport
      tipo <- input$tipo.boosting
      output$plot.boosting.import <- renderPlot(isolate(exe(codigo)))
      cod <- ifelse(codigo == "",boosting_importance_plot(paste0("modelo.boosting.",input$tipo.boosting)), codigo)
      insert_report(paste0("modelo.b.imp.",tipo), 
                    paste0("Importancia de las Variables (",tipo,")"),
                    cod)
    }, error = function(e) {
      clean_boosting(1)
    })
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_boosting <- function(){
    tryCatch({ # Se corren los codigo
      tipo <- input$tipo.boosting
      codigo <- input$fieldCodeBoostingDisp
      output$plot.boosting.disp <- renderPlot(exe(codigo))
      insert_report(paste0("disp.boosting.",tipo),paste0("Dispersi\u00F3n del Modelo BOOSTING (",tipo,")"), codigo)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_boosting(2)
      showNotification(paste0("Error (B-02) : ", e), duration = 15, type = "error")
    })
  }
  
  # Execute model, prediction and indices
  boosting_full <- function() {
    if(!is.null(calibrate_boosting(datos.aprendizaje))){
      execute_boosting()
      execute_boosting_pred()
      execute_boosting_ind()
    }else{
      showNotification(translate("ErrorBsize"), duration = 15, type = "error")
    }
  }

  # Generates the model
  execute_boosting <- function() {
    tryCatch({ # Se corren los codigo
        isolate(exe(cod.b.modelo))
        isolate(tipo <- input$tipo.boosting)
        output$txtBoosting <- renderPrint(exe("print(summary(modelo.boosting.",tipo,",plotit = FALSE))"))
        
        plotear_boosting_imp()
  
        insert_report(paste0("modelo.b.",tipo), paste0("Generaci\u00F3n del Modelo BOOSTING (",tipo,")"),
                      cod.b.modelo, "\nmodelo.boosting.",tipo)
  
        nombres.modelos <<- c(nombres.modelos, paste0("modelo.boosting.",tipo))
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_boosting(1)
      showNotification(paste0("Error (B-01) : ",e), duration = 15, type = "error")
    })
  }

  # Generate the prediction
  execute_boosting_pred <- function(){
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.b.pred))
      isolate(tipo <- input$tipo.boosting)
      
      # Cambia la tabla con la prediccion de boosting
      output$boostingPrediTable <- DT::renderDataTable(tb_predic(real.val, exe("prediccion.boosting.",tipo)),server = FALSE)
      insert_report(paste0("pred.b.",tipo),paste0("Predicci\u00F3n del Modelo BOOSTING (",tipo,")"),
                    cod.b.pred,"\nkt(head(tb_predic(real.val, prediccion.boosting.",input$tipo.boosting,")$x$data[,-1]))",interpretation = FALSE)

      plot_disp_boosting()
      nombres.modelos <<- c(nombres.modelos, paste0("modelo.boosting.",tipo))
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comprativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_boosting(2)
      showNotification(paste0("Error (B-02) : ",e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_boosting_ind <- function() {
    if(exists(paste0("prediccion.boosting.",input$tipo.boosting))){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.b.ind))
        isolate(tipo <- input$tipo.boosting)
        
        indices.boosting <- general_indices(datos.prueba[,variable.predecir], exe("prediccion.boosting.",tipo))
        #eval(parse(text = paste0("indices.boosting.",tipo, "<<- indices.boosting")))

        insert_report(paste0("ind.b.",tipo), paste0("\u00CDndices Generales del Modelo (",tipo,")"),
                             cod.knn.ind, "\nkt(general_indices(datos.prueba[,'",variable.predecir,"'] ,prediccion.boosting.",tipo,"))\n",
                             "indices.boosting <- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.boosting.",tipo,")\n",
                             "IndicesM[['bl-",tipo,"']] <- indices.boosting")
        
        df <- as.data.frame(indices.boosting)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfb <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfb2 <- render_index_table(df2)

        #nombres.modelos <<- c(nombres.modelos, paste0("indices.boosting.",tipo))
        IndicesM[[paste0("bl-",tipo)]] <<- indices.boosting
        update_comparative_selector()
      },
      error = function(e) { # Regresamos al estado inicial y mostramos un error
        clean_boosting(3)
        showNotification(paste0("Error (B-03) : ", e), duration = 15, type = "error")
      })
    }
  }
  
  # NN PAGE ---------------------------------------------------------------------------------------------------------------

  # When user change the layer selector
  observeEvent(c(input$cant.capas.nn, input$segmentButton), {
    if(!is.null(datos.aprendizaje) && !is.null(input$cant.capas.nn)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn) {
          shinyjs::show(paste0("nn.cap.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.", i))
        }
      }
    }
  })

  # When the nn model is generated
  observeEvent(input$runNn, {
    if (validate_data()) { # Si se tiene los datos entonces :
      nn_full()
    }
  })

  # When the user changes the parameters
  observeEvent(c(input$cant.capas.nn,input$threshold.nn,input$stepmax.nn,
                 input$nn.cap.1,input$nn.cap.2,input$nn.cap.3,input$nn.cap.4,input$nn.cap.5,
                 input$nn.cap.6,input$nn.cap.7,input$nn.cap.8,input$nn.cap.9,input$nn.cap.10),{
    if(validate_data(print = FALSE)){
      default_codigo_nn()
    }
  })

  # Upgrade code fields to default version
  default_codigo_nn <- function(){
    #Se acualiza el codigo del modelo
    
    codigo <- nn_model(data = "datos.aprendizaje",
                       variable.pred = variable.predecir,
                       model.var = "modelo.nn",
                       mean.var = "mean.nn",
                       sd.var = "sd.nn",
                       threshold = input$threshold.nn,
                       stepmax = input$stepmax.nn,
                       cant.hidden = input$cant.capas.nn,
                       input$nn.cap.1,input$nn.cap.2,
                       input$nn.cap.3,input$nn.cap.4,
                       input$nn.cap.5,input$nn.cap.6,
                       input$nn.cap.7,input$nn.cap.8,
                       input$nn.cap.9,input$nn.cap.10)

    updateAceEditor(session, "fieldCodeNn", value = codigo)
    cod.nn.modelo <<- codigo

    #Cambia el codigo del grafico del árbol
    updateAceEditor(session, "fieldCodeNnPlot", value = nn_plot())

    #Se genera el codigo de la prediccion
    codigo <- nn_prediction(variable.pred = variable.predecir)
    updateAceEditor(session, "fieldCodeNnPred", value = codigo)
    cod.nn.pred <<- codigo

    # Se genera el codigo de la dispersion
    codigo <- disp_models("prediccion.nn", translate("nn"), variable.predecir)
    updateAceEditor(session, "fieldCodeNnDisp", value = codigo)

    #Se genera el codigo de la indices
    codigo <- extract_code("general_indices")
    updateAceEditor(session, "fieldCodeNnIG", value = codigo)
    cod.nn.ind <<- codigo
  }

  # Shows the graph of the network
  plot_net <- function(){
    tryCatch({
      capas <- c(input$nn.cap.1,input$nn.cap.2,input$nn.cap.3,input$nn.cap.4,
                 input$nn.cap.5,input$nn.cap.6,input$nn.cap.7,input$nn.cap.8,input$nn.cap.9,input$nn.cap.10)
      capas <- capas[1:input$cant.capas.nn]
      if(input$cant.capas.nn * sum(capas) <= 1000 & ncol(modelo.nn$covariate) <= 25){
        output$plot.nn <- renderPlot(isolate(exe(input$fieldCodeNnPlot)))
        cod <- ifelse(input$fieldCodeNnPlot == "", nn_plot(), input$fieldCodeNnPlot)
        insert_report("modelo.nn.graf", "Red Neuronal", cod)
      }else{
        showNotification(translate("bigPlot"), duration = 10, type = "message")
      }
    },
    error = function(e){
      output$plot.nn <- renderPlot(NULL)
      remove_report_elem("modelo.nn.graf")
    })
  }
  
  # Shows the graph the dispersion of the model with respect to the real values
  plot_disp_nn <- function(){
    tryCatch({ # Se corren los codigo
      output$plot.nn.disp <- renderPlot(exe(input$fieldCodeNnDisp))
      insert_report("disp.nn", "Dispersi\u00F3n del Modelo Redes Neuronales", input$fieldCodeNnDisp)
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_nn(2)
      showNotification(paste0("Error (NN-02) : ", e), duration = 15, type = "error")
    })
  }

  # Cleans the data according to the process where the error is generated
  clean_nn <- function(capa = NULL) {
    for (i in capa:3) {
      switch(i, {
        exe("modelo.nn <- NULL")
        output$txtnn <- renderPrint(invisible(""))
        output$plot.nn <- renderPlot(NULL)
        remove_report_elem("modelo.nn")
        remove_report_elem("modelo.nn.graf")
      }, {
        exe("prediccion.nn <- NULL")
        remove_report_elem("pred.nn")
        output$nnPrediTable <- DT::renderDataTable(NULL)
      },{
        exe("indices.nn <- NULL")
        remove_report_elem("ind.nn")
      })
    }
  }

  # Execute model, prediction and indices
  nn_full <- function() {
    execute_nn()
    if(NN_EXECUTION){
      execute_nn_pred()
      execute_nn_ind()
    }
  }

  # Generates the model
  execute_nn <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.nn.modelo))
      output$txtnn <- renderPrint(print(modelo.nn))
      insert_report("modelo.nn", "Generaci\u00F3n del modelo Redes Neuronales",
                    cod.nn.modelo,"\nsummary(modelo.nn)")
      plot_net()
      nombres.modelos <<- c(nombres.modelos,"modelo.nn")
      NN_EXECUTION <<- TRUE
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_nn(1)
      showNotification(paste0("Error (NN-01) : ",e), duration = 15, type = "error")
    },
    warning = function(w){
      clean_nn(1)
      NN_EXECUTION <<- FALSE
      showNotification(paste0(translate("nnWar")," (NN-01) : ",w), duration = 20, type = "warning")
    })
  }

  # Generate the prediction
  execute_nn_pred <- function() {
    tryCatch({ # Se corren los codigo
      isolate(exe(cod.nn.pred))
      
      # Cambia la tabla con la prediccion de nn
      output$nnPrediTable <- DT::renderDataTable(tb_predic(real.val, prediccion.nn),server = FALSE)

      insert_report("pred.nn", "Predicci\u00F3n del Modelo Redes Neuronales", cod.nn.pred,
                    "\nkt(head(tb_predic(real.val, prediccion.nn)$x$data[,-1]))",interpretation = FALSE)

      plot_disp_nn()
      nombres.modelos <<- c(nombres.modelos,"prediccion.nn")
      updatePlot$tablaCom <- !updatePlot$tablaCom #graficar otra vez la tabla comparativa
    },
    error = function(e) { # Regresamos al estado inicial y mostramos un error
      clean_nn(2)
      showNotification(paste0("Error (NN-02) : ",e), duration = 15, type = "error")
    })
  }

  # Generates the indices
  execute_nn_ind <- function() {
    if(exists("prediccion.nn")){
      tryCatch({ # Se corren los codigo
        isolate(exe(cod.nn.ind))
        indices.nn <- general_indices(datos.prueba[,variable.predecir], prediccion.nn)

        insert_report("ind.nn","\u00CDndices Generales del Modelo Redes Neuronales",
                      cod.nn.ind, 
                      "\nkt(general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.nn))\n",
                      "indices.nn <- general_indices(datos.prueba[,'",variable.predecir,"'], prediccion.nn)\n",
                      "IndicesM[['nn']] <- indices.nn")

        df <- as.data.frame(indices.nn)
        colnames(df) <- c(translate("RMSE"), translate("MAE"), translate("ER"), translate("correlacion"))
        output$indexdfnn <- render_index_table(df)
        
        df2 <- as.data.frame(summary_indices(datos.aprendizaje[,variable.predecir]))
        colnames(df2) <- c(translate("minimo"),translate("q1"),translate("q3"),translate("maximo"))
        output$indexdfnn2 <- render_index_table(df2)
        
        IndicesM[["nn"]] <<- indices.nn
        update_comparative_selector()
      },
      error = function(e) { #Regresamos al estado inicial y mostramos un error
        clean_nn(3)
        showNotification(paste0("Error (NN-03) : ",e), duration = 15, type = "error")
      })
    }
  }

  # COMPARISON CHART ------------------------------------------------------------------------------------------------------
  
  # Updates the selectors in the comparison table page
  update_comparative_selector <- function(){
    nombres <- models_mode(IndicesM)
    shinyWidgets::updateCheckboxGroupButtons(session,"select.models",choices = sort(nombres),selected = sort(nombres),
                                             status = "primary",checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                                                 no = icon("remove", lib = "glyphicon")))
  }

  #Muestra la tabla comparativa.
  output$TablaComp <- DT::renderDataTable({
    graficar <- updatePlot$tablaCom
    if (!is.null(datos.aprendizaje)) {
      
      insert_report("tabla.comparativa","Tabla Comparativa","kt(comparative_table(",as_string_c(input$select.models),",IndicesM) )")
      
      DT::datatable(comparative_table(input$select.models),
                    selection = "none", editable = FALSE,
                    options = list(dom = "frtip", pageLength = 9, buttons = NULL))
    }
  },server = FALSE)

  # NEW PREDICTIONS PAGE --------------------------------------------------------------------------------------------------

  # Update the different tables in the "shiny" application
  update_table_pn <- function(tablas = c("contentsPred", "contentsPred2")){
    if("contentsPred2" %in% tablas){
      output$contentsPred <- render_table_data(datos.aprendizaje.completos,editable = F,
                                                    scrollY = "25vh", server = F)
    }
    if("contentsPred2" %in% tablas){
      output$contentsPred2 <- render_table_data(datos.aprendizaje.completos,editable = F,
                                                     scrollY = "25vh", server = F)
    }
    if("contentsPred3" %in% tablas){
      output$contentsPred3 <- render_table_data(datos.prueba.completos,editable = F,
                                                     scrollY = "25vh", server = T)
    }
  }

  # Update the model text for prediction of new individuals
  update_model_text_pn <- function(codigo){
    updateAceEditor(session, "fieldPredNuevos", value = codigo)
    if(is.null(modelo.nuevos)){
      output$txtPredNuevos <- renderPrint(invisible(NULL))
    }else{
      output$txtPredNuevos <- renderPrint(print(modelo.nuevos))
    }
  }

  # Update the prediction table of new individuals
  update_pred_pn <- function(codigo){
    updateAceEditor(session, "fieldCodePredPN", value = codigo)
    if(!is.null(predic.nuevos)){
      datos.aux.prueba <- new_col(datos.prueba.completos, variable.predecir.pn, predic.nuevos)
      output$PrediTablePN <- render_table_data(datos.aux.prueba,editable = F,
                                                scrollY = "25vh",server = T)
    }else{
      output$PrediTablePN <- DT::renderDT(DT::datatable(data.frame()))
    }
  }

  # Updates neural network layers of new individuals
  update_nn_layers_pn <- function(){
    if(!is.null(datos.aprendizaje.completos) && !is.null(input$cant.capas.nn.pred)){
      for (i in 1:10) {
        if(i <= input$cant.capas.nn.pred) {
          shinyjs::show(paste0("nn.cap.pred.", i))
        } else {
          shinyjs::hide(paste0("nn.cap.pred.", i))
        }
      }
    }
  }

  update_nn_layers_pn()

  # Download the data with the prediction
  output$downloaDatosPred <- downloadHandler(
    filename = function() {
      input$file3$name
    },
    content = function(file) {
      if(!is.null(predic.nuevos)){
        write.csv(new_col(datos.prueba.completos, variable.predecir.pn, predic.nuevos), file, row.names = input$rownameNPred2)
      }
    }
  )

  # When the number of neural network layers changes.
  observeEvent(c(input$cant.capas.nn.pred), {
    update_nn_layers_pn()
  })

  # When the number of components changes.
  observeEvent(input$permitir.ncomp.pred, {
    if (input$permitir.ncomp.pred) {
      shinyjs::enable("ncomp.rd.pred")
    } else {
      shinyjs::disable("ncomp.rd.pred")
    }
  })
  
  # When allowing the lambda changes
  observeEvent(input$permitir.landa.pred, {
    if (input$permitir.landa.pred) {
      shinyjs::enable("landa.pred")
    } else {
      shinyjs::disable("landa.pred")
    }
  })
  

  # When learning data is loaded
  observeEvent(input$loadButtonNPred,{
    codigo.carga <- code_load(row.names = input$rownameNPred,
                              path = input$file2$datapath,
                              sep = input$sepNPred,
                              sep.dec = input$decNPred,
                              header = input$headerNPred,
                              d.o = "datos.originales.completos",
                              d = "datos.aprendizaje.completos")

    tryCatch({
      isolate(exe(codigo.carga))
      if(ncol(datos.originales.completos) <= 1) {
        showNotification(translate("errorSeg"), duration = 10, type = "error")
        return(NULL)
      }
      codigo.na <- ""
      codigo.na <- paste0(code_NA(deleteNA = input$deleteNAnPred, d.o = "datos.originales.completos"), 
                          "\n", "datos.aprendizaje.completos <- datos.originales.completos")
      isolate(exe(codigo.na))

      updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames_empty(var_numerical(datos.aprendizaje.completos))))
      updateNumericInput(session, "kmax.knn.pred", value = round(sqrt(nrow(datos.aprendizaje.completos))))
      updateNumericInput(session, "mtry.rf.pred", value = round(sqrt(ncol(datos.aprendizaje.completos) -1)))
    },
    error = function(e) {
      showNotification(paste0("Error:", e), duration = 10, type = "error")
      datos.aprendizaje.completos <<- NULL
      datos.originales.completos <<- NULL
      return(NULL)
    })

    modelo.nuevos <<- NULL
    predic.nuevos <<- NULL
    update_pred_pn("")
    update_model_text_pn("")
    update_table_pn()
  })

  # Show the select box of the panel to transform data (eventReactive)
  update_trans_pn <- eventReactive(c(input$loadButtonNPred), {
    contadorPN <<- contadorPN + 1
    if (!is.null(datos.aprendizaje.completos) && ncol(datos.aprendizaje.completos) > 0) {
      res <- data.frame(Variables = colnames(datos.aprendizaje.completos),
                        Tipo = c(1:ncol(datos.aprendizaje.completos)),
                        Activa = c(1:ncol(datos.aprendizaje.completos)))
      res$Tipo <- sapply(colnames(datos.aprendizaje.completos), function(i) paste0(
        '<select id="Predsel', i, contadorPN, '"> <option value="categorico">',translate("categorico"),'</option>',
        '<option value="numerico" ', ifelse(class(datos.aprendizaje.completos[, i]) %in% c("numeric", "integer"),' selected="selected"', ""),'>', translate("numerico"),'</option>',
        '<option value="disyuntivo">',translate("disyuntivo"),'</option> </select>'
      ))
      res$Activa <- sapply(colnames(datos.aprendizaje.completos), function(i) paste0('<input type="checkbox" id="Predbox', i, contadorPN, '" checked/>'))
      update_nn_layers_pn()
    } else {
      res <- as.data.frame(NULL)
      showNotification(translate("tieneCData"), duration = 10, type = "error")
    }
    return(res)
  })

  # Show the select box of the panel to transform data
  output$transDataPredN <- DT::renderDataTable(update_trans_pn(),
                                          escape = FALSE, selection = "none", server = FALSE,
                                          options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollY = "35vh"), rownames = F,
                                          callback = JS("table.rows().every(function(i, tab, row) {
                                                        var $this = $(this.node());
                                                        $this.attr('id', this.data()[0]);
                                                        $this.addClass('shiny-input-checkbox');});
                                                        Shiny.unbindAll(table.table().node());
                                                        Shiny.bindAll(table.table().node());"))

  # Use and show the codes to transform the data
  transform_data_pn <- function() {
    var.noactivas <- c()
    code.res <- "datos.aprendizaje.completos <- datos.originales.completos \n"
    for (var in colnames(datos.originales.completos)) {
      if (input[[paste0("Predbox", var, contadorPN)]]) {
        if (input[[paste0("Predsel", var, contadorPN)]] == "categorico" & class(datos.originales.completos[, var]) %in% c("numeric", "integer")) {
          code.res <- paste0(code.res, code_transf(var, "categorico", d.o = "datos.originales.completos", d = "datos.aprendizaje.completos" ), "\n")
        }
        if (input[[paste0("Predsel", var, contadorPN)]] == "numerico" & !(class(datos.originales.completos[, var]) %in% c("numeric", "integer"))) {
          code.res <- paste0(code.res, code_transf(var, "numerico",  d.o = "datos.originales.completos", d = "datos.aprendizaje.completos" ), "\n")
        }
        if (input[[paste0("Predsel", var, contadorPN)]] == "disyuntivo") {
          code.res <- paste0(code.res, code_transf(var, "disyuntivo", d.o = "datos.originales.completos", d = "datos.aprendizaje.completos" ), "\n")
        }
      } else {
        var.noactivas <- c(var.noactivas, var)
      }
    }

    isolate(exe(code.res))
    if (length(var.noactivas) > 0) {
      des <- code_deactivate(var.noactivas,"datos.aprendizaje.completos")
      isolate(exe(des))
      code.res <- paste0(code.res, "\n", des)
    }
    code.res <- paste0(code.res, "\n")
    return(code.res)
  }
  
  # When the user enters the prediction panel
  observeEvent(input$predecirPromidat, {
    if(!is.null(datos.prueba.completos)){
      if(exists("modelo.nuevos") && !is.null(modelo.nuevos)){
        codigo <- switch(modelo.seleccionado.pn,
                         rl  =  rl_prediction(data = 'datos.prueba.completos', 
                                              model.var = 'modelo.nuevos', 
                                              pred.var = 'predic.nuevos'),
                         rlr =  rlr_prediction(data.a = "datos.aprendizaje.completos",
                                               data.p = 'datos.prueba.completos',
                                               variable.pred = variable.predecir.pn,
                                               model.var = 'modelo.nuevos',
                                               pred.var = 'predic.nuevos',
                                               lambda = if(input$permitir.landa.pred){ifelse(is.na(input$landa.pred),NULL,input$landa.pred)}else{NULL},
                                               cv.var = "cv.glm.nuevos"),
                         knn =  kkn_prediction(data = 'datos.prueba.completos',
                                               variable.pred = variable.predecir.pn,
                                               model.var = 'modelo.nuevos', 
                                               pred.var  = 'predic.nuevos'),
                         dt  = dt_prediction(data = "datos.prueba.completos",
                                             model.var = "modelo.nuevos",
                                             pred.var = "predic.nuevos"),
                         rf  = rf_prediction(data = "datos.prueba.completos",
                                             variable.pred = variable.predecir.pn,
                                             model.var = "modelo.nuevos",
                                             pred.var = "predic.nuevos"),
                         boosting = boosting_prediction(data = "datos.prueba.completos",
                                                        variable.pred = variable.predecir.pn,
                                                        model.var = "modelo.nuevos",
                                                        pred.var = "predic.nuevos",
                                                        n.trees = input$iter.boosting.pred),
                         svm = svm_prediction(data = "datos.prueba.completos", 
                                              variable.pred = variable.predecir.pn,
                                              model.var = "modelo.nuevos", 
                                              pred.var = "predic.nuevos"),
                         rd  =  rd_prediction(data = "datos.prueba.completos",
                                              model.var = "modelo.nuevos",
                                              pred.var = "predic.nuevos",
                                              n.comp = "n.comp.rd.np",
                                              ncomp = if(input$permitir.ncomp.pred){input$ncomp.rd.pred}else{NULL}),
                         nn = nn_prediction(data = "datos.prueba.completos",
                                            variable.pred = variable.predecir.pn,
                                            model.var = "modelo.nuevos",
                                            pred.var = "predic.nuevos",
                                            mean.var = "mean.nn.np",
                                            sd.var = "sd.nn.np"))
        tryCatch({
          exe(codigo)
          update_pred_pn(codigo)
        },error =  function(e){
          showNotification(paste0("Error :", e), duration = 10, type = "error")
        })
      }else{
        showNotification(paste0("Error :", translate("ErrorModelo")), duration = 10, type = "error")
      }
    }else{
      showNotification(paste0("Error :", translate("ErrorDatosPN")), duration = 10, type = "error")
    }
  })
  
  # When the data transform button is pressed
  observeEvent(input$transButtonPredN, {
    code.trans.pn <<- transform_data_pn()

    updateSelectInput(session, "sel.predic.var.nuevos", choices = rev(colnames_empty(var_numerical(datos.aprendizaje.completos))))
    updateNumericInput(session, "mtry.rf.pred", value = round(sqrt(ncol(datos.aprendizaje.completos) -1)))

    modelo.nuevos <<- NULL
    predic.nuevos <<- NULL
    update_pred_pn("")

    update_model_text_pn("")
    update_table_pn()
  })

  # When the user presses the generate model button
  observeEvent(input$PredNuevosBttnModelo,{
    variable.predecir.pn <<- input$sel.predic.var.nuevos
    modelo.seleccionado.pn  <<- input$selectModelsPred

    codigo <- switch(input$selectModelsPred,
                     rl   = rl_model(data = "datos.aprendizaje.completos", 
                                     variable.pred = variable.predecir.pn, 
                                     model.var = "modelo.nuevos"),
                     rlr  = rlr_model(data = "datos.aprendizaje.completos",
                                       variable.pred = variable.predecir.pn,
                                       model.var = "modelo.nuevos",
                                       cv.var = "cv.glm.nuevos",
                                       alpha = input$alpha.rlr.pred,
                                       standardize = input$switch.scale.rlr.pred),
                     knn =  kkn_model(data = "datos.aprendizaje.completos",
                                      variable.pred = variable.predecir.pn,
                                      scale = input$switch.scale.knn.pred, 
                                      kmax = input$kmax.knn.pred,
                                      kernel = input$kernel.knn.pred, 
                                      model.var = "modelo.nuevos",
                                      distance = input$distance.knn.pred),
                     dt  = dt_model(data = "datos.aprendizaje.completos",
                                     variable.pred = variable.predecir.pn,
                                     model.var = "modelo.nuevos",
                                     minsplit = input$minsplit.dt.pred,
                                     maxdepth = input$maxdepth.dt.pred),
                     rf  = rf_model(data = "datos.aprendizaje.completos",
                                     variable.pred = variable.predecir.pn,
                                     model.var = "modelo.nuevos",
                                     ntree = input$ntree.rf.pred,
                                     mtry = input$mtry.rf.pred),
                     boosting = boosting_model(data = "datos.aprendizaje.completos",
                                                variable.pred = variable.predecir.pn,
                                                model.var = "modelo.nuevos",
                                                n.trees = input$iter.boosting.pred,
                                                distribution = input$tipo.boosting.pred,
                                                shrinkage = input$shrinkage.boosting.pre),
                     svm = svm_model(data = "datos.aprendizaje.completos",
                                     variable.pred = variable.predecir.pn,
                                     model.var = "modelo.nuevos",
                                     scale = input$switch.scale.svm.pred,
                                     kernel = input$kernel.svm.pred),
                     rd = rd_model(data = "datos.aprendizaje.completos",
                                   variable.pred = variable.predecir.pn,
                                   model.var = "modelo.nuevos",
                                   n.comp = "n.comp.rd.np",
                                   mode = input$mode.rd.pred,
                                   scale = input$switch.scale.rd.pred),
                     nn = nn_model(data = "datos.aprendizaje.completos",
                                   variable.pred = variable.predecir.pn,
                                   model.var = "modelo.nuevos",
                                   mean.var = "mean.nn.np",
                                   sd.var = "sd.nn.np",
                                   threshold = input$threshold.nn.pred,
                                   stepmax = input$stepmax.nn.pred,
                                   cant.hidden = input$cant.capas.nn.pred,
                                   input$nn.cap.pred.1,input$nn.cap.pred.2,
                                   input$nn.cap.pred.3,input$nn.cap.pred.4,
                                   input$nn.cap.pred.5,input$nn.cap.pred.6,
                                   input$nn.cap.pred.7,input$nn.cap.pred.8,
                                   input$nn.cap.pred.9,input$nn.cap.pred.10))

      modelo.nuevos <<- NULL
      predic.nuevos <<- NULL
      update_pred_pn("")
      
      tryCatch({
        if( (input$selectModelsPred == "boosting" &&
             !is.null(calibrate_boosting(datos.aprendizaje.completos)) ) ||
             input$selectModelsPred != "boosting" ){
          exe(codigo)
          update_model_text_pn(codigo)
        }else{
          showNotification(translate("ErrorBsize"), duration = 15, type = "error")
        }
      },
      error =  function(e){
        showNotification(paste0("Error: ", e), duration = 10, type = "error")
      },
      warning = function(w){
        if(input$selectModelsPred == "nn"){
          showNotification(paste0(translate("nnWar")," (NN-01) : ",w), duration = 20, type = "warning")
        }
      })
  })

  # When the user loads the data
  observeEvent(input$loadButtonNPred2,{
    codigo.carga <- code_load(row.names = input$rownameNPred2,
                              path = input$file3$datapath,
                              sep = input$sep.nPred2,
                              sep.dec = input$dec.nPred2,
                              header  = input$headerNPred2,
                              d.o = "datos.prueba.completos",
                              d = "datos.prueba.completos")

    tryCatch({
      isolate(exe(codigo.carga))
      codigo.na <- ""
      codigo.na <- paste0(code_NA(deleteNA = input$deleteNAnPred2,
                                  d.o = paste0("datos.prueba.completos")))
      datos.prueba.completos[,variable.predecir.pn] <<- NULL
      validate_pn_data(datos.originales.completos, datos.prueba.completos, variable.predecir.pn)
      isolate(exe( codigo.na))
      datos.prueba.completos[,variable.predecir.pn] <<- NA_real_
      code.trans.pn <<- gsub("datos.originales.completos", "datos.prueba.completos", code.trans.pn)
      code.trans.pn <<- gsub("datos.aprendizaje.completos", "datos.prueba.completos", code.trans.pn)
      exe(code.trans.pn)
      if(ncol(datos.prueba.completos) <= 1) {
        showNotification(translate("errorSeg"), duration = 10, type = "error")
        return(NULL)
      }
      update_table_pn("contentsPred3")
    },
    error = function(e) {
      showNotification(paste0("Error: ", e), duration = 10, type = "error")
      datos.prueba.completos <<- NULL
      predic.nuevos <<- NULL
      return(NULL)
    })
  })

  # PAGINA DE REPORTE -----------------------------------------------------------------------------------------------------

  # When the user enters the report page
  observeEvent(input$principal, {
    if(input$principal == "reporte"){
      extra_funs <- paste0(extract_code("pairs.panels"), "\n",
                           extract_code("dummy"),"\n",
                           extract_code("dummy.data.frame"),"\n",
                           extract_code("scatterplot3d"),"\n",
                           extract_code("printRandomForests"),"\n",
                           extract_code("printRandomForest"),"\n",
                           extract_code("printRandomForests"))
      updateAceEditor(session, "fieldCodeReport", value = word_report(input$textTitulo, input$textNombre, extra =  extra_funs))
    }
  })

  # When the user changes the report title
  observeEvent(input$textTitulo, {
    updateAceEditor(session, "fieldCodeReport", value = str_replace(input$fieldCodeReport, "title: '.*'", paste0("title: '", input$textTitulo, "'")))
  })

  # When the user changes the author of the report
  observeEvent(input$textNombre, {
    updateAceEditor(session, "fieldCodeReport", value = str_replace(input$fieldCodeReport, "author: '.*'", paste0("author: '", input$textNombre, "'")))
  })

  # When the user downloads the report
  output$descargar <- downloadHandler(
    filename = function() {
      paste(input$textTitulo,'-', input$textNombre, '.zip', sep='')
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL

      namermd <- paste(input$textTitulo,'-', input$textNombre, '.rmd', sep='')
      
      e <- options()$encoding
      options(encoding = regressoR:::enc)
      
      write.table(input$fieldCodeReport,namermd,row.names=F,col.names=F,quote=F)
      options(encoding = e)

      files <- c(namermd, files)

      src <- normalizePath(namermd)
      withCallingHandlers({
        overwrite_cat()
        salida.code <<- ""
        shinyjs::html("txtreport", salida.code)
        #merge_envir <- rlang::env_clone(get_env_report())
        #parent.env(merge_envir) <- options_regressor()$exe.envir
        out <- rmarkdown::render(src,  params = NULL, rmarkdown::word_document(highlight = "tango"), envir = get_env_report())
      },
      message = function(m) {
        salida.code <<- paste0(m$message, salida.code)
        shinyjs::html(id = "txtreport", html = salida.code)
      })

      recover_cat()
      file.rename(out, paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''))
      files <- c(paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''), files)

      zip::zip(file, files)
    }
  )

  # CHANGE LANGUAGE -------------------------------------------------------------------------------------------------------

  # Updates the labelInputs that the language changes
  updateLabelInput <- function (session, labelid, value = NULL) {
    message <- list(labelid = labelid)
    message <- message[!vapply(message, is.null, FUN.VALUE = logical(1))]
    if(length(labelid) == 1) {
      labelid <- list(labelid)
    }
    ifelse(is.null(value), sentvalue <- translate(labelid),
            ifelse(length(value) == 1, sentvalue <- list(value), sentvalue <- value))
    session$sendCustomMessage(type = 'updateLabel',
                              message = list(ids = labelid, values = sentvalue))
  }

  # When the user changes the language
  observeEvent(c(input$idioma), {
    options_regressor(language = input$idioma)
    
    updateLabelInput(session, c("idioma","selidioma","data","basico","resumen","normalidad",
                                "dispersion","distribucion","correlacion","poderpred","reporte",
                                "aprendizaje","acercade","comparacion","predicnuevos","knnl","dtl",
                                "rfl","bl","svml","cargar","header","Rownames","eliminana","si","no",
                                "cargarchivo","subir","trans","aplicar","separador","coma","puntocoma",
                                "tab","separadordec","punto","subir","configuraciones","semilla",
                                "habilitada","deshabilitada","seleccionarPredecir","propA","propP",
                                "generar","descargar","dataA","dataP","numerico","categorico","disyuntivo",
                                "resumenvar","selvar","plotnormal","opciones", "selcolor","selvars",
                                "selcolores","codigo","codedist","numericas","categoricas","ejecutar",
                                "selmetodo","seltipo","resultados","distpred","distpredcat","pares",
                                "denspred","generatem","predm","mc","indices","gclasificacion","garbol",
                                "reglas","evolerror","varImp","selkernel","kmax","escal","minsplit",
                                "maxdepth","splitIndex","numTree","numVars","ruleNumTree","selectAlg",
                                "rocCurva","tablaComp","selectMod","selectCat", "reporte","titulo",
                                "nombre","codreporte","salida","copyright","info","version","cargarNuev",
                                "cargarDatos","transDatos","seleParModel","generarM","variables","tipo",
                                "activa","nn","xgb","selbooster","selnrounds","selectCapas","threshold",
                                "stepmax","redPlot","rll","rlr","posibLanda","coeff","gcoeff",
                                "automatico","landa","shrinkage","resumenVarPre", "R2", "distknn",
                                "ncomp", "rd", "RdPred", "RdVarPred", "errRDnCom", "RMSE","eliminar", "imputar"))

    updatePlot$normal <- normal_default("datos", input$sel.normal, input$col.normal, translate("curvanormal"))
    updatePlot$dya.cat <- def_code_cat(variable = input$sel.distribucion.cat)
    updatePlot$calc.normal <- default_calc_normal(label.yes = translate("positivo"),label.no = translate("negativo"),label.without = translate("sinasimetria"))

    execute_knn_ind()
    execute_svm_ind()
    execute_rd_ind()
    execute_dt_ind()
    execute_rf_ind()
    execute_rl_ind()
  })

  # END THE SESSION -------------------------------------------------------------------------------------------------------

  # When the session closes
  session$onSessionEnded(function() {
    recover_cat()
    options_regressor(exe.envir = NULL)
    clean_report()
    stopApp()
  })
  
})
