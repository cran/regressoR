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
validate_data <- function(updateData, print = TRUE, idioma = "es") {
  if (is.null(updateData$variable.predecir) && print) {
    showNotification(tr("tieneVP",idioma), duration = 10, type = "error")
  }
  if (is.null(updateData$datos) && print) {
    showNotification(tr("tieneD",idioma), duration = 10, type = "error")
  }
  if (is.null(updateData$datos.aprendizaje) && print) {
    showNotification(tr("tieneDAP",idioma), duration = 10, type = "error")
  }
  return(!is.null(updateData$datos) && !is.null(updateData$variable.predecir) && !is.null(updateData$datos.aprendizaje))
}
 
