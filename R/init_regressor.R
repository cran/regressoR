#' Start regressoR
#' @title This function will start regressoR
#' @return Nothing
#' @description An interactive 'Shiny' application for data regression.
#' @details This starts the regressoR application on the user's local computer.
#' @keywords regressoR
#' @examples
#'  if(interactive()){
#'    init_regressor()
#'  }
init_regressor <- function(){
  Sys.setenv("LANGUAGE" = "ES")
  shiny::runApp(appDir = system.file("application", package = "regressoR"), launch.browser = TRUE)
}
