#' Launch My Shiny App
#'
#' This function starts the Shiny app included in the package.
#'
#' @return Shiny app webpage.
#' @importFrom shiny runApp
#' @export
launchmyapp <- function() {
  appdir <- system.file("shinyapp", package = "bis620.2023")
  if (appdir == "") {
    stop("Shiny app not found in the package")
  }
  shiny::runApp(appdir)
}
