#' Launch the projectLSA Shiny Application
#'
#' This function starts the Shiny app included in the projectLSA package.
#'
#' @return Launches the Shiny application in the default web browser.
#' @export
run_projectLSA <- function() {
  app_dir <- system.file("app", package = "projectLSA")
  if (app_dir == "") {
    stop("Could not find Shiny app. Try reinstalling the projectLSA package.", call. = FALSE)
  }
  shiny::runApp(app_dir, launch.browser = TRUE)
}

