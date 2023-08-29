#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "WisNormPlot", package = "WisNorms")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `WisNorms`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
