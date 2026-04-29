#rsconnect::deployApp("/Users/michaelabers/Desktop/HPV")

#' Run shiny app v1
#'
#' @export
runV1 <- function() {
  appDir <- system.file("v1", package = "HPV")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `HPV`", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
