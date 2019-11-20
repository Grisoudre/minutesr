minutesr <- function() {
  appDir <- system.file("minutesr", "myapp", package = "minutesr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `minutesr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
