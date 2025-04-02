#' Launch the IEHFC Shiny App
#'
#' This function starts the IEHFC Shiny application.
#' @export
iehfc_app <- function() {

  requireNamespace("shiny", quietly = TRUE)

  # Get the installed package path
  app_dir <- system.file("iehfc_app", package = "iehfc")

  # Ensure "www" directory is correctly registered
  www_path <- system.file("iehfc_app/www", package = "iehfc")

  if (nzchar(www_path) && dir.exists(www_path)) {
    message("Registering www path: ", www_path)
    shiny::addResourcePath("www", www_path)
  } else {
    warning("www/ directory not found in installed package: ", www_path)
  }

  # Check if the directory exists
  if (app_dir == "") {
    stop("The application directory is not found. Try reinstalling `iehfc`.", call. = FALSE)
  }

  # Function to load UI and server
  extract_source <- function(filename) {
    source(file.path(app_dir, filename), local = parent.frame(), chdir = TRUE)$value
  }

  # Run the Shiny app
  shiny::shinyApp(
    ui = extract_source("iehfc_ui.R"),
    server = extract_source("iehfc_server.R")
  )
}



