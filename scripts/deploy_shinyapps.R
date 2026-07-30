#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
app_name <- if (length(args) >= 1L) args[[1L]] else "origenes"
account <- if (length(args) >= 2L) {
  args[[2L]]
} else {
  Sys.getenv("ORIGENES_SHINYAPPS_ACCOUNT", unset = "division2cbr")
}

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Instala rsconnect con install.packages('rsconnect').", call. = FALSE)
}

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(
    file.path(dirname(sub("^--file=", "", file_arg[[1L]])), ".."),
    winslash = "/"
  )
} else {
  normalizePath(".", winslash = "/")
}

required <- c("app.R", "DESCRIPTION", "R/ui.R", "R/server.R", "www/styles.css")
missing <- required[!file.exists(file.path(root, required))]
if (length(missing)) {
  stop("Faltan archivos requeridos: ", paste(missing, collapse = ", "), call. = FALSE)
}

data_files <- c(
  "data/cache/latest.rds",
  "data/origenes.rds",
  "data/origenes.csv"
)
available_data <- data_files[file.exists(file.path(root, data_files))]
if (!length(available_data)) {
  warning(
    "Se publicará la interfaz sin datos. Agrega data/cache/latest.rds, ",
    "data/origenes.rds o data/origenes.csv para habilitar el dashboard.",
    call. = FALSE
  )
}

message("Destino: https://", account, ".shinyapps.io/", app_name, "/")
if (length(available_data)) {
  message("Datos incluidos: ", paste(available_data, collapse = ", "))
}

rsconnect::deployApp(
  appDir = root,
  appName = app_name,
  account = account,
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = FALSE
)
