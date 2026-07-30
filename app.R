suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(plotly)
  library(scales)
  library(stringr)
  library(tibble)
  library(tidyr)
})

APP_ROOT <- Sys.getenv("ORIGENES_APP_ROOT", unset = "")
if (!nzchar(APP_ROOT)) {
  APP_ROOT <- normalizePath(".", winslash = "/", mustWork = FALSE)
}
Sys.setenv(ORIGENES_APP_ROOT = APP_ROOT)

renviron <- file.path(APP_ROOT, ".Renviron")
if (file.exists(renviron)) {
  readRenviron(renviron)
}
renviron_deploy <- file.path(APP_ROOT, ".Renviron.deploy")
if (file.exists(renviron_deploy)) {
  readRenviron(renviron_deploy)
}

source(file.path(APP_ROOT, "data_nxtgen.R"), local = FALSE)
source(file.path(APP_ROOT, "data_brokers.R"), local = FALSE)
source(file.path(APP_ROOT, "R", "bubble_brokers.R"), local = FALSE)
source(file.path(APP_ROOT, "R", "ambassadors_api.R"), local = FALSE)
source(file.path(APP_ROOT, "R", "origenes_names.R"), local = FALSE)
source(file.path(APP_ROOT, "R", "origenes_join.R"), local = FALSE)
source(file.path(APP_ROOT, "R", "origenes_resumen.R"), local = FALSE)
source(file.path(APP_ROOT, "R", "origenes_dt.R"), local = FALSE)
source(file.path(APP_ROOT, "R", "origenes_ventas.R"), local = FALSE)
source(file.path(APP_ROOT, "R", "origenes_citas.R"), local = FALSE)
source(file.path(APP_ROOT, "R", "ui.R"), local = FALSE)
source(file.path(APP_ROOT, "R", "server.R"), local = FALSE)

shiny::shinyApp(
  ui = origenes_ui(),
  server = origenes_server
)
