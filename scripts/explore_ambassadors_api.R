#!/usr/bin/env Rscript
# Descubre objetos en la Data API de Embajadores (ambassadors.mx).
# Uso: Rscript scripts/explore_ambassadors_api.R

root <- Sys.getenv("ORIGENES_APP_ROOT", unset = "")
if (!nzchar(root)) {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) {
    root <- normalizePath(
      file.path(dirname(sub("^--file=", "", file_arg[[1L]])), ".."),
      winslash = "/"
    )
  } else {
    root <- normalizePath(".", winslash = "/")
  }
}
Sys.setenv(ORIGENES_APP_ROOT = root)

renv <- file.path(root, ".Renviron")
if (file.exists(renv)) {
  readRenviron(renv)
}

source(file.path(root, "R", "bubble_brokers.R"), local = FALSE)
source(file.path(root, "R", "ambassadors_api.R"), local = FALSE)

if (!ambassadors_configured()) {
  stop("Configura AMBASSADORS_BASE y AMBASSADORS_TOKEN en .Renviron", call. = FALSE)
}

out_dir <- file.path(root, "data", "ambassadors", "explore")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("Explorando API Embajadores…")
discovery <- ambassadors_discover_types()
catalog <- discovery$catalog

utils::write.csv(
  catalog,
  file.path(out_dir, "object_catalog.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

ok <- catalog[isTRUE(catalog$ok) | (!is.na(catalog$status) & catalog$status < 400L), , drop = FALSE]
message("Meta status: ", discovery$meta_status %||% NA)
message("Tipos en /meta: ", length(discovery$meta_types))
if (length(discovery$meta_types)) {
  message("  ", paste(discovery$meta_types, collapse = ", "))
}
message("Endpoints OK con datos: ", sum(catalog$ok, na.rm = TRUE))
print(catalog[catalog$ok %in% TRUE, c("endpoint", "status", "n_sample", "remaining"), drop = FALSE])

# Extrae completo cada tipo OK
bundle <- list(fetched_at = Sys.time(), objects = list())
for (endpoint in catalog$endpoint[catalog$ok %in% TRUE]) {
  message("Descargando: ", endpoint)
  df <- ambassadors_fetch_endpoint(endpoint, page_size = 100L, max_pages = 300L)
  bundle$objects[[endpoint]] <- df
  saveRDS(df, file.path(out_dir, paste0(endpoint, ".rds")))
  utils::write.csv(
    utils::head(df, 50L),
    file.path(out_dir, paste0(endpoint, "_sample50.csv")),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  message("  filas=", nrow(df), " cols=", ncol(df))
}

saveRDS(bundle, file.path(out_dir, "bundle_ok.rds"))

# Resumen columnas
col_summary <- lapply(names(bundle$objects), function(nm) {
  df <- bundle$objects[[nm]]
  tibble::tibble(
    endpoint = nm,
    n_rows = nrow(df),
    n_cols = ncol(df),
    columns = paste(names(df), collapse = " | ")
  )
})
col_summary <- dplyr::bind_rows(col_summary)
utils::write.csv(
  col_summary,
  file.path(out_dir, "columns_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

message("Listo: ", out_dir)
