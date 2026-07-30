#!/usr/bin/env Rscript
# Construye datasets canónicos Orígenes: Embajadores ⋈ Bubble RSCG.
# Uso: Rscript scripts/build_origenes_canonical.R

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

source(file.path(root, "data_nxtgen.R"), local = FALSE)
source(file.path(root, "data_brokers.R"), local = FALSE)
source(file.path(root, "R", "bubble_brokers.R"), local = FALSE)
source(file.path(root, "R", "ambassadors_api.R"), local = FALSE)
source(file.path(root, "R", "origenes_names.R"), local = FALSE)
source(file.path(root, "R", "origenes_join.R"), local = FALSE)

message("Construyendo canónicos Orígenes…")
canonical <- origenes_build_all_canonical(root = root, origins = c("NxtGen", "Broker"))
path <- origenes_save_canonical(canonical, root = root)

diag <- canonical$diagnostics
message("Fuentes: amb=", canonical$ambassadors_source, " | rscg=", canonical$rscg_source)
message(
  sprintf(
    "Join meetings: %d / %d (%.1f%%)",
    diag$meetings$matched,
    diag$meetings$amb_with_rscg_id,
    100 * diag$meetings$match_rate
  )
)
message(
  sprintf(
    "Join leads: %d / %d (%.1f%%)",
    diag$leads$matched_either,
    diag$leads$amb_with_rscg_id,
    100 * diag$leads$match_rate
  )
)

for (key in names(canonical$origins)) {
  m <- canonical$origins[[key]]$meta
  message(
    sprintf(
      "[%s] embajadores=%d | meetings_rscg=%d (hub-join=%d) | sales=%d | teams=%d | name_map=%d (renombrados=%d)",
      key,
      m$n_embajadores,
      m$n_meetings_rscg,
      m$n_meetings_joined_hub,
      m$n_sales,
      m$n_teams,
      m$n_name_map %||% 0L,
      m$n_embajadores_reconciled %||% 0L
    )
  )
}

message("Guardado: ", path)
