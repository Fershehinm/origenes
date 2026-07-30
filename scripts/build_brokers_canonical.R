#!/usr/bin/env Rscript
# Construye tablas canónicas de Brokers desde data/brokers/raw/

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE)
} else {
  NA_character_
}

root <- if (!is.na(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
} else {
  normalizePath(".", winslash = "/", mustWork = FALSE)
}

if (!file.exists(file.path(root, "data_brokers.R"))) {
  stop("No se encontró data_brokers.R desde root=", root, call. = FALSE)
}

Sys.setenv(ORIGENES_APP_ROOT = root)

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(tibble)
})

source(file.path(root, "data_nxtgen.R"), local = FALSE)
source(file.path(root, "data_brokers.R"), local = FALSE)

canonical <- brokers_build_canonical(root = root, as_of = as.Date("2026-07-20"), write = TRUE)
q <- canonical$quality

cat("\n=== Brokers canonical build ===\n")
cat("root: ", root, "\n", sep = "")
cat("citas: ", q$citas_n, "\n", sep = "")
cat("embajadores: ", q$embajadores_n, "\n", sep = "")
cat("ventas: ", q$ventas_n,
    " (firmadas=", q$ventas_firmadas,
    ", en_proceso=", q$ventas_en_proceso, ")\n", sep = "")
cat(sprintf("tasa realizada: %.1f%%\n", 100 * q$tasa_realizada))
cat(sprintf("share fin de semana: %.1f%%\n", 100 * q$share_fin_semana))
cat(sprintf("share origin leader (Mauricio): %.1f%%\n", 100 * q$share_origin_leader))
cat("name_map filas: ", q$name_map_n, "\n", sep = "")
print(q$name_map_fuentes)
cat("match ventas: ")
print(q$match_venta_status)
cat("\nEscrito en: ", brokers_canonical_dir(root), "\n", sep = "")

monthly <- canonical$citas |>
  dplyr::filter(!is.na(.data$gen_cita)) |>
  dplyr::group_by(.data$gen_cita) |>
  dplyr::summarise(
    citas = dplyr::n(),
    realizadas = sum(.data$realizada),
    canceladas = sum(.data$cancelada),
    tasa_realizada = mean(.data$realizada),
    share_fds = mean(.data$es_fin_semana),
    share_ol = mean(.data$vendedor_rol == "origin_leader"),
    mes_incompleto = any(.data$mes_incompleto),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$gen_cita)

cat("\n=== Agendada → realizada por mes ===\n")
print(as.data.frame(monthly), row.names = FALSE)

reg_conv <- canonical$embajadores |>
  dplyr::filter(!is.na(.data$gen_registro)) |>
  dplyr::group_by(.data$gen_registro) |>
  dplyr::summarise(
    registrados = dplyr::n(),
    con_primera_cita = sum(.data$tiene_primera_cita),
    tasa = mean(.data$tiene_primera_cita),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$gen_registro)

cat("\n=== Registro → 1ª cita por mes de registro ===\n")
print(as.data.frame(reg_conv), row.names = FALSE)

ventas_m <- canonical$ventas |>
  dplyr::group_by(.data$gen_venta) |>
  dplyr::summarise(
    firmadas = sum(.data$cuenta_conversion),
    en_proceso = sum(.data$es_en_proceso),
    proyeccion = sum(.data$cuenta_proyeccion),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$gen_venta)

cat("\n=== Ventas (Firmado vs en proceso) ===\n")
print(as.data.frame(ventas_m), row.names = FALSE)

ambiguous <- canonical$citas |>
  dplyr::count(.data$embajador_raw, .data$embajador, name = "n") |>
  dplyr::filter(.data$embajador_raw != .data$embajador) |>
  dplyr::arrange(dplyr::desc(.data$n))

cat("\n=== Alias aplicados (revisar / validar) ===\n")
print(as.data.frame(utils::head(ambiguous, 20)), row.names = FALSE)
