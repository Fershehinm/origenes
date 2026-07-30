#!/usr/bin/env Rscript
# Auditoría Registrados NxtGen: dash (hub) vs Sheets.
#
# Uso:
#   Rscript scripts/audit_nxtgen_registrados.R 2026-01
#   Rscript scripts/audit_nxtgen_registrados.R 2026-01 path/al/sheets_registros.csv
#
# Si no pasas el CSV de Sheets, exporta solo el lado dash + instrucciones.
# El CSV de Sheets debe tener al menos: Nombre (o Embajador) y Fecha de registro
# (o Gen Registro tipo "2026 - 01"). Ideal: export de la pestaña Registros/Embajadores.

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

args <- commandArgs(trailingOnly = TRUE)
ym <- if (length(args) >= 1L) args[[1L]] else format(Sys.Date(), "%Y-%m")
sheets_path <- if (length(args) >= 2L) args[[2L]] else NA_character_

if (!grepl("^[0-9]{4}-[0-9]{2}$", ym)) {
  stop("Periodo debe ser YYYY-MM, ej. 2026-01", call. = FALSE)
}
period_start <- as.Date(paste0(ym, "-01"))
period_end <- as.Date(lubridate::ceiling_date(period_start, "month") - 1)

source(file.path(root, "data_nxtgen.R"), local = FALSE)
source(file.path(root, "data_brokers.R"), local = FALSE)
source(file.path(root, "R", "origenes_names.R"), local = FALSE)
source(file.path(root, "R", "origenes_join.R"), local = FALSE)
source(file.path(root, "R", "origenes_resumen.R"), local = FALSE)

out_dir <- file.path(root, "data", "audit")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

joined <- origenes_load_joined(root)
if (is.null(joined)) {
  stop("Falta data/canonical/origenes_joined.rds", call. = FALSE)
}

prep <- origenes_resumen_prepare(joined$origins$nxtgen, as_of = Sys.Date())
lista <- origenes_read_embajador_lista(
  file.path(root, "data", "nxtgen", "NxtGen Analytics - Lista de Embajadores.csv")
)

dash <- prep$embajadores |>
  dplyr::filter(
    !is.na(.data$fecha_registro),
    .data$fecha_registro >= period_start,
    .data$fecha_registro <= period_end
  ) |>
  dplyr::mutate(
    nombre_key = brokers_norm_name(.data$nombre),
    en_lista = .data$nombre_key %in% lista$embajador_key,
    fuente = "dash_hub"
  ) |>
  dplyr::arrange(.data$fecha_registro, .data$nombre)

dash_path <- file.path(out_dir, paste0("nxtgen_registrados_", ym, "_dash.csv"))
utils::write.csv(dash, dash_path, row.names = FALSE, fileEncoding = "UTF-8")

message(sprintf(
  "DASH %s: %d filas | %d keys únicas | en lista=%d | fuera lista=%d",
  ym,
  nrow(dash),
  dplyr::n_distinct(dash$nombre_key),
  sum(dash$en_lista),
  sum(!dash$en_lista)
))
message("Exportado: ", dash_path)

origenes_read_sheets_registros <- function(path) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  keys <- gsub("[^a-z0-9]", "", tolower(names(raw)))

  name_col <- which(keys %in% c(
    "nombre", "embajador", "embajadorregistrado"
  ))[1]
  if (is.na(name_col)) {
    stop("No encuentro columna Nombre/Embajador en Sheets CSV", call. = FALSE)
  }

  # Preferir "Fecha de registro" / "Fecha"; evitar columnas de primera/última cita.
  fecha_col <- which(keys %in% c("fechaderegistro", "fecharegistro", "fecha"))[1]
  # Preferir "Gen Registro" sobre "Gen" / "Generación".
  gen_col <- which(keys == "genregistro")[1]
  if (is.na(gen_col)) {
    gen_col <- which(keys %in% c("generacion"))[1]
  }
  conciliacion_col <- which(keys %in% c("conciliacionnombre", "conciliacion"))[1]

  nombre <- stringr::str_squish(as.character(raw[[name_col]]))
  if (!is.na(conciliacion_col)) {
    conc <- stringr::str_squish(as.character(raw[[conciliacion_col]]))
    nombre <- dplyr::coalesce(dplyr::na_if(conc, ""), nombre)
  }

  fecha <- if (!is.na(fecha_col)) {
    origenes_parse_date(raw[[fecha_col]])
  } else {
    as.Date(rep(NA, length(nombre)))
  }

  gen <- if (!is.na(gen_col)) {
    stringr::str_squish(as.character(raw[[gen_col]]))
  } else {
    rep(NA_character_, length(nombre))
  }

  tibble::tibble(
    nombre = nombre,
    fecha_registro = fecha,
    gen_registro = gen,
    fuente = "sheets"
  ) |>
    dplyr::filter(!is.na(.data$nombre), nzchar(.data$nombre)) |>
    dplyr::mutate(
      nombre_key = brokers_norm_name(.data$nombre),
      gen_norm = dplyr::coalesce(
        dplyr::na_if(.data$gen_registro, ""),
        dplyr::if_else(
          !is.na(.data$fecha_registro),
          paste(format(.data$fecha_registro, "%Y"), format(.data$fecha_registro, "%m"), sep = " - "),
          NA_character_
        )
      )
    )
}

if (is.na(sheets_path) || !nzchar(sheets_path)) {
  default_sheets <- file.path(
    root, "data", "nxtgen",
    paste0("NxtGen Analytics - Registros ", ym, ".csv")
  )
  alt <- file.path(root, "data", "nxtgen", "NxtGen Analytics - Registros de embajadores.csv")
  if (file.exists(default_sheets)) {
    sheets_path <- default_sheets
  } else if (file.exists(alt)) {
    sheets_path <- alt
  }
}

if (is.na(sheets_path) || !file.exists(sheets_path)) {
  message("")
  message("Falta el export de Sheets para comparar.")
  message("Pon el CSV en una de estas rutas y re-ejecuta:")
  message("  data/nxtgen/NxtGen Analytics - Registros de embajadores.csv")
  message(sprintf("  data/nxtgen/NxtGen Analytics - Registros %s.csv", ym))
  message("O: Rscript scripts/audit_nxtgen_registrados.R ", ym, " /ruta/al.csv")
  message("")
  message("Columnas útiles: Nombre, Conciliacion Nombre, Fecha de registro, Gen Registro")
  quit(save = "no", status = 0)
}

sheets_all <- origenes_read_sheets_registros(sheets_path)
gen_label <- paste(format(period_start, "%Y"), format(period_start, "%m"), sep = " - ")
sheets <- sheets_all |>
  dplyr::filter(
    (!is.na(.data$fecha_registro) &
      .data$fecha_registro >= period_start &
      .data$fecha_registro <= period_end) |
      (!is.na(.data$gen_norm) & .data$gen_norm == gen_label)
  ) |>
  dplyr::distinct(.data$nombre_key, .keep_all = TRUE)

sheets_path_out <- file.path(out_dir, paste0("nxtgen_registrados_", ym, "_sheets.csv"))
utils::write.csv(sheets, sheets_path_out, row.names = FALSE, fileEncoding = "UTF-8")

only_dash <- dash |>
  dplyr::anti_join(sheets, by = "nombre_key") |>
  dplyr::transmute(
    lado = "solo_dash",
    nombre = .data$nombre,
    nombre_key = .data$nombre_key,
    fecha_registro = .data$fecha_registro,
    en_lista = .data$en_lista
  )
only_sheets <- sheets |>
  dplyr::anti_join(dash, by = "nombre_key") |>
  dplyr::transmute(
    lado = "solo_sheets",
    nombre = .data$nombre,
    nombre_key = .data$nombre_key,
    fecha_registro = .data$fecha_registro,
    en_lista = .data$nombre_key %in% lista$embajador_key
  )
both <- dash |>
  dplyr::inner_join(
    sheets |> dplyr::select(nombre_key, nombre_sheets = nombre, fecha_sheets = fecha_registro),
    by = "nombre_key"
  ) |>
  dplyr::transmute(
    lado = "ambos",
    nombre = .data$nombre,
    nombre_key = .data$nombre_key,
    fecha_dash = .data$fecha_registro,
    fecha_sheets = .data$fecha_sheets,
    en_lista = .data$en_lista
  )

diff <- dplyr::bind_rows(only_dash, only_sheets)
diff_path <- file.path(out_dir, paste0("nxtgen_registrados_", ym, "_diff.csv"))
utils::write.csv(diff, diff_path, row.names = FALSE, fileEncoding = "UTF-8")
both_path <- file.path(out_dir, paste0("nxtgen_registrados_", ym, "_ambos.csv"))
utils::write.csv(both, both_path, row.names = FALSE, fileEncoding = "UTF-8")

message(sprintf(
  "SHEETS %s: %d | AMBOS: %d | SOLO DASH: %d | SOLO SHEETS: %d",
  ym, nrow(sheets), nrow(both), nrow(only_dash), nrow(only_sheets)
))
message("Diff: ", diff_path)
if (nrow(only_dash)) {
  message("Solo en dash:")
  message(paste(" -", only_dash$nombre, collapse = "\n"))
}
if (nrow(only_sheets)) {
  message("Solo en Sheets:")
  message(paste(" -", only_sheets$nombre, collapse = "\n"))
}
