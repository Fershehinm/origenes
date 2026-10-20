# Diff por mes de registrados, ya con las reglas del dash:
# Gen Registro del Sheets, altas históricas y una persona por mes.
readRenviron(".Renviron")
Sys.setenv(ORIGENES_APP_ROOT = normalizePath(".", winslash = "/", mustWork = FALSE))
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
})
source("data_nxtgen.R")
source("data_brokers.R")
source("R/bubble_brokers.R")
source("R/ambassadors_api.R")
source("R/origenes_names.R")
source("R/origenes_join.R")
source("R/origenes_brokers_roster.R")
source("R/origenes_resumen.R")

root <- Sys.getenv("ORIGENES_APP_ROOT")
joined <- origenes_load_joined(root)
origin <- joined$origins$broker
roster <- origenes_brokers_roster_build(origin, root = root)
prep <- origenes_resumen_prepare(origin, roster = roster)
emb <- prep$embajadores

alias <- origenes_brokers_fold_alias(root)
canon <- function(x) origenes_brokers_canon_key(x, alias)

sheet_path <- origenes_brokers_embajadores_sheet_path(root)
raw <- utils::read.csv(sheet_path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
mes_m <- stringr::str_match(
  stringr::str_squish(as.character(raw[["Gen Registro"]] %||% "")),
  "(\\d{4})\\s*-\\s*(\\d{2})"
)
fecha_txt <- stringr::str_squish(as.character(raw[["Fecha de registro"]] %||% ""))
sh <- tibble::tibble(
  nombre = stringr::str_squish(as.character(raw[["Nombre"]] %||% "")),
  fecha_txt = fecha_txt,
  fecha = origenes_brokers_parse_sheet_fecha(fecha_txt),
  mes = ifelse(is.na(mes_m[, 2]), "", paste(mes_m[, 2], mes_m[, 3], sep = "-")),
  k_nom = canon(raw[["Nombre"]]),
  k_conc = canon(raw[["Conciliacion Nombre"]])
)
corte <- max(sh$fecha, na.rm = TRUE)

# Una persona por mes en el Sheets (la repetición del mismo mes no cuenta).
sh_person <- sh |>
  dplyr::mutate(canon = dplyr::if_else(nzchar(.data$k_nom), .data$k_nom, .data$k_conc)) |>
  dplyr::filter(nzchar(.data$canon), nzchar(.data$mes)) |>
  dplyr::group_by(.data$canon, .data$mes) |>
  dplyr::summarise(
    nombre_en_sheets = dplyr::first(.data$nombre),
    fecha_sheets = dplyr::first(.data$fecha_txt),
    .groups = "drop"
  )

dash_rows <- lapply(seq_len(nrow(emb)), function(i) {
  fechas <- as.Date(strsplit(as.character(emb$fechas_cohorte[i]), "|", fixed = TRUE)[[1]])
  fechas <- fechas[!is.na(fechas)]
  if (!length(fechas)) {
    return(NULL)
  }
  k <- canon(dplyr::coalesce(emb$nombre_raw[i] %||% "", emb$nombre[i]))
  if (!nzchar(k)) {
    k <- canon(emb$nombre[i])
  }
  tibble::tibble(
    canon = k,
    mes = format(fechas, "%Y-%m"),
    nombre_en_dash = emb$nombre[i],
    nombre_en_app = dplyr::coalesce(emb$nombre_raw[i] %||% "", emb$nombre[i]),
    fecha_dash = format(fechas, "%Y-%m-%d")
  )
})
dash <- dplyr::bind_rows(dash_rows) |>
  dplyr::distinct(.data$canon, .data$mes, .keep_all = TRUE)

solo_dash <- dplyr::anti_join(dash, sh_person, by = c("canon", "mes"))
solo_sheet <- dplyr::anti_join(sh_person, dash, by = c("canon", "mes"))

rows_dash <- solo_dash |>
  dplyr::left_join(
    sh_person |> dplyr::select("canon", "nombre_en_sheets", "mes_en_sheets" = "mes", "fecha_sheets"),
    by = "canon"
  ) |>
  dplyr::mutate(
    en_sheet = !is.na(.data$nombre_en_sheets),
    situacion = dplyr::case_when(
      !.data$en_sheet & .data$fecha_dash > as.character(corte) ~ "El export del Sheets no llega a esta fecha",
      !.data$en_sheet ~ "No está en el Sheets",
      TRUE ~ "Sí está en el Sheets, en otra generación"
    ),
    preguntar = dplyr::if_else(.data$situacion == "No está en el Sheets", "sí", "no"),
    nombre_en_sheets = dplyr::coalesce(.data$nombre_en_sheets, ""),
    mes_en_sheets = dplyr::coalesce(.data$mes_en_sheets, ""),
    fecha_sheets = dplyr::coalesce(.data$fecha_sheets, "")
  ) |>
  dplyr::select(
    "mes", "nombre_en_dash", "nombre_en_app", "fecha_dash",
    "nombre_en_sheets", "mes_en_sheets", "fecha_sheets", "situacion", "preguntar"
  )

rows_sheet <- solo_sheet |>
  dplyr::transmute(
    mes = .data$mes,
    nombre_en_dash = "",
    nombre_en_app = "",
    fecha_dash = "",
    nombre_en_sheets = .data$nombre_en_sheets,
    mes_en_sheets = .data$mes,
    fecha_sheets = .data$fecha_sheets,
    situacion = "Está en el Sheets y no está en el dash",
    preguntar = "sí"
  )

out <- dplyr::bind_rows(rows_dash, rows_sheet) |>
  dplyr::arrange(.data$mes, .data$preguntar != "sí", .data$nombre_en_dash, .data$nombre_en_sheets)

path <- file.path(root, "data", "audit", "brokers_registrados_diff_por_mes.csv")
readr::write_csv(out, path)

message("dash personas ", nrow(emb), " | persona-mes ", nrow(dash), " | sheets persona-mes ", nrow(sh_person))
message("diff ", nrow(out))
print(dplyr::count(out, situacion, sort = TRUE))
message("por mes dash vs sheets (persona única)")
meses <- sort(unique(c(dash$mes, sh_person$mes)))
cmp <- tibble::tibble(
  mes = meses,
  dash = vapply(meses, function(m) sum(dash$mes == m), integer(1)),
  sheets = vapply(meses, function(m) sum(sh_person$mes == m), integer(1))
) |>
  dplyr::mutate(delta = .data$dash - .data$sheets)
print(cmp)
