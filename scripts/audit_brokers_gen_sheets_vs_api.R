# Compara registros del Sheets Brokers vs contactos Ambassador Broker del API.
readRenviron(".Renviron")
Sys.setenv(ORIGENES_APP_ROOT = ".")
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
  library(readr)
})
source("data_nxtgen.R")
source("data_brokers.R")
source("R/bubble_brokers.R")
source("R/ambassadors_api.R")
source("R/origenes_names.R")
source("R/origenes_join.R")

contact <- ambassadors_fetch_endpoint("contact")
std <- origenes_standardize_amb_contacts(contact)
sheets <- read.csv(
  "data/brokers/raw/Brokers Academy Analytics - Registros de embajadores.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

norm_gen <- function(x) {
  x <- str_squish(gsub("[()]", "", as.character(x)))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

sh <- tibble(
  nombre = str_squish(sheets[["Nombre"]]),
  conciliacion = str_squish(sheets[["Conciliacion Nombre"]]),
  generacion = norm_gen(sheets[["Generación"]]),
  gen_registro = str_squish(sheets[["Gen Registro"]]),
  fecha = sheets[["Fecha de registro"]],
  canal = str_squish(sheets[["Canal"]]),
  graduacion = str_squish(sheets[["Fecha graduación"]])
) |>
  mutate(
    key_nombre = brokers_norm_name(nombre),
    key_conc = brokers_norm_name(conciliacion),
    generacion = coalesce(generacion, "(sin generación)"),
    gen_registro = ifelse(
      is.na(gen_registro) | !nzchar(gen_registro),
      "(sin gen registro)",
      gen_registro
    )
  )

api <- std |>
  filter(!is.na(nombre_key), nzchar(nombre_key)) |>
  distinct(nombre_key, .keep_all = TRUE)

match_type <- function(keys) {
  hit <- api$type_os[match(keys, api$nombre_key)]
  ifelse(is.na(hit), "no_en_api", hit)
}

sh <- sh |>
  mutate(
    match_nombre = match_type(key_nombre),
    match_conc = match_type(key_conc),
    match = case_when(
      match_nombre != "no_en_api" ~ match_nombre,
      match_conc != "no_en_api" ~ match_conc,
      TRUE ~ "no_en_api"
    ),
    match_via = case_when(
      match_nombre != "no_en_api" ~ "nombre",
      match_conc != "no_en_api" ~ "conciliacion",
      TRUE ~ NA_character_
    )
  )

message("=== SHEETS vs API type ===")
print(count(sh, match, sort = TRUE))

sheet_keys <- unique(c(sh$key_nombre, sh$key_conc))
sheet_keys <- sheet_keys[!is.na(sheet_keys) & nzchar(sheet_keys)]
brokers <- std |> filter(is_broker_ambassador)
solo_api <- brokers |>
  filter(!nombre_key %in% sheet_keys) |>
  mutate(mes = format(fecha_registro, "%Y-%m")) |>
  arrange(fecha_registro, nombre)

message("API broker ", nrow(brokers),
        " | sheets ", nrow(sh),
        " | solo API ", nrow(solo_api))

readr::write_csv(
  std |>
    dplyr::transmute(
      nombre = .data$nombre,
      nombre_key = .data$nombre_key,
      type_os = .data$type_os,
      fecha_registro = .data$fecha_registro,
      origin_os = .data$origin_os,
      email = .data$email
    ),
  "data/audit/amb_contacts_slim.csv"
)
write_csv(
  sh |> select(nombre, conciliacion, generacion, gen_registro, fecha, canal, graduacion, match, match_via),
  "data/audit/brokers_gen_sheets_vs_api.csv"
)
write_csv(
  solo_api |> transmute(nombre, nombre_key, fecha_registro, mes, type_os, email, origin_os, status_os),
  "data/audit/brokers_gen_solo_api.csv"
)
write_csv(
  sh |>
    filter(match != "Ambassador Broker") |>
    select(nombre, conciliacion, generacion, gen_registro, fecha, match, match_via),
  "data/audit/brokers_gen_sheets_no_broker.csv"
)

message("=== por Generación sheets ===")
print(sh |> count(generacion, match) |> arrange(generacion, desc(n)))
message("=== solo API por mes ===")
print(count(solo_api, mes))
