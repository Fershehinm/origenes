#!/usr/bin/env Rscript
# Analiza exports NxtGen Sheets vs Ambassadors/Bubble (solo diagnóstico).
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
})

root <- Sys.getenv("ORIGENES_APP_ROOT", unset = normalizePath("."))
setwd(root)
source("data_nxtgen.R", local = FALSE)
source("data_brokers.R", local = FALSE)
source("R/origenes_names.R", local = FALSE)
source("R/origenes_join.R", local = FALSE)

sheets_parse_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  parsed <- suppressWarnings(lubridate::parse_date_time(
    as.character(x),
    orders = c(
      "dmy HMS", "dmy HM", "dmy",
      "dmY HMS", "dmY HM", "dmY",
      "mdy HMS", "mdy HM", "mdy",
      "Ymd HMS", "Ymd HM", "Ymd"
    ),
    quiet = TRUE,
    tz = "UTC"
  ))
  as.Date(parsed)
}

ym <- function(d) {
  ifelse(is.na(d), NA_character_, paste(format(d, "%Y"), format(d, "%m"), sep = " - "))
}
eq <- function(a, b) !is.na(a) & !is.na(b) & a == b
pct <- function(n, d) if (!d) NA_real_ else round(100 * n / d, 1)

dir_nxt <- "data/nxtgen"
message("=== 1) REGISTROS ===")
reg <- read.csv(
  file.path(dir_nxt, "NxtGen Analytics - Registros de embajadores.csv"),
  stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"
) |>
  transmute(
    fecha = sheets_parse_date(Fecha),
    nombre_raw = str_squish(Nombre),
    conciliacion = str_squish(.data[["Conciliacion Nombre"]]),
    nombre = coalesce(na_if(conciliacion, ""), nombre_raw),
    gen_col = str_squish(Gen),
    fecha_1ra = sheets_parse_date(.data[["Fecha primera cita realizada"]]),
    fecha_ult_real = sheets_parse_date(.data[["Fecha de última cita realizada"]]),
    gen_registro = str_squish(.data[["Gen Registro"]]),
    dias_ult = suppressWarnings(as.numeric(.data[["Días desde la última cita realizadas"]])),
    fecha_ult_any = sheets_parse_date(.data[["Fecha de última cita agendada/realizada"]]),
    gen_1ra = str_squish(.data[["Gen primera cita realizada"]])
  ) |>
  mutate(
    nombre_key = brokers_norm_name(nombre),
    gen_from_fecha = ym(fecha),
    gen_1ra_from_fecha = ym(fecha_1ra),
    dias_from_ult_real = as.numeric(Sys.Date() - fecha_ult_real)
  )

message(sprintf(
  "Gen Registro == ym(Fecha): %s/%s (%.1f%%)",
  sum(eq(reg$gen_registro, reg$gen_from_fecha)), nrow(reg),
  pct(sum(eq(reg$gen_registro, reg$gen_from_fecha)), nrow(reg))
))
message(sprintf(
  "Gen 1ra == ym(Fecha 1ra): %s/%s",
  sum(eq(reg$gen_1ra, reg$gen_1ra_from_fecha), na.rm = TRUE),
  sum(!is.na(reg$fecha_1ra))
))
message(sprintf(
  "Gen col (Gen) filled: %s/%s",
  sum(nzchar(coalesce(reg$gen_col, ""))), nrow(reg)
))
# dias approx
ok_dias <- !is.na(reg$dias_ult) & !is.na(reg$fecha_ult_real)
message(sprintf(
  "Días ≈ hoy - última realizada (±2): %s/%s",
  sum(abs(reg$dias_ult[ok_dias] - reg$dias_from_ult_real[ok_dias]) <= 2),
  sum(ok_dias)
))

message("\n=== 2) CITAS ===")
cit <- read.csv(
  file.path(dir_nxt, "NxtGen Analytics - Citas.csv"),
  stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"
)
# normalize colnames
names(cit) <- str_squish(names(cit))
cit <- cit |>
  transmute(
    prospecto = str_squish(.data[["Nombre del Prospecto"]]),
    fecha_inicio = sheets_parse_date(.data[["Fecha de Inicio"]]),
    estatus = str_squish(Estatus),
    embajador = str_squish(Embajador),
    primera_cita = tolower(str_squish(.data[["Primera Cita"]])) %in% c("si", "sí", "yes", "true", "1"),
    fecha_creacion = sheets_parse_date(.data[["Fecha de Creación"]]),
    embajador_reg = str_squish(.data[["Embajador Registrado"]]),
    gen_cita = str_squish(.data[["Gen Cita"]])
  ) |>
  mutate(
    embajador_key = brokers_norm_name(embajador),
    embajador_reg_key = brokers_norm_name(embajador_reg),
    gen_from_inicio = ym(fecha_inicio),
    mismo_emb = embajador_key == embajador_reg_key |
      (is.na(embajador_reg) | !nzchar(embajador_reg) | embajador_reg_key == "")
  )

message(sprintf(
  "Gen Cita == ym(Fecha Inicio): %s/%s (%.1f%%)",
  sum(eq(cit$gen_cita, cit$gen_from_inicio)), nrow(cit),
  pct(sum(eq(cit$gen_cita, cit$gen_from_inicio)), nrow(cit))
))
message(sprintf(
  "Embajador == Embajador Registrado: %s/%s (%.1f%%)",
  sum(cit$mismo_emb, na.rm = TRUE), nrow(cit),
  pct(sum(cit$mismo_emb, na.rm = TRUE), nrow(cit))
))
message("Estatus:")
print(count(cit, estatus, sort = TRUE))
message("Primera cita si/no:")
print(count(cit, primera_cita))

message("\n=== 3) VENTAS ===")
ven <- read.csv(
  file.path(dir_nxt, "NxtGen Analytics - Ventas.csv"),
  stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"
)
ven <- ven |>
  transmute(
    fecha_firma = sheets_parse_date(.data[["Fecha de firma"]]),
    cliente = str_squish(.data[["Nombre de cliente"]]),
    origen = str_squish(Origen),
    proyecto = str_squish(Proyecto),
    recompra = tolower(str_squish(Recompra)),
    vendedor = str_squish(Vendedor),
    status = str_squish(Status),
    precio = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", .data[["Precio de venta"]]))),
    intencion = str_squish(Intencion),
    conciliacion_lead = str_squish(.data[["Conciliacion Lead"]]),
    gen_venta = str_squish(.data[["Gen Venta"]]),
    primera_cita = sheets_parse_date(.data[["Primera Cita"]]),
    gen_cita = str_squish(.data[["Gen Cita"]]),
    ciclo = suppressWarnings(as.numeric(.data[["Ciclo Venta"]]))
  ) |>
  mutate(
    gen_from_firma = ym(fecha_firma),
    gen_cita_from_1ra = ym(primera_cita),
    ciclo_from_dates = as.numeric(fecha_firma - primera_cita)
  )

message(sprintf(
  "Gen Venta == ym(Fecha firma): %s/%s",
  sum(eq(ven$gen_venta, ven$gen_from_firma)), nrow(ven)
))
message(sprintf(
  "Gen Cita == ym(Primera Cita): %s/%s (non-NA 1ra=%s)",
  sum(eq(ven$gen_cita, ven$gen_cita_from_1ra), na.rm = TRUE),
  sum(!is.na(ven$primera_cita)),
  sum(!is.na(ven$primera_cita))
))
message(sprintf(
  "Ciclo ≈ firma - 1ra (±1): %s/%s",
  sum(abs(ven$ciclo - ven$ciclo_from_dates) <= 1, na.rm = TRUE),
  sum(!is.na(ven$ciclo) & !is.na(ven$ciclo_from_dates))
))
message("Status / Origen / Recompra:")
print(count(ven, status, origen, recompra))

message("\n=== 4) LISTA ===")
lista <- origenes_read_embajador_lista(
  file.path(dir_nxt, "NxtGen Analytics - Lista de Embajadores.csv")
)
# Recompute lista from citas
cit_real <- cit |> filter(origenes_normalize_key(estatus) == "realizada")
lista_from_cit <- cit_real |>
  mutate(emb = coalesce(na_if(embajador_reg, ""), embajador)) |>
  filter(nzchar(emb), !is.na(fecha_inicio)) |>
  group_by(emb_key = brokers_norm_name(emb)) |>
  summarise(
    embajador = dplyr::first(emb),
    n_citas = dplyr::n(),
    ultima = max(fecha_inicio, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(dias = as.numeric(Sys.Date() - ultima))

cmp_lista <- lista |>
  dplyr::rename(n_lista = n_citas, dias_lista = dias_desde_ultima) |>
  left_join(
    lista_from_cit |> dplyr::rename(n_calc = n_citas, dias_calc = dias),
    by = c("embajador_key" = "emb_key")
  )
message(sprintf(
  "Lista matched to citas: %s/%s",
  sum(!is.na(cmp_lista$n_calc)), nrow(lista)
))
message(sprintf(
  "# citas exact match: %s/%s",
  sum(cmp_lista$n_lista == cmp_lista$n_calc, na.rm = TRUE),
  sum(!is.na(cmp_lista$n_calc))
))
message(sprintf(
  "# citas ±2: %s/%s",
  sum(abs(cmp_lista$n_lista - cmp_lista$n_calc) <= 2, na.rm = TRUE),
  sum(!is.na(cmp_lista$n_calc))
))

message("\n=== 5) CROSS: Sheets Citas vs Bubble meetings NxtGen ===")
rscg <- origenes_load_rscg_bundle()
mr <- rscg$meetings_raw

first_col <- intersect(c("First.Meeting", "First.meeting"), names(mr))
first_col <- if (length(first_col)) first_col[[1]] else NA_character_

bub <- tibble(
  id = origenes_id_norm(mr$X_id),
  prospecto = str_squish(as.character(mr$Lead.full.name)),
  start = as.Date(origenes_parse_date(mr$Start.date)),
  created = as.Date(origenes_parse_date(mr$Created.Date)),
  status = str_squish(as.character(mr$Status_OS)),
  embajador = str_squish(as.character(mr$Ambassador)),
  first_meeting = if (!is.na(first_col)) as.character(mr[[first_col]]) else NA_character_,
  origen = as.character(mr$Origin_OS)
) |>
  filter(origenes_origin_key(origen) == "nxtgen") |>
  mutate(
    prospecto_key = brokers_norm_name(prospecto),
    embajador_key = brokers_norm_name(embajador),
    primera = tolower(first_meeting) %in% c("true", "yes", "1"),
    gen = ym(start),
    status_key = origenes_normalize_key(status)
  )

message(sprintf(
  "Sheets citas=%s | Bubble NxtGen meetings=%s",
  nrow(cit), nrow(bub)
))
message(sprintf(
  "Sheets fecha_inicio parsed: %s/%s | range %s → %s",
  sum(!is.na(cit$fecha_inicio)), nrow(cit),
  min(cit$fecha_inicio, na.rm = TRUE), max(cit$fecha_inicio, na.rm = TRUE)
))

cit2 <- cit |>
  mutate(prospecto_key = brokers_norm_name(prospecto), day = fecha_inicio)
joined <- cit2 |>
  left_join(
    bub |>
      select(
        prospecto_key, day = start, bub_status = status_key, bub_emb = embajador_key,
        bub_primera = primera, bub_id = id, bub_created = created
      ),
    by = c("prospecto_key", "day"),
    relationship = "many-to-many"
  ) |>
  group_by(prospecto, fecha_inicio, embajador, estatus) |>
  slice(1) |>
  ungroup()

message(sprintf(
  "Citas Sheets matched Bubble (prospecto+día): %s/%s (%.1f%%)",
  sum(!is.na(joined$bub_id)), nrow(joined),
  pct(sum(!is.na(joined$bub_id)), nrow(joined))
))

# fallback: prospecto only within ±1 day
fb <- cit2 |>
  mutate(rid = dplyr::row_number()) |>
  left_join(
    bub |> select(prospecto_key, bub_day = start, bub_id = id, bub_emb = embajador_key),
    by = "prospecto_key",
    relationship = "many-to-many"
  ) |>
  filter(!is.na(bub_day), abs(as.numeric(day - bub_day)) <= 1) |>
  distinct(rid)
message(sprintf(
  "Citas matched Bubble (prospecto ±1 día): %s/%s (%.1f%%)",
  nrow(fb), nrow(cit2), pct(nrow(fb), nrow(cit2))
))

st <- joined |> filter(!is.na(bub_id)) |>
  mutate(
    sheets_st = origenes_normalize_key(estatus),
    status_eq = sheets_st == bub_status
  )
if (nrow(st)) {
  message(sprintf(
    "Status equal when matched: %s/%s (%.1f%%)",
    sum(st$status_eq), nrow(st), pct(sum(st$status_eq), nrow(st))
  ))
  message(sprintf(
    "Primera cita equal: %s/%s",
    sum(st$primera_cita == st$bub_primera, na.rm = TRUE), nrow(st)
  ))
  message(sprintf(
    "Embajador equal: %s/%s",
    sum(st$embajador_key == st$bub_emb, na.rm = TRUE), nrow(st)
  ))
  message(sprintf(
    "Fecha creación == Bubble Created.Date day: %s/%s",
    sum(eq(st$fecha_creacion, st$bub_created), na.rm = TRUE),
    sum(!is.na(st$fecha_creacion) & !is.na(st$bub_created))
  ))
}

message("\n=== 6) CROSS: Sheets Ventas vs Bubble sales NxtGen ===")
sales <- (rscg$sales_clean %||% tibble()) |>
  filter(origenes_origin_key(origen) == "nxtgen") |>
  mutate(
    cliente_key = brokers_norm_name(cliente_nombre),
    bub_fecha = as.Date(fecha),
    bub_status = origenes_normalize_key(estado_venta),
    bub_precio = coalesce(monto_facturacion, monto),
    bub_proyecto = proyectos,
    bub_vendedor = vendedor
  )
message(sprintf("Sheets ventas=%s | Bubble NxtGen sales=%s", nrow(ven), nrow(sales)))
ven2 <- ven |>
  mutate(cliente_key = brokers_norm_name(coalesce(na_if(conciliacion_lead, ""), cliente)))
sv_best <- ven2 |>
  left_join(
    sales |> select(cliente_key, bub_fecha, bub_status, bub_precio, bub_proyecto, bub_vendedor),
    by = "cliente_key",
    relationship = "many-to-many"
  ) |>
  group_by(cliente, fecha_firma) |>
  slice_min(order_by = abs(as.numeric(fecha_firma - bub_fecha)), n = 1, with_ties = FALSE) |>
  ungroup()
message(sprintf(
  "Ventas matched by cliente name: %s/%s",
  sum(!is.na(sv_best$bub_fecha)), nrow(ven2)
))
message(sprintf(
  "Fecha firma == Bubble fecha (±1d): %s/%s",
  sum(abs(as.numeric(sv_best$fecha_firma - sv_best$bub_fecha)) <= 1, na.rm = TRUE),
  sum(!is.na(sv_best$bub_fecha))
))
message(sprintf(
  "Status Firmado ↔ bubble firmado: %s/%s",
  sum(origenes_normalize_key(sv_best$status) == "firmado" &
        grepl("firmad", sv_best$bub_status), na.rm = TRUE),
  sum(!is.na(sv_best$bub_status))
))

message("\n=== 7) METRIC RECIPE (what Sheets tables count) ===")
message("-- Tabla Embajadores: cohort = Gen Registro --")
print(reg |> count(gen_registro, name = "registrados") |> arrange(gen_registro) |> tail(8))

message("-- Volumen citas por Gen Cita / ventas por Gen Venta --")
res_citas <- cit |>
  group_by(gen_cita) |>
  summarise(
    agendadas = n(),
    realizadas = sum(origenes_normalize_key(estatus) == "realizada"),
    primera_realizada = sum(primera_cita & origenes_normalize_key(estatus) == "realizada"),
    .groups = "drop"
  )
res_ventas <- ven |>
  filter(origenes_normalize_key(status) == "firmado") |>
  group_by(gen_venta) |>
  summarise(unidades = n(), fact = sum(precio, na.rm = TRUE), .groups = "drop")
print(
  res_citas |>
    filter(str_detect(coalesce(gen_cita, ""), "^2026")) |>
    left_join(res_ventas, by = c("gen_cita" = "gen_venta"))
)

message("\n=== 8) REGISTROS activity fields vs Citas sheet ===")
# For each embajador in reg, compute first/last realizada from citas
act <- cit |>
  filter(origenes_normalize_key(estatus) == "realizada") |>
  mutate(emb_key = brokers_norm_name(coalesce(na_if(embajador_reg, ""), embajador))) |>
  group_by(emb_key) |>
  summarise(
    calc_1ra = min(fecha_inicio, na.rm = TRUE),
    calc_ult = max(fecha_inicio, na.rm = TRUE),
    .groups = "drop"
  )
reg_act <- reg |>
  left_join(act, by = c("nombre_key" = "emb_key"))
message(sprintf(
  "Fecha 1ra sheets == min cita realizada: %s/%s",
  sum(eq(reg_act$fecha_1ra, reg_act$calc_1ra), na.rm = TRUE),
  sum(!is.na(reg_act$fecha_1ra) & !is.na(reg_act$calc_1ra))
))
message(sprintf(
  "Fecha últ realizada sheets == max cita realizada: %s/%s",
  sum(eq(reg_act$fecha_ult_real, reg_act$calc_ult), na.rm = TRUE),
  sum(!is.na(reg_act$fecha_ult_real) & !is.na(reg_act$calc_ult))
))

message("\nDone.")
