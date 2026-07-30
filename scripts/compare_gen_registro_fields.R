#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
})

root <- Sys.getenv("ORIGENES_APP_ROOT", unset = normalizePath("."))
setwd(root)
source("data_nxtgen.R", local = FALSE)
source("data_brokers.R", local = FALSE)
source("R/origenes_names.R", local = FALSE)
source("R/origenes_join.R", local = FALSE)

ym <- function(d) {
  ifelse(is.na(d), NA_character_, paste(format(d, "%Y"), format(d, "%m"), sep = " - "))
}
eq_day <- function(a, b) !is.na(a) & !is.na(b) & a == b
eq_ym <- function(a, b) !is.na(a) & !is.na(b) & a == b

sheets <- read.csv(
  "data/nxtgen/NxtGen Analytics - Registros de embajadores.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
) |>
  transmute(
    nombre_raw = str_squish(Nombre),
    conciliacion = str_squish(.data[["Conciliacion Nombre"]]),
    nombre = coalesce(na_if(conciliacion, ""), nombre_raw),
    fecha = origenes_parse_date(Fecha),
    gen = str_squish(.data[["Gen Registro"]]),
    fecha_1ra_sheets = origenes_parse_date(.data[["Fecha primera cita realizada"]])
  ) |>
  mutate(
    nombre_key = brokers_norm_name(nombre),
    nombre_raw_key = brokers_norm_name(nombre_raw),
    gen = coalesce(na_if(gen, ""), ym(fecha))
  )

amb <- readRDS("data/ambassadors/explore/Contact.rds")
meet <- readRDS("data/ambassadors/explore/Meeting.rds")

hub <- tibble(
  amb_id = origenes_id_norm(amb$X_id),
  nombre = str_squish(coalesce(
    amb$Full.Name,
    paste(coalesce(amb$First.Name, ""), coalesce(amb$Last.Name, ""))
  )),
  type_os = as.character(amb$Type_OS),
  created = origenes_parse_date(amb$Created.Date),
  modified = origenes_parse_date(amb$Modified.Date),
  last_meeting = origenes_parse_date(amb$Last.Meeting.Date)
) |>
  mutate(
    nombre_key = brokers_norm_name(nombre),
    is_nxtgen = !is.na(type_os) & origenes_normalize_key(type_os) == "ambassadornxtgen"
  )

meet_std <- origenes_standardize_amb_meetings(meet)
hub_meets <- meet_std |>
  filter(!is.na(amb_contact_id), !is.na(fecha_cita)) |>
  group_by(amb_contact_id) |>
  summarise(
    first_meet_hub = min(fecha_cita),
    first_realizada_hub = {
      d <- fecha_cita[origenes_normalize_key(status_os) == "realizada"]
      if (length(d)) min(d) else as.Date(NA)
    },
    .groups = "drop"
  )

rscg <- origenes_load_rscg_bundle()
mr <- rscg$meetings_raw
first_col <- intersect(c("First.Meeting", "First.meeting", "first_meeting"), names(mr))
first_col <- if (length(first_col)) first_col[[1]] else NA_character_

bub <- tibble(
  embajador = str_squish(as.character(mr$Ambassador)),
  origen = as.character(mr$Origin_OS),
  start = origenes_parse_date(mr$Start.date),
  created = origenes_parse_date(mr$Created.Date),
  first_meeting = if (!is.na(first_col)) as.character(mr[[first_col]]) else NA_character_
) |>
  filter(!is.na(embajador), nzchar(embajador)) |>
  mutate(
    nombre_key = brokers_norm_name(embajador),
    origin_key = origenes_origin_key(origen)
  )

agg_bub <- function(df) {
  df |>
    group_by(nombre_key) |>
    summarise(
      first_cita = {
        d <- start[!is.na(start)]
        if (length(d)) min(d) else as.Date(NA)
      },
      first_created_meet = {
        d <- created[!is.na(created)]
        if (length(d)) min(d) else as.Date(NA)
      },
      first_primera_cita = {
        ok <- tolower(as.character(first_meeting)) %in% c("true", "yes", "1")
        d <- start[ok & !is.na(start)]
        if (length(d)) min(d) else as.Date(NA)
      },
      .groups = "drop"
    )
}

bub_ng <- agg_bub(bub |> filter(origin_key == "nxtgen"))
bub_all <- agg_bub(bub)

pick_hub <- function(key, raw_key) {
  hit <- hub |> filter(nombre_key == key)
  if (!nrow(hit)) hit <- hub |> filter(nombre_key == raw_key)
  if (!nrow(hit)) {
    return(hit[0, ])
  }
  if (isTRUE(any(hit$is_nxtgen))) hit <- hit |> filter(is_nxtgen)
  hit |> arrange(desc(is_nxtgen), created) |> slice(1)
}

rows <- map_dfr(seq_len(nrow(sheets)), function(i) {
  h <- pick_hub(sheets$nombre_key[i], sheets$nombre_raw_key[i])
  k <- sheets$nombre_key[i]
  bng <- bub_ng |> filter(nombre_key == k)
  if (!nrow(bng)) bng <- bub_ng |> filter(nombre_key == sheets$nombre_raw_key[i])
  ball <- bub_all |> filter(nombre_key == k)
  if (!nrow(ball)) ball <- bub_all |> filter(nombre_key == sheets$nombre_raw_key[i])
  hm <- if (nrow(h)) {
    hub_meets |> filter(amb_contact_id == h$amb_id[[1]])
  } else {
    hub_meets[0, ]
  }

  tibble(
    nombre = sheets$nombre[i],
    fecha = sheets$fecha[i],
    gen = sheets$gen[i],
    matched_hub = nrow(h) > 0,
    hub_type = if (nrow(h)) h$type_os[[1]] else NA_character_,
    hub_created = if (nrow(h)) h$created[[1]] else as.Date(NA),
    hub_modified = if (nrow(h)) h$modified[[1]] else as.Date(NA),
    hub_last_meeting = if (nrow(h)) h$last_meeting[[1]] else as.Date(NA),
    hub_first_meet = if (nrow(hm)) hm$first_meet_hub[[1]] else as.Date(NA),
    hub_first_realizada = if (nrow(hm)) hm$first_realizada_hub[[1]] else as.Date(NA),
    bub_first_cita_nxt = if (nrow(bng)) bng$first_cita[[1]] else as.Date(NA),
    bub_first_created_meet_nxt = if (nrow(bng)) bng$first_created_meet[[1]] else as.Date(NA),
    bub_first_primera_nxt = if (nrow(bng)) bng$first_primera_cita[[1]] else as.Date(NA),
    bub_first_cita_any = if (nrow(ball)) ball$first_cita[[1]] else as.Date(NA),
    sheets_1ra = sheets$fecha_1ra_sheets[i]
  )
})

candidates <- list(
  "amb.Created.Date" = rows$hub_created,
  "amb.Modified.Date" = rows$hub_modified,
  "amb.Last.Meeting.Date" = rows$hub_last_meeting,
  "amb.first Meeting.Start" = rows$hub_first_meet,
  "amb.first Meeting realizada" = rows$hub_first_realizada,
  "bubble.first Start.date (NxtGen)" = rows$bub_first_cita_nxt,
  "bubble.first meeting Created.Date (NxtGen)" = rows$bub_first_created_meet_nxt,
  "bubble.first First.Meeting=true (NxtGen)" = rows$bub_first_primera_nxt,
  "bubble.first Start.date (any origin)" = rows$bub_first_cita_any,
  "sheets.Fecha 1ra cita (control)" = rows$sheets_1ra
)

n_fecha <- sum(!is.na(rows$fecha))
n_gen <- sum(!is.na(rows$gen))

score <- map_dfr(names(candidates), function(nm) {
  d <- candidates[[nm]]
  both <- !is.na(rows$fecha) & !is.na(d)
  tibble(
    campo = nm,
    n_non_na = sum(!is.na(d)),
    day_hits = sum(eq_day(rows$fecha, d)),
    day_pct_all = round(100 * sum(eq_day(rows$fecha, d)) / n_fecha, 1),
    day_pct_paired = round(100 * sum(eq_day(rows$fecha, d)) / max(1, sum(both)), 1),
    month_hits = sum(eq_ym(rows$gen, ym(d))),
    month_pct_all = round(100 * sum(eq_ym(rows$gen, ym(d))) / n_gen, 1),
    month_pct_paired = round(100 * sum(eq_ym(rows$gen, ym(d))) / max(1, sum(!is.na(rows$gen) & !is.na(d))), 1),
    median_abs_delta_days = suppressWarnings(median(abs(as.numeric(rows$fecha - d)), na.rm = TRUE))
  )
}) |>
  arrange(desc(month_hits), desc(day_hits))

cat("Sheets n=", nrow(sheets), " Fecha=", n_fecha, " hub_match=", sum(rows$matched_hub), "\n\n")
print(as.data.frame(score), row.names = FALSE)

cat("\n--- Proximidad a amb.Created.Date ---\n")
delta <- abs(as.numeric(rows$fecha - rows$hub_created))
cat("exact day:", sum(delta == 0, na.rm = TRUE), "\n")
cat("±1 day:", sum(delta <= 1, na.rm = TRUE), "\n")
cat("±3 day:", sum(delta <= 3, na.rm = TRUE), "\n")
cat("±7 day:", sum(delta <= 7, na.rm = TRUE), "\n")
cat("paired n:", sum(!is.na(delta)), "\n")

rows_amb <- rows |>
  filter(!is.na(hub_type), origenes_normalize_key(hub_type) == "ambassadornxtgen")
cat("\nSolo Type_OS=Ambassador NxtGen (n=", nrow(rows_amb), "):\n", sep = "")
cat(
  "Created month hits:", sum(eq_ym(rows_amb$gen, ym(rows_amb$hub_created))),
  "/", nrow(rows_amb),
  " (", round(100 * mean(eq_ym(rows_amb$gen, ym(rows_amb$hub_created))), 1), "%)\n",
  sep = ""
)
cat(
  "Created day hits:", sum(eq_day(rows_amb$fecha, rows_amb$hub_created)),
  "/", nrow(rows_amb), "\n",
  sep = ""
)
