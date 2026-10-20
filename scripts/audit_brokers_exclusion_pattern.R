readRenviron(".Renviron")
Sys.setenv(ORIGENES_APP_ROOT = ".")
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})
source("data_nxtgen.R")
source("data_brokers.R")
source("R/bubble_brokers.R")
source("R/ambassadors_api.R")
source("R/origenes_names.R")
source("R/origenes_join.R")

contact <- readRDS("data/ambassadors/explore/contact.rds")
std <- origenes_standardize_amb_contacts(contact)
raw <- contact |>
  transmute(
    amb_contact_id = as.character(.data$X_id),
    created_by = as.character(.data$Created.By),
    email = as.character(.data$Email),
    phone = as.character(.data$Phone),
    status_os = as.character(.data$Status_OS),
    origin_os = as.character(.data$Origin_OS),
    owner_r = as.character(.data$Owner_R),
    prospecting = as.character(.data$Prospecting.Method_OS),
    background = as.character(.data$Background),
    first_meeting = as.character(.data$First.Meeting),
    rscg_lead = as.character(.data$RSCG.Lead.ID),
    last_meeting = as.character(.data$Last.Meeting.Date),
    challenge = as.character(.data$Origin.Challenge_R),
    team = as.character(.data$Origin.Team_R)
  )

brokers <- std |>
  filter(.data$is_broker_ambassador) |>
  left_join(raw, by = "amb_contact_id")

sheets <- read.csv(
  "data/brokers/raw/Brokers Academy Analytics - Registros de embajadores.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)
name_map <- read.csv("data/brokers/canonical/name_map.csv", stringsAsFactors = FALSE)

fold <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  gsub("[^a-z0-9]+", "", x)
}
alias_from <- c(fold(name_map$nombre_key), fold(name_map$embajador))
alias_to <- c(fold(name_map$embajador_key), fold(name_map$embajador_key))
alias <- stats::setNames(alias_to, alias_from)
canon <- function(x) {
  f <- fold(x)
  hit <- unname(alias[f])
  ifelse(!is.na(hit), hit, f)
}

sheet_canon <- unique(c(canon(sheets[["Nombre"]]), canon(sheets[["Conciliacion Nombre"]])))
sheet_canon <- sheet_canon[!is.na(sheet_canon) & nzchar(sheet_canon)]

nz <- function(x) {
  x <- trimws(as.character(x))
  !is.na(x) & nzchar(x) & x != "NA" & x != "null"
}

brokers <- brokers |>
  mutate(
    f_created = as.Date(.data$fecha_registro),
    en_sheets = canon(.data$nombre) %in% sheet_canon,
    mes = format(.data$f_created, "%Y-%m")
  )

win <- brokers |> filter(.data$f_created <= as.Date("2026-06-30"))
fuera <- win |> filter(!.data$en_sheets)
dentro <- win |> filter(.data$en_sheets)

message("ventana dic-jun brokers ", nrow(win),
        " en sheets ", nrow(dentro),
        " fuera ", nrow(fuera))

message("=== columnas ===")
message(paste(names(brokers), collapse = " | "))
message("=== FUERA ===")
print(fuera |>
  select(any_of(c(
    "nombre", "f_created", "created_by", "status_os", "origin_os", "owner_r",
    "prospecting", "team", "challenge", "email", "phone", "rscg_lead",
    "first_meeting", "last_meeting", "background"
  ))))

fields <- c(
  "created_by", "status_os", "origin_os", "owner_r", "prospecting", "team",
  "challenge", "email", "phone", "rscg_lead", "first_meeting", "last_meeting",
  "background"
)
message("=== llenado ===")
print(bind_rows(lapply(fields, function(f) {
  tibble(
    campo = f,
    fuera = paste0(sum(nz(fuera[[f]])), "/", nrow(fuera)),
    dentro = paste0(sum(nz(dentro[[f]])), "/", nrow(dentro))
  )
})))

message("=== created_by fuera ===")
print(count(fuera, .data$created_by, sort = TRUE))
message("=== created_by dentro ===")
print(count(dentro, .data$created_by, sort = TRUE))
message("=== campos estandar .x ===")
print(fuera |>
  transmute(
    nombre,
    f_created,
    created_by,
    email = .data$email.x,
    phone,
    status = .data$status_os.x,
    origin = .data$origin_os.x,
    prospecting_method,
    first_meeting = .data$first_meeting.x,
    matched_rscg_lead,
    team_id = .data$origin_team_id,
    challenge_id = .data$origin_challenge_id,
    background,
    rscg_lead
  ))

message("=== llenado real .x ===")
std_fields <- c(
  "email.x", "status_os.x", "origin_os.x", "prospecting_method",
  "origin_team_id", "origin_challenge_id", "phone", "background", "owner_r"
)
print(bind_rows(lapply(std_fields, function(f) {
  tibble(
    campo = f,
    fuera = paste0(sum(nz(fuera[[f]]) | (f == "first_meeting.x" & fuera[[f]] %in% TRUE)), "/", nrow(fuera)),
    dentro = paste0(sum(nz(dentro[[f]])), "/", nrow(dentro))
  )
})))
message("first_meeting fuera ", sum(fuera$first_meeting.x, na.rm = TRUE),
        " dentro ", sum(dentro$first_meeting.x, na.rm = TRUE))
message("matched_rscg fuera ", sum(fuera$matched_rscg_lead, na.rm = TRUE),
        " dentro ", sum(dentro$matched_rscg_lead, na.rm = TRUE))

message("=== quien tiene telefono y SI esta en sheets ===")
print(dentro |> filter(nz(.data$phone)) |> select("nombre", "f_created", "phone", "created_by", "email.x"))

message("=== admin_user: en sheets vs no, por mes ===")
print(win |>
  filter(.data$created_by == "admin_user_ambassadors-hub_live") |>
  count(.data$mes, .data$en_sheets))

message("=== 4 dic por created_by ===")
dic4 <- win |> filter(.data$f_created == as.Date("2025-12-04"))
print(dic4 |> count(.data$created_by, .data$en_sheets))
message("4 dic FUERA")
print(dic4 |> filter(!.data$en_sheets) |> select("nombre", "email.x", "phone", "created_by"))
message("4 dic DENTRO, mismos campos vacios?")
print(dic4 |>
  filter(.data$en_sheets) |>
  mutate(
    tiene_email = nz(.data$email.x),
    tiene_phone = nz(.data$phone),
    tiene_team = nz(.data$origin_team_id),
    tiene_rscg = .data$matched_rscg_lead
  ) |>
  count(.data$created_by, .data$tiene_email, .data$tiene_phone, .data$tiene_team, .data$tiene_rscg))

# mismos created_by del 4 dic: estan los fuera mezclados con gente que si entro?
dic4 <- win |> filter(.data$f_created == as.Date("2025-12-04"))
message("=== cohorte 2025-12-04 ===")
message("n ", nrow(dic4), " fuera ", sum(!dic4$en_sheets))
print(dic4 |> count(.data$en_sheets, .data$created_by))
print(dic4 |> filter(!.data$en_sheets) |> select("nombre", "created_by", "email", "phone", "owner_r", "background", "rscg_lead"))
print(dic4 |> filter(.data$en_sheets) |> count(.data$created_by))

message("=== emails fuera ===")
print(fuera |> select("nombre", "email", "phone", "created_by", "owner_r", "background", "rscg_lead", "first_meeting"))
