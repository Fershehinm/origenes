#!/usr/bin/env Rscript
# Audita Referrer_R en user Ambassadors (quién refiere / vacíos).
# Uso: Rscript scripts/audit_user_referrer.R

readRenviron(".Renviron")
source("R/bubble_brokers.R")
source("R/ambassadors_api.R")
source("data_nxtgen.R")

`%||%` <- function(a, b) if (!is.null(a) && length(a)) a else b

fold <- function(x) origenes_normalize_key(x)

user <- ambassadors_fetch_endpoint("user")
if (!nrow(user)) {
  stop("user vacío: revisa AMBASSADORS_* o Privacy del type User", call. = FALSE)
}

nm <- intersect(
  c("Full.Name", "Name", "first_name", "First.Name", "email", "Email"),
  names(user)
)[1]
if (is.na(nm)) {
  stop("Sin columna de nombre. cols: ", paste(names(user), collapse = ", "), call. = FALSE)
}

lookup <- stats::setNames(as.character(user[[nm]]), as.character(user$X_id))
ref_id <- as.character(user$Referrer_R %||% user$Referrer_r %||% NA)
user$referrer_nombre <- unname(lookup[ref_id])

created_col <- intersect(c("Created.Date", "Created_Date", "created_date"), names(user))[1]
created <- if (!is.na(created_col)) {
  as.Date(substr(as.character(user[[created_col]]), 1L, 10L))
} else {
  as.Date(rep(NA, nrow(user)))
}
yr <- !is.na(created) & created >= as.Date("2026-01-01")

want <- list(
  arturo = fold(c("Arturo Cole")),
  salazar = fold(c(
    "Eduardo Salazar",
    "Eduardo Javier Salazar Perez",
    "Eduardo Salazar Yemi Alfie"
  )),
  otros = fold(c(
    "Otros", "Marketing",
    "Mauricio Sanchez", "Mau Sanchez",
    "Moi Salame", "Diego Espinosa"
  ))
)
ref_f <- fold(user$referrer_nombre)
is_a <- !is.na(ref_f) & ref_f %in% want$arturo
is_s <- !is.na(ref_f) & ref_f %in% want$salazar
is_o <- !is.na(ref_f) & ref_f %in% want$otros
empty <- yr & (is.na(ref_id) | !nzchar(ref_id) | ref_id %in% c("NA", "NULL"))
rest <- yr & !is_a & !is_s & !is_o & !empty

message("users 2026: ", sum(yr))
message("referrer Arturo: ", sum(yr & is_a))
message("referrer Salazar: ", sum(yr & is_s))
message("referrer Otros (Mau/Moi/Diego/Mkt): ", sum(yr & is_o))
message("referrer vacío: ", sum(empty))
message("referrer OTRO nombre (resto): ", sum(rest))
message("\nNombres en el resto:")
print(sort(table(user$referrer_nombre[rest], useNA = "ifany"), decreasing = TRUE))
