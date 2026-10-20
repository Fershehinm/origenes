# Roster editable Brokers: Nombre (app), Conciliación (CRM), Canal (manual).
# Overrides persistentes en data/brokers/canonical/embajadores_overrides.csv

origenes_brokers_overrides_path <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  file.path(root, "data", "brokers", "canonical", "embajadores_overrides.csv")
}

#' Catálogo fijo de Canal (altas nuevas: pedirlas y agregar aquí).
origenes_brokers_canal_choices <- function() {
  c(
    "Diego Pimienta",
    "Alejandro Cervantes",
    "Arturo Cole",
    "Mauricio Sanchez",
    "Darwin Angulo",
    "Eduardo Salazar",
    "Jose Manuel Duran",
    "Diego Espinosa",
    "Renata Pereyra",
    "Yemi Alfie",
    "Rodrigo Espriu",
    "Marketing"
  )
}

#' Claves de embajadores que pertenecen a un equipo en el hub (líderes).
origenes_brokers_hub_team_keys <- function(origin_obj) {
  teams <- origin_obj$teams %||% tibble::tibble()
  if (!nrow(teams)) {
    return(character())
  }
  names_lead <- character()
  if ("Leader.Full.Name" %in% names(teams)) {
    names_lead <- as.character(teams[["Leader.Full.Name"]])
  }
  keys <- brokers_norm_name(names_lead)
  unique(keys[!is.na(keys) & nzchar(keys)])
}

origenes_brokers_overrides_empty <- function() {
  tibble::tibble(
    nombre_key = character(),
    amb_contact_id = character(),
    nombre_app = character(),
    conciliacion = character(),
    canal = character(),
    pertenece_equipo = logical(),
    pertenece_manual = logical(),
    generacion = character(),
    fecha_graduacion = character(),
    gen_registro = character(),
    excluido = logical(),
    updated_at = character()
  )
}

#' Gen de graduación que marca En capacitación. El Sheets histórico llega a Gen 5.
origenes_brokers_generacion_choices <- function(extra = character()) {
  base <- paste("Gen", seq_len(5L))
  extra <- stringr::str_squish(as.character(extra))
  extra <- unique(extra[!is.na(extra) & nzchar(extra) & !extra %in% base])
  vals <- c(base, extra)
  c("(sin gen de graduación)" = "", stats::setNames(vals, vals))
}

origenes_brokers_canal_label <- function(x) {
  x <- stringr::str_squish(as.character(x %||% ""))
  choices <- origenes_brokers_canal_choices()
  fold <- function(z) tolower(stringr::str_squish(as.character(z)))
  hit <- match(fold(x), fold(choices))
  out <- ifelse(!is.na(hit), choices[hit], x)
  dplyr::na_if(out, "")
}

#' Canal del Sheets, una vez por persona. El override del dash gana si ya trae canal.
origenes_brokers_sheets_canal <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  path <- origenes_brokers_embajadores_sheet_path(root)
  empty <- stats::setNames(character(), character())
  if (!file.exists(path)) {
    return(empty)
  }
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  if (!nrow(raw) || !"Nombre" %in% names(raw) || !"Canal" %in% names(raw)) {
    return(empty)
  }
  alias <- origenes_brokers_fold_alias(root)
  nombre <- stringr::str_squish(as.character(raw[["Nombre"]]))
  conc <- if ("Conciliacion Nombre" %in% names(raw)) {
    stringr::str_squish(as.character(raw[["Conciliacion Nombre"]]))
  } else {
    rep("", length(nombre))
  }
  canal <- origenes_brokers_canal_label(raw[["Canal"]])
  key <- origenes_brokers_canon_key(dplyr::coalesce(dplyr::na_if(nombre, ""), conc), alias)
  ok <- !is.na(key) & nzchar(key) & !is.na(canal) & nzchar(canal)
  if (!any(ok)) {
    return(empty)
  }
  stats::setNames(canal[ok], key[ok])[!duplicated(key[ok])]
}

#' Quien ya tiene una cita en la app de embajadores (el cruce con el hub).
origenes_brokers_app_cita_keys <- function(origin_obj, alias = NULL) {
  meetings <- origin_obj$meetings %||% tibble::tibble()
  if (!nrow(meetings) || !"join_meeting_hub" %in% names(meetings)) {
    return(character())
  }
  if (is.null(alias)) {
    alias <- origenes_brokers_fold_alias()
  }
  hit <- meetings$join_meeting_hub %in% TRUE
  keys <- origenes_brokers_canon_key(meetings$embajador[hit], alias)
  unique(keys[!is.na(keys) & nzchar(keys)])
}

origenes_brokers_embajadores_sheet_path <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  nuevo <- file.path(root, "data", "brokers", "raw", "Brokers Academy Analytics - Embajadores.csv")
  viejo <- file.path(root, "data", "brokers", "raw", "Brokers Academy Analytics - Registros de embajadores.csv")
  if (file.exists(nuevo)) nuevo else viejo
}

origenes_brokers_load_overrides <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  path <- origenes_brokers_overrides_path(root)
  if (!file.exists(path)) {
    return(origenes_brokers_overrides_empty())
  }
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  if (!nrow(raw)) {
    return(origenes_brokers_overrides_empty())
  }
  pertenece <- raw$pertenece_equipo
  if (is.character(pertenece) || is.factor(pertenece)) {
    key <- origenes_normalize_key(pertenece)
    pertenece <- ifelse(
      key %in% c("", "na", "nan"),
      NA,
      key %in% c("si", "true", "1", "yes")
    )
  } else {
    pertenece <- as.logical(pertenece)
  }
  excluido <- raw$excluido
  if (is.null(excluido)) {
    excluido <- rep(FALSE, nrow(raw))
  } else if (is.character(excluido) || is.factor(excluido)) {
    excluido <- origenes_normalize_key(excluido) %in% c("si", "true", "1", "yes")
  } else {
    excluido <- dplyr::coalesce(as.logical(excluido), FALSE)
  }
  pertenece_manual <- raw$pertenece_manual
  if (is.null(pertenece_manual)) {
    # CSV viejo: FALSE sembrado no cuenta como manual
    pertenece_manual <- rep(FALSE, length(pertenece))
  } else if (is.character(pertenece_manual) || is.factor(pertenece_manual)) {
    pertenece_manual <- origenes_normalize_key(pertenece_manual) %in% c("si", "true", "1", "yes")
  } else {
    pertenece_manual <- dplyr::coalesce(as.logical(pertenece_manual), FALSE)
  }
  tibble::tibble(
    nombre_key = brokers_norm_name(raw$nombre_key %||% raw$nombre_app),
    amb_contact_id = as.character(raw$amb_contact_id %||% NA_character_),
    nombre_app = stringr::str_squish(as.character(raw$nombre_app %||% "")),
    conciliacion = stringr::str_squish(as.character(raw$conciliacion %||% "")),
    canal = stringr::str_squish(as.character(raw$canal %||% "")),
    pertenece_equipo = pertenece,
    pertenece_manual = pertenece_manual,
    generacion = stringr::str_squish(as.character(raw$generacion %||% "")),
    fecha_graduacion = {
      raw_fg <- as.character(raw$fecha_graduacion %||% "")
      parsed <- origenes_brokers_parse_sheet_fecha(raw_fg)
      iso <- suppressWarnings(as.Date(raw_fg))
      d <- dplyr::coalesce(parsed, iso)
      ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
    },
    gen_registro = stringr::str_squish(as.character(raw$gen_registro %||% "")),
    excluido = excluido,
    updated_at = as.character(raw$updated_at %||% NA_character_)
  ) |>
    dplyr::filter(!is.na(.data$nombre_key), nzchar(.data$nombre_key)) |>
    dplyr::distinct(.data$nombre_key, .keep_all = TRUE)
}

origenes_brokers_save_overrides <- function(overrides,
                                           root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  path <- origenes_brokers_overrides_path(root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  empty <- origenes_brokers_overrides_empty()
  if (is.null(overrides) || !nrow(overrides)) {
    utils::write.csv(empty, path, row.names = FALSE, fileEncoding = "UTF-8")
    return(invisible(path))
  }
  out <- overrides |>
    dplyr::mutate(
      nombre_key = brokers_norm_name(.data$nombre_key %||% .data$nombre_app),
      pertenece_equipo = as.logical(.data$pertenece_equipo),
      pertenece_manual = dplyr::coalesce(as.logical(.data$pertenece_manual), FALSE),
      conciliacion = dplyr::na_if(stringr::str_squish(as.character(.data$conciliacion)), ""),
      canal = dplyr::na_if(stringr::str_squish(as.character(.data$canal)), ""),
      generacion = dplyr::na_if(stringr::str_squish(as.character(.data$generacion)), ""),
      fecha_graduacion = {
        raw_fg <- stringr::str_squish(as.character(.data$fecha_graduacion %||% ""))
        parsed <- origenes_brokers_parse_sheet_fecha(raw_fg)
        iso <- suppressWarnings(as.Date(raw_fg))
        d <- dplyr::coalesce(parsed, iso)
        ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
      },
      gen_registro = dplyr::na_if(stringr::str_squish(as.character(.data$gen_registro)), ""),
      excluido = dplyr::coalesce(as.logical(.data$excluido), FALSE),
      updated_at = dplyr::coalesce(
        as.character(.data$updated_at),
        format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
      )
    ) |>
    dplyr::filter(!is.na(.data$nombre_key), nzchar(.data$nombre_key)) |>
    dplyr::distinct(.data$nombre_key, .keep_all = TRUE) |>
    dplyr::select(dplyr::any_of(names(empty)))
  utils::write.csv(out, path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(path)
}

#' Opciones de conciliación: embajador del CRM solo en citas que ya existen en la app.
origenes_brokers_conciliacion_choices <- function(origin_obj) {
  meetings <- origin_obj$meetings %||% tibble::tibble()
  if (!nrow(meetings)) {
    return(character())
  }
  if ("join_meeting_hub" %in% names(meetings)) {
    meetings <- meetings[meetings$join_meeting_hub %in% TRUE, , drop = FALSE]
  }
  cols <- c("embajador_bubble", "embajador")
  vals <- character()
  for (col in cols) {
    if (col %in% names(meetings)) {
      vals <- c(vals, as.character(meetings[[col]]))
    }
  }
  vals <- sort(unique(stringr::str_squish(vals)))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  vals
}

#' Select HTML por fila (conciliación / canal) → input brokers_roster_cell_edit.
origenes_brokers_select_cell <- function(key, field, value, choices, allow_empty = TRUE) {
  key <- as.character(key %||% "")
  value <- as.character(value %||% "")
  opts <- list()
  if (isTRUE(allow_empty)) {
    opts[[length(opts) + 1L]] <- htmltools::tags$option(
      value = "",
      selected = !nzchar(value),
      ""
    )
  }
  for (ch in choices) {
    ch <- as.character(ch)
    if (!nzchar(ch)) next
    opts[[length(opts) + 1L]] <- htmltools::tags$option(
      value = ch,
      selected = identical(value, ch),
      ch
    )
  }
  htmltools::tags$select(
    class = "or-roster-select",
    `data-key` = key,
    `data-field` = field,
    onchange = paste0(
      "Shiny.setInputValue('brokers_roster_cell_edit',",
      "{key:this.getAttribute('data-key'),",
      "field:this.getAttribute('data-field'),",
      "value:this.value,nonce:Date.now()},{priority:'event'})"
    ),
    opts
  )
}

#' Input de texto por fila (fecha / generación / conciliación / canal).
#' Devuelve HTML string (más barato que tags htmltools × cientos de filas).
origenes_brokers_text_cell <- function(key,
                                       field,
                                       value,
                                       placeholder = "",
                                       list_id = NULL) {
  key <- htmltools::htmlEscape(as.character(key %||% ""), attribute = TRUE)
  field <- htmltools::htmlEscape(as.character(field %||% ""), attribute = TRUE)
  value <- htmltools::htmlEscape(as.character(value %||% ""), attribute = TRUE)
  placeholder <- htmltools::htmlEscape(as.character(placeholder %||% ""), attribute = TRUE)
  list_attr <- if (!is.null(list_id) && nzchar(list_id)) {
    paste0(' list="', htmltools::htmlEscape(list_id, attribute = TRUE), '"')
  } else {
    ""
  }
  paste0(
    '<input type="text" class="or-roster-input" value="', value, '"',
    ' placeholder="', placeholder, '"',
    ' data-key="', key, '" data-field="', field, '"',
    list_attr,
    " onchange=\"Shiny.setInputValue('brokers_roster_cell_edit',",
    "{key:this.getAttribute('data-key'),",
    "field:this.getAttribute('data-field'),",
    "value:this.value,nonce:Date.now()},{priority:'event'})\" />"
  )
}

#' Datalist HTML compartido (una sola vez en la página).
origenes_brokers_datalist_html <- function(id, choices) {
  choices <- unique(as.character(choices))
  choices <- choices[!is.na(choices) & nzchar(stringr::str_squish(choices))]
  opts <- paste0(
    '<option value="',
    htmltools::htmlEscape(choices, attribute = TRUE),
    '"></option>',
    collapse = ""
  )
  paste0('<datalist id="', htmltools::htmlEscape(id, attribute = TRUE), '">', opts, "</datalist>")
}

#' Tabla Embajadores alineada al Sheets (una fila por persona registrada).
origenes_brokers_embajadores_tabla <- function(origin_obj,
                                              roster,
                                              as_of = Sys.Date(),
                                              root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  empty <- tibble::tibble(
    nombre_key = character(),
    .fecha_reg = as.Date(character()),
    `Fecha de registro` = character(),
    Nombre = character(),
    `Pertenece a un equipo` = character(),
    Role = character(),
    `Conciliacion Nombre` = character(),
    Canal = character(),
    `Fecha graduación` = character(),
    Generación = character(),
    `Fecha primera cita realizada` = character(),
    `Fecha de última cita realizada` = character(),
    `Gen Registro` = character(),
    `Días desde la última cita realizadas` = integer(),
    `Fecha de última cita agendada/realizada` = character(),
    `Gen primera cita realizada` = character(),
    `Ciclo Registro / Primera Cita` = integer()
  )
  if (is.null(roster) || !nrow(roster)) {
    return(empty)
  }
  as_of <- as.Date(as_of)
  alias <- origenes_brokers_fold_alias(root)
  team_keys <- origenes_brokers_hub_team_keys(origin_obj)
  meetings <- origin_obj$meetings %||% tibble::tibble()

  act <- tibble::tibble(
    .canon = character(),
    fecha_primera_cita = as.Date(character()),
    fecha_ultima_realizada = as.Date(character()),
    fecha_ultima_cita = as.Date(character())
  )
  if (nrow(meetings) && "embajador" %in% names(meetings)) {
    m <- meetings |>
      dplyr::filter(!is.na(.data$fecha), !is.na(.data$embajador), nzchar(.data$embajador)) |>
      dplyr::mutate(
        fecha = as.Date(.data$fecha),
        realizada = origenes_meeting_is_realizada(.data$status_os),
        es_primera = dplyr::coalesce(as.logical(.data$first_meeting), FALSE),
        .canon = origenes_brokers_canon_key(.data$embajador, alias)
      )
    if (nrow(m)) {
      act <- m |>
        dplyr::group_by(.data$.canon) |>
        dplyr::summarise(
          fecha_ultima_cita = as.Date(max(.data$fecha, na.rm = TRUE)),
          fecha_primera_cita = {
            fechas <- .data$fecha[.data$realizada & .data$es_primera]
            if (length(fechas)) as.Date(min(fechas)) else as.Date(NA)
          },
          fecha_ultima_realizada = {
            fechas <- .data$fecha[.data$realizada]
            if (length(fechas)) as.Date(max(fechas)) else as.Date(NA)
          },
          .groups = "drop"
        )
    }
  }

  grad <- origenes_brokers_graduacion_map(root)

  cohorte <- tibble::tibble(
    nombre = roster$nombre_app,
    nombre_raw = roster$nombre_app,
    fecha_registro = roster$fecha_registro
  ) |>
    origenes_brokers_assign_fechas_cohorte(root = root)

  match_key <- origenes_brokers_canon_key(
    dplyr::coalesce(
      dplyr::na_if(stringr::str_squish(as.character(roster$conciliacion)), ""),
      stringr::str_squish(as.character(roster$nombre_app))
    ),
    alias
  )

  primera <- act$fecha_primera_cita[match(match_key, act$.canon)]
  ultima_real <- act$fecha_ultima_realizada[match(match_key, act$.canon)]
  ultima_any <- act$fecha_ultima_cita[match(match_key, act$.canon)]
  fecha_grad_ov <- if ("fecha_graduacion" %in% names(roster)) {
    suppressWarnings(as.Date(roster$fecha_graduacion))
  } else {
    as.Date(rep(NA, nrow(roster)))
  }
  fecha_grad_sheet <- if (nrow(grad)) {
    dplyr::coalesce(
      grad$fecha_graduacion[match(brokers_norm_name(roster$nombre_app), grad$embajador_key)],
      grad$fecha_graduacion[match(brokers_norm_name(roster$conciliacion), grad$embajador_key)]
    )
  } else {
    as.Date(rep(NA, nrow(roster)))
  }
  fecha_grad <- dplyr::coalesce(fecha_grad_ov, fecha_grad_sheet)

  fechas_c <- cohorte$fechas_cohorte
  gen_reg <- vapply(fechas_c, function(s) {
    parts <- strsplit(as.character(s %||% ""), "|", fixed = TRUE)[[1]]
    parts <- parts[!is.na(parts) & nzchar(parts)]
    if (!length(parts)) {
      return(NA_character_)
    }
    origenes_ym_label(min(as.Date(parts)))
  }, character(1))

  fecha_reg <- as.Date(roster$fecha_registro)
  ciclo <- as.integer(primera - fecha_reg)
  ciclo[!is.finite(ciclo)] <- NA_integer_
  dias <- as.integer(as_of - ultima_real)
  dias[!is.finite(dias)] <- NA_integer_

  es_lider <- roster$nombre_key %in% team_keys |
    brokers_norm_name(roster$conciliacion) %in% team_keys

  tibble::tibble(
    nombre_key = roster$nombre_key,
    .fecha_reg = as.Date(fecha_reg),
    `Fecha de registro` = origenes_fmt_date_sheets(fecha_reg),
    Nombre = as.character(roster$nombre_app),
    `Pertenece a un equipo` = dplyr::if_else(
      dplyr::coalesce(as.logical(roster$pertenece_equipo), FALSE),
      "si",
      ""
    ),
    Role = dplyr::if_else(es_lider, "Team Leader", ""),
    `Conciliacion Nombre` = dplyr::coalesce(
      dplyr::na_if(stringr::str_squish(as.character(roster$conciliacion)), ""),
      ""
    ),
    Canal = dplyr::coalesce(dplyr::na_if(stringr::str_squish(as.character(roster$canal)), ""), ""),
    `Fecha graduación` = origenes_fmt_date_sheets(fecha_grad),
    Generación = dplyr::coalesce(dplyr::na_if(stringr::str_squish(as.character(roster$generacion)), ""), ""),
    `Fecha primera cita realizada` = origenes_fmt_date_sheets(primera),
    `Fecha de última cita realizada` = origenes_fmt_date_sheets(ultima_real),
    `Gen Registro` = dplyr::coalesce(gen_reg, ""),
    `Días desde la última cita realizadas` = dias,
    `Fecha de última cita agendada/realizada` = origenes_fmt_date_sheets(ultima_any),
    `Gen primera cita realizada` = dplyr::coalesce(origenes_ym_label(primera), ""),
    `Ciclo Registro / Primera Cita` = ciclo
  ) |>
    dplyr::arrange(
      dplyr::desc(.data$.fecha_reg),
      .data$Nombre,
      .data$nombre_key
    )
}

#' Siembra overrides desde el CSV del Sheets (Conciliación / Canal / Gen / Grad).
origenes_brokers_seed_overrides_from_sheets <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  path <- origenes_brokers_embajadores_sheet_path(root)
  if (!file.exists(path)) {
    return(origenes_brokers_overrides_empty())
  }
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  if (!nrow(raw) || !"Nombre" %in% names(raw)) {
    return(origenes_brokers_overrides_empty())
  }
  nombre <- stringr::str_squish(as.character(raw[["Nombre"]]))
  conc <- if ("Conciliacion Nombre" %in% names(raw)) {
    stringr::str_squish(as.character(raw[["Conciliacion Nombre"]]))
  } else {
    rep("", length(nombre))
  }
  canal <- if ("Canal" %in% names(raw)) {
    stringr::str_squish(as.character(raw[["Canal"]]))
  } else {
    rep("", length(nombre))
  }
  gen <- if ("Generación" %in% names(raw)) {
    stringr::str_squish(gsub("[()]", "", as.character(raw[["Generación"]])))
  } else {
    rep("", length(nombre))
  }
  fecha_grad <- if ("Fecha graduación" %in% names(raw)) {
    d <- origenes_brokers_parse_sheet_fecha(raw[["Fecha graduación"]])
    ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
  } else {
    rep(NA_character_, length(nombre))
  }
  pertenece_col <- names(raw)[
    origenes_normalize_key(names(raw)) %in% c("perteneceaunequipo", "perteneceequipo")
  ]
  if (length(pertenece_col)) {
    key <- origenes_normalize_key(raw[[pertenece_col[[1]]]])
    pertenece <- ifelse(key %in% c("", "na"), NA, key %in% c("si", "true", "1", "yes"))
  } else {
    pertenece <- rep(NA, length(nombre))
  }
  role <- if ("Role" %in% names(raw)) {
    origenes_normalize_key(raw[["Role"]])
  } else {
    rep("", length(nombre))
  }
  # Team Leader del Sheets cuenta como pertenece a equipo.
  pertenece <- dplyr::coalesce(pertenece, role %in% c("teamleader", "team leader"))
  pertenece_manual <- !is.na(pertenece)

  first_nonempty <- function(x) {
    x <- stringr::str_squish(as.character(x))
    x <- x[!is.na(x) & nzchar(x)]
    if (!length(x)) NA_character_ else x[[1L]]
  }

  tibble::tibble(
    nombre_app = nombre,
    nombre_key = brokers_norm_name(nombre),
    amb_contact_id = NA_character_,
    conciliacion = dplyr::na_if(conc, ""),
    canal = dplyr::na_if(canal, ""),
    pertenece_equipo = pertenece,
    pertenece_manual = pertenece_manual,
    generacion = dplyr::na_if(gen, ""),
    fecha_graduacion = fecha_grad,
    gen_registro = NA_character_,
    excluido = FALSE,
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ) |>
    dplyr::filter(!is.na(.data$nombre_key), nzchar(.data$nombre_key)) |>
    dplyr::group_by(.data$nombre_key) |>
    dplyr::summarise(
      amb_contact_id = dplyr::first(.data$amb_contact_id),
      nombre_app = dplyr::first(.data$nombre_app),
      conciliacion = first_nonempty(.data$conciliacion),
      canal = first_nonempty(.data$canal),
      pertenece_equipo = {
        vals <- .data$pertenece_equipo[!is.na(.data$pertenece_equipo)]
        if (length(vals)) any(as.logical(vals)) else NA
      },
      pertenece_manual = any(.data$pertenece_manual, na.rm = TRUE),
      generacion = first_nonempty(.data$generacion),
      fecha_graduacion = first_nonempty(.data$fecha_graduacion),
      gen_registro = NA_character_,
      excluido = FALSE,
      updated_at = dplyr::first(.data$updated_at),
      .groups = "drop"
    )
}

#' Completa overrides vacíos con la conciliación histórica del Sheets.
#' El valor ya guardado en el dash (no vacío) gana; el Sheets rellena huecos.
origenes_brokers_merge_overrides_with_sheets <- function(overrides,
                                                         root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  seeded <- origenes_brokers_seed_overrides_from_sheets(root)
  if (!nrow(seeded)) {
    return(if (is.null(overrides)) origenes_brokers_overrides_empty() else overrides)
  }
  if (is.null(overrides) || !nrow(overrides)) {
    return(seeded)
  }
  alias <- origenes_brokers_fold_alias(root)
  first_nonempty <- function(x) {
    x <- stringr::str_squish(as.character(x))
    x <- x[!is.na(x) & nzchar(x)]
    if (!length(x)) NA_character_ else x[[1L]]
  }
  empty <- origenes_brokers_overrides_empty()
  for (nm in names(empty)) {
    if (!nm %in% names(overrides)) {
      overrides[[nm]] <- empty[[nm]][0]
    }
    if (!nm %in% names(seeded)) {
      seeded[[nm]] <- empty[[nm]][0]
    }
  }
  ov <- overrides |>
    dplyr::mutate(person_key = origenes_brokers_canon_key(.data$nombre_key, alias))
  sh <- seeded |>
    dplyr::mutate(person_key = origenes_brokers_canon_key(.data$nombre_key, alias)) |>
    dplyr::group_by(.data$person_key) |>
    dplyr::summarise(
      dplyr::across(
        c(
          "nombre_app", "nombre_key", "amb_contact_id", "conciliacion", "canal",
          "generacion", "fecha_graduacion", "gen_registro", "updated_at"
        ),
        first_nonempty
      ),
      pertenece_equipo = {
        vals <- .data$pertenece_equipo[!is.na(.data$pertenece_equipo)]
        if (length(vals)) any(as.logical(vals)) else NA
      },
      pertenece_manual = any(dplyr::coalesce(as.logical(.data$pertenece_manual), FALSE)),
      excluido = any(dplyr::coalesce(as.logical(.data$excluido), FALSE)),
      .groups = "drop"
    )
  dplyr::full_join(ov, sh, by = "person_key", suffix = c("", "_sh")) |>
    dplyr::mutate(
      nombre_key = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$nombre_key)), ""),
        dplyr::na_if(stringr::str_squish(as.character(.data$nombre_key_sh)), "")
      ),
      nombre_app = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$nombre_app)), ""),
        dplyr::na_if(stringr::str_squish(as.character(.data$nombre_app_sh)), "")
      ),
      amb_contact_id = dplyr::coalesce(.data$amb_contact_id, .data$amb_contact_id_sh),
      # Dash gana si ya trae valor; Sheets rellena vacío (carga histórica).
      conciliacion = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$conciliacion)), ""),
        dplyr::na_if(stringr::str_squish(as.character(.data$conciliacion_sh)), "")
      ),
      canal = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$canal)), ""),
        dplyr::na_if(stringr::str_squish(as.character(.data$canal_sh)), "")
      ),
      generacion = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$generacion)), ""),
        dplyr::na_if(stringr::str_squish(as.character(.data$generacion_sh)), "")
      ),
      fecha_graduacion = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$fecha_graduacion)), ""),
        dplyr::na_if(stringr::str_squish(as.character(.data$fecha_graduacion_sh)), "")
      ),
      gen_registro = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$gen_registro)), ""),
        dplyr::na_if(stringr::str_squish(as.character(.data$gen_registro_sh)), "")
      ),
      pertenece_equipo = dplyr::case_when(
        dplyr::coalesce(as.logical(.data$pertenece_manual), FALSE) ~
          dplyr::coalesce(as.logical(.data$pertenece_equipo), FALSE),
        !is.na(.data$pertenece_equipo) ~ as.logical(.data$pertenece_equipo),
        !is.na(.data$pertenece_equipo_sh) ~ as.logical(.data$pertenece_equipo_sh),
        TRUE ~ NA
      ),
      pertenece_manual = dplyr::coalesce(as.logical(.data$pertenece_manual), FALSE) |
        dplyr::coalesce(as.logical(.data$pertenece_manual_sh), FALSE),
      excluido = dplyr::coalesce(as.logical(.data$excluido), FALSE) |
        dplyr::coalesce(as.logical(.data$excluido_sh), FALSE),
      updated_at = dplyr::coalesce(.data$updated_at, .data$updated_at_sh)
    ) |>
    dplyr::select(dplyr::any_of(names(empty))) |>
    dplyr::filter(!is.na(.data$nombre_key), nzchar(.data$nombre_key)) |>
    dplyr::distinct(.data$nombre_key, .keep_all = TRUE)
}

#' Altas del Sheets que aún no están en el API → historicos + overrides.
origenes_brokers_sync_historicos_from_sheets <- function(api_canons,
                                                         root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  path <- origenes_brokers_embajadores_sheet_path(root)
  empty <- origenes_brokers_load_historicos(root)
  if (!file.exists(path)) {
    return(empty)
  }
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  if (!nrow(raw) || !"Nombre" %in% names(raw)) {
    return(empty)
  }
  alias <- origenes_brokers_fold_alias(root)
  nombre <- stringr::str_squish(as.character(raw[["Nombre"]]))
  fecha <- origenes_brokers_parse_sheet_fecha(raw[["Fecha de registro"]] %||% "")
  canal <- if ("Canal" %in% names(raw)) {
    dplyr::na_if(stringr::str_squish(as.character(raw[["Canal"]])), "")
  } else {
    rep(NA_character_, length(nombre))
  }
  gen <- if ("Generación" %in% names(raw)) {
    dplyr::na_if(stringr::str_squish(gsub("[()]", "", as.character(raw[["Generación"]]))), "")
  } else {
    rep(NA_character_, length(nombre))
  }
  pertenece_col <- names(raw)[
    origenes_normalize_key(names(raw)) %in% c("perteneceaunequipo", "perteneceequipo")
  ]
  if (length(pertenece_col)) {
    key <- origenes_normalize_key(raw[[pertenece_col[[1]]]])
    pertenece <- key %in% c("si", "true", "1", "yes")
  } else {
    pertenece <- rep(FALSE, length(nombre))
  }
  role <- if ("Role" %in% names(raw)) origenes_normalize_key(raw[["Role"]]) else rep("", length(nombre))
  pertenece <- pertenece | role %in% c("teamleader", "team leader")
  sheet <- tibble::tibble(
    nombre = nombre,
    fecha_registro = fecha,
    canal = canal,
    pertenece_equipo = pertenece,
    generacion = gen,
    canon = origenes_brokers_canon_key(nombre, alias)
  ) |>
    dplyr::filter(
      !is.na(.data$nombre),
      nzchar(.data$nombre),
      !is.na(.data$fecha_registro),
      !.data$canon %in% api_canons
    ) |>
    dplyr::arrange(.data$fecha_registro, .data$nombre) |>
    dplyr::distinct(.data$canon, .keep_all = TRUE)

  hist <- empty
  if (nrow(hist)) {
    hist <- hist[!hist$canon %in% sheet$canon, , drop = FALSE]
  }
  dplyr::bind_rows(hist, sheet) |>
    dplyr::arrange(.data$fecha_registro, .data$nombre) |>
    dplyr::distinct(.data$canon, .keep_all = TRUE)
}

origenes_brokers_save_historicos <- function(hist,
                                             root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  path <- origenes_brokers_historicos_path(root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(hist) || !nrow(hist)) {
    utils::write.csv(
      tibble::tibble(
        nombre = character(),
        fecha_registro = character(),
        canal = character(),
        pertenece_equipo = logical(),
        generacion = character(),
        nota = character()
      ),
      path,
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
    return(invisible(path))
  }
  nota_col <- if ("nota" %in% names(hist)) {
    dplyr::coalesce(
      dplyr::na_if(stringr::str_squish(as.character(hist$nota)), ""),
      "Sheets histórico (no en API)"
    )
  } else {
    rep("Sheets histórico (no en API)", nrow(hist))
  }
  out <- hist |>
    dplyr::mutate(
      fecha_registro = format(as.Date(.data$fecha_registro), "%Y-%m-%d"),
      canal = dplyr::coalesce(as.character(.data$canal), ""),
      pertenece_equipo = dplyr::coalesce(as.logical(.data$pertenece_equipo), FALSE),
      generacion = dplyr::coalesce(as.character(.data$generacion), ""),
      nota = nota_col
    ) |>
    dplyr::select("nombre", "fecha_registro", "canal", "pertenece_equipo", "generacion", "nota")
  utils::write.csv(out, path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(path)
}

#' Clave de nombre sin acentos ni espacios, para cruzar Sheets y app.
origenes_brokers_fold_key <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "", x)
  x[is.na(x)] <- ""
  x
}

origenes_brokers_fold_alias <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  path <- file.path(root, "data", "brokers", "canonical", "name_map.csv")
  if (!file.exists(path)) {
    return(stats::setNames(character(), character()))
  }
  map <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!nrow(map)) {
    return(stats::setNames(character(), character()))
  }
  fuente <- if ("fuente_mapa" %in% names(map)) as.character(map$fuente_mapa) else rep("", nrow(map))
  prio <- match(fuente, c("frecuencia", "lista", "conciliacion", "alias_manual"), nomatch = 1L)
  nombre <- origenes_brokers_fold_key(map$nombre_key)
  embajador <- origenes_brokers_fold_key(map$embajador)
  destino <- origenes_brokers_fold_key(map$embajador_key)
  from <- as.vector(rbind(nombre, embajador, destino))
  to <- rep(destino, each = 3L)
  pr <- rep(prio, each = 3L)
  ord <- rep(seq_len(nrow(map)), each = 3L)
  ok <- !is.na(from) & !is.na(to) & nzchar(from) & nzchar(to)
  from <- from[ok]
  to <- to[ok]
  pr <- pr[ok]
  ord <- ord[ok]
  # Mayor prioridad gana. Dentro de la misma fuente, gana la primera fila.
  o <- order(pr, -ord)
  from <- from[o]
  to <- to[o]
  keep <- !duplicated(from, fromLast = TRUE)
  stats::setNames(to[keep], from[keep])
}

#' Una fila por persona canónica, para el filtro de embajador.
origenes_brokers_personas <- function(roster, alias = NULL, con_cita = NULL) {
  empty <- tibble::tibble(key = character(), label = character(), canal = character())
  if (is.null(roster) || !nrow(roster) || !"nombre_key" %in% names(roster)) {
    return(empty)
  }
  if (is.null(alias)) {
    alias <- origenes_brokers_fold_alias()
  }
  key <- origenes_brokers_canon_key(roster$nombre_key, alias)
  label <- as.character(roster$nombre_app %||% roster$nombre_key)
  canal <- as.character(roster$canal %||% rep("", nrow(roster)))
  ok <- !is.na(key) & nzchar(key) & !is.na(label) & nzchar(label)
  out <- tibble::tibble(key = key[ok], label = label[ok], canal = canal[ok]) |>
    dplyr::arrange(.data$label) |>
    dplyr::distinct(.data$key, .keep_all = TRUE)
  if (!is.null(con_cita)) {
    out <- out[out$key %in% con_cita, , drop = FALSE]
  }
  out
}

origenes_brokers_canon_key <- function(x, alias = NULL) {
  f <- origenes_brokers_fold_key(x)
  if (is.null(alias) || !length(alias)) {
    return(f)
  }
  hit <- unname(alias[f])
  ifelse(!is.na(hit), hit, f)
}

#' Nombre visible por clave canónica (alias_manual gana).
origenes_brokers_canon_label <- function(root = Sys.getenv("ORIGENES_APP_ROOT", "."),
                                         alias = NULL) {
  path <- file.path(root, "data", "brokers", "canonical", "name_map.csv")
  if (!file.exists(path)) {
    return(stats::setNames(character(), character()))
  }
  map <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  if (!nrow(map) || !"embajador" %in% names(map)) {
    return(stats::setNames(character(), character()))
  }
  if (is.null(alias)) {
    alias <- origenes_brokers_fold_alias(root)
  }
  fuente <- if ("fuente_mapa" %in% names(map)) as.character(map$fuente_mapa) else rep("", nrow(map))
  ord <- order(match(fuente, c("alias_manual", "conciliacion", "lista", "frecuencia")), na.last = TRUE)
  map <- map[ord, , drop = FALSE]
  canon <- origenes_brokers_canon_key(map$embajador, alias)
  ok <- !is.na(canon) & nzchar(canon) & !is.na(map$embajador) & nzchar(map$embajador)
  canon <- canon[ok]
  lab <- map$embajador[ok]
  stats::setNames(lab[!duplicated(canon)], canon[!duplicated(canon)])
}

#' Una fila por persona. Dos altas con el mismo nombre canónico cuentan una vez,
#' en el mes de la primera fecha de registro.
origenes_brokers_collapse_same_person <- function(emb,
                                                  root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  if (is.null(emb) || !nrow(emb) || !"nombre" %in% names(emb)) {
    return(emb)
  }
  alias <- origenes_brokers_fold_alias(root)
  pretty <- origenes_brokers_canon_label(root, alias)
  id_src <- dplyr::coalesce(
    dplyr::na_if(stringr::str_squish(as.character(emb$nombre_raw %||% NA_character_)), ""),
    stringr::str_squish(as.character(emb$nombre))
  )
  canon <- origenes_brokers_canon_key(id_src, alias)
  blank <- is.na(canon) | !nzchar(canon)
  canon[blank] <- paste0("row-", seq_len(nrow(emb)))[blank]
  emb$.canon <- canon
  emb$.pretty <- unname(pretty[canon])
  emb$.pretty[is.na(emb$.pretty)] <- ""

  emb |>
    dplyr::arrange(as.Date(.data$fecha_registro), .data$amb_contact_id) |>
    dplyr::group_by(.data$.canon) |>
    dplyr::mutate(
      .gen_keep = dplyr::na_if(stringr::str_squish(as.character(.data$generacion)), ""),
      .mes_keep = dplyr::na_if(stringr::str_squish(as.character(.data$gen_registro_meses)), ""),
      .can_keep = dplyr::na_if(stringr::str_squish(as.character(.data$canal)), ""),
      .n = dplyr::n()
    ) |>
    dplyr::mutate(
      nombre = dplyr::if_else(
        .data$.n > 1L & nzchar(dplyr::first(.data$.pretty)),
        dplyr::first(.data$.pretty[nzchar(.data$.pretty)]),
        .data$nombre
      ),
      generacion = dplyr::first(.data$.gen_keep[!is.na(.data$.gen_keep)]),
      gen_registro_meses = paste(unique(unlist(strsplit(
        paste(.data$.mes_keep[!is.na(.data$.mes_keep)], collapse = "|"),
        "|",
        fixed = TRUE
      ))), collapse = "|"),
      canal = dplyr::first(.data$.can_keep[!is.na(.data$.can_keep)]),
      pertenece_equipo = any(dplyr::coalesce(.data$pertenece_equipo, FALSE))
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(nombre_key = brokers_norm_name(.data$nombre)) |>
    dplyr::select(-".canon", -".pretty", -".gen_keep", -".mes_keep", -".can_keep", -".n")
}

origenes_brokers_parse_sheet_fecha <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x[!nzchar(x)] <- NA_character_
  cuatro <- as.Date(x, format = "%d/%m/%Y")
  anio <- suppressWarnings(as.integer(format(cuatro, "%Y")))
  corto <- is.na(cuatro) | (!is.na(anio) & anio < 1000L)
  if (any(corto, na.rm = TRUE)) {
    dos <- as.Date(x, format = "%d/%m/%y")
    cuatro[corto] <- dos[corto]
  }
  cuatro
}

origenes_brokers_historicos_path <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  file.path(root, "data", "brokers", "canonical", "embajadores_historicos.csv")
}

#' Altas que el Sheets tiene y el API de Brokers no.
#' Histórico fijo: al publicar, el alta sale solo de la app de embajadores.
origenes_brokers_load_historicos <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  path <- origenes_brokers_historicos_path(root)
  empty <- tibble::tibble(
    nombre = character(),
    fecha_registro = as.Date(character()),
    canal = character(),
    pertenece_equipo = logical(),
    generacion = character(),
    canon = character()
  )
  if (!file.exists(path)) {
    return(empty)
  }
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  if (!nrow(raw) || !"nombre" %in% names(raw)) {
    return(empty)
  }
  pertenece <- tolower(stringr::str_squish(as.character(raw$pertenece_equipo %||% "")))
  alias <- origenes_brokers_fold_alias(root)
  tibble::tibble(
    nombre = stringr::str_squish(as.character(raw$nombre)),
    fecha_registro = as.Date(raw$fecha_registro),
    canal = dplyr::na_if(stringr::str_squish(as.character(raw$canal %||% "")), ""),
    pertenece_equipo = pertenece %in% c("si", "sí", "true", "1"),
    generacion = dplyr::na_if(stringr::str_squish(as.character(raw$generacion %||% "")), ""),
    canon = origenes_brokers_canon_key(raw$nombre, alias)
  ) |>
    dplyr::filter(!is.na(.data$nombre), nzchar(.data$nombre), !is.na(.data$fecha_registro)) |>
    dplyr::distinct(.data$canon, .keep_all = TRUE)
}

#' Suma al cohorte de Brokers las altas históricas que todavía no vienen del API.
origenes_brokers_append_historicos <- function(emb,
                                               root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  hist <- origenes_brokers_load_historicos(root)
  if (!nrow(hist)) {
    return(emb)
  }
  alias <- origenes_brokers_fold_alias(root)
  have <- character()
  if (!is.null(emb) && nrow(emb)) {
    id_src <- dplyr::coalesce(
      dplyr::na_if(stringr::str_squish(as.character(emb$nombre_raw %||% "")), ""),
      stringr::str_squish(as.character(emb$nombre))
    )
    have <- unique(c(
      origenes_brokers_canon_key(id_src, alias),
      origenes_brokers_canon_key(emb$nombre, alias)
    ))
    have <- have[nzchar(have)]
  }
  hist <- hist[!hist$canon %in% have, , drop = FALSE]
  if (!nrow(hist)) {
    return(emb)
  }
  add <- tibble::tibble(
    amb_contact_id = paste0("historico-", hist$canon),
    nombre = hist$nombre,
    nombre_raw = hist$nombre,
    nombre_key = brokers_norm_name(hist$nombre),
    fecha_registro = hist$fecha_registro,
    generacion = hist$generacion,
    canal = hist$canal,
    pertenece_equipo = hist$pertenece_equipo,
    gen_registro_meses = NA_character_,
    type_os = "Ambassador Broker",
    historico_sheets = TRUE
  )
  if (is.null(emb) || !nrow(emb)) {
    return(add)
  }
  dplyr::bind_rows(emb, add)
}

#' Fechas de registro del Sheets, una por persona y mes.
#' Si el Sheets repite el mismo mes, queda una sola fecha.
origenes_brokers_sheets_fechas_index <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  path <- origenes_brokers_embajadores_sheet_path(root)
  empty <- stats::setNames(character(), character())
  if (!file.exists(path)) {
    return(empty)
  }
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  if (!"Gen Registro" %in% names(raw)) {
    return(empty)
  }
  mes_m <- stringr::str_match(
    stringr::str_squish(as.character(raw[["Gen Registro"]] %||% "")),
    "(\\d{4})\\s*-\\s*(\\d{2})"
  )
  mes <- ifelse(is.na(mes_m[, 2]), "", paste(mes_m[, 2], mes_m[, 3], sep = "-"))
  fecha <- origenes_brokers_parse_sheet_fecha(raw[["Fecha de registro"]] %||% "")
  sin_mes <- !nzchar(mes) & !is.na(fecha)
  mes[sin_mes] <- format(fecha[sin_mes], "%Y-%m")
  alias <- origenes_brokers_fold_alias(root)
  nombre <- origenes_brokers_canon_key(raw[["Nombre"]], alias)
  conc <- if ("Conciliacion Nombre" %in% names(raw)) {
    origenes_brokers_canon_key(raw[["Conciliacion Nombre"]], alias)
  } else {
    rep("", length(nombre))
  }
  long <- tibble::tibble(
    canon = c(nombre, conc),
    mes = c(mes, mes),
    fecha = c(fecha, fecha)
  ) |>
    dplyr::filter(nzchar(.data$canon), nzchar(.data$mes), !is.na(.data$fecha)) |>
    dplyr::group_by(.data$canon, .data$mes) |>
    dplyr::summarise(fecha = min(.data$fecha), .groups = "drop") |>
    dplyr::group_by(.data$canon) |>
    dplyr::summarise(
      fechas = paste(format(sort(unique(.data$fecha)), "%Y-%m-%d"), collapse = "|"),
      .groups = "drop"
    )
  stats::setNames(long$fechas, long$canon)
}

#' Mes de cohorte = Gen Registro del Sheets. Sin fila en el Sheets, la fecha del API.
origenes_brokers_assign_fechas_cohorte <- function(emb,
                                                  root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  if (is.null(emb) || !nrow(emb)) {
    return(emb)
  }
  idx <- origenes_brokers_sheets_fechas_index(root)
  alias <- origenes_brokers_fold_alias(root)
  raw_src <- dplyr::coalesce(
    dplyr::na_if(stringr::str_squish(as.character(emb$nombre_raw %||% "")), ""),
    stringr::str_squish(as.character(emb$nombre))
  )
  k_raw <- origenes_brokers_canon_key(raw_src, alias)
  k_nom <- origenes_brokers_canon_key(emb$nombre, alias)
  desde_raw <- unname(idx[k_raw])
  desde_nom <- unname(idx[k_nom])
  desde_raw[is.na(desde_raw)] <- ""
  desde_nom[is.na(desde_nom)] <- ""
  api <- as.Date(emb$fecha_registro)
  emb$fechas_cohorte <- vapply(seq_along(api), function(i) {
    parts <- c(
      strsplit(desde_raw[i], "|", fixed = TRUE)[[1]],
      strsplit(desde_nom[i], "|", fixed = TRUE)[[1]]
    )
    parts <- unique(parts[!is.na(parts) & nzchar(parts)])
    if (length(parts)) {
      return(paste(sort(parts), collapse = "|"))
    }
    if (is.na(api[i])) "" else format(api[i], "%Y-%m-%d")
  }, character(1))
  emb
}

#' Meses de columna K (Gen Registro) por nombre del Sheets.
#' Una persona puede salir en más de un mes si el Sheets la repite.
origenes_brokers_sheets_gen_registro <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  path <- origenes_brokers_embajadores_sheet_path(root)
  empty <- tibble::tibble(nombre_key = character(), gen_registro_meses = character())
  if (!file.exists(path)) {
    return(empty)
  }
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  if (!"Gen Registro" %in% names(raw)) {
    return(empty)
  }
  gen <- if ("Generación" %in% names(raw)) {
    stringr::str_squish(gsub("[()]", "", as.character(raw[["Generación"]])))
  } else {
    rep("", nrow(raw))
  }
  # COUNTIFS columna H "<>": solo filas con Generación llena.
  gen_ok <- !is.na(gen) & nzchar(gen)
  mes <- stringr::str_match(
    stringr::str_squish(as.character(raw[["Gen Registro"]])),
    "(\\d{4})\\s*-\\s*(\\d{2})"
  )
  mes <- ifelse(is.na(mes[, 2]), NA_character_, paste(mes[, 2], mes[, 3], sep = "-"))
  mes[!gen_ok] <- NA_character_
  alias <- origenes_brokers_fold_alias(root)
  nombre <- origenes_brokers_canon_key(raw[["Nombre"]], alias)
  conc <- if ("Conciliacion Nombre" %in% names(raw)) {
    origenes_brokers_canon_key(raw[["Conciliacion Nombre"]], alias)
  } else {
    rep("", length(nombre))
  }
  tibble::tibble(
    nombre_key = c(nombre, conc),
    gen_registro_mes = c(mes, mes)
  ) |>
    dplyr::filter(nzchar(.data$nombre_key), !is.na(.data$gen_registro_mes)) |>
    dplyr::distinct() |>
    dplyr::group_by(.data$nombre_key) |>
    dplyr::summarise(
      gen_registro_meses = paste(sort(unique(.data$gen_registro_mes)), collapse = "|"),
      .groups = "drop"
    )
}

origenes_brokers_auto_conciliacion <- function(nombre_app, crm_names) {
  n <- length(nombre_app)
  if (!n) {
    return(character())
  }
  if (!length(crm_names)) {
    return(rep(NA_character_, n))
  }
  lista <- tibble::tibble(
    embajador = stringr::str_squish(as.character(crm_names)),
    embajador_key = brokers_norm_name(crm_names)
  ) |>
    dplyr::filter(!is.na(.data$embajador_key), nzchar(.data$embajador_key)) |>
    dplyr::distinct(.data$embajador_key, .keep_all = TRUE)

  keys <- brokers_norm_name(nombre_app)
  out <- rep(NA_character_, n)
  # Match exacto vectorizado; fuzzy solo para los que faltan.
  exact <- match(keys, lista$embajador_key)
  hit <- !is.na(exact)
  out[hit] <- lista$embajador[exact[hit]]
  miss <- which(!hit & !is.na(keys) & nzchar(keys))
  if (length(miss)) {
    out[miss] <- vapply(
      keys[miss],
      origenes_match_key_to_lista,
      character(1),
      lista_keys = lista$embajador_key,
      lista_names = lista$embajador
    )
  }
  out
}

#' Roster completo para la pestaña Embajadores (Brokers).
origenes_brokers_roster_build <- function(origin_obj,
                                          overrides = NULL,
                                          root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  emb <- origin_obj$embajadores %||% tibble::tibble()
  if (!nrow(emb)) {
    return(tibble::tibble(
      amb_contact_id = character(),
      nombre_key = character(),
      nombre_app = character(),
      conciliacion = character(),
      conciliacion_auto = character(),
      canal = character(),
      pertenece_equipo = logical(),
      generacion = character(),
      fecha_registro = as.Date(character()),
      fuente_conciliacion = character()
    ))
  }

  if (is.null(overrides)) {
    overrides <- origenes_brokers_load_overrides(root)
  }
  # Sheets solo si aún no hay overrides (la carga histórica ya vive en el CSV).
  if (!nrow(overrides)) {
    overrides <- origenes_brokers_merge_overrides_with_sheets(overrides, root)
  }

  crm_names <- origenes_brokers_conciliacion_choices(origin_obj)
  team_keys <- origenes_brokers_hub_team_keys(origin_obj)
  alias <- origenes_brokers_fold_alias(root)

  # Nombre = como viene en Ambassadors (nombre_raw). Conciliación = CRM / mapa.
  nombre_app <- dplyr::coalesce(
    dplyr::na_if(stringr::str_squish(as.character(emb$nombre_raw)), ""),
    stringr::str_squish(as.character(emb$nombre))
  )
  nombre_crm <- stringr::str_squish(as.character(emb$nombre %||% ""))
  crm_keys <- brokers_norm_name(crm_names)
  crm_idx <- match(brokers_norm_name(nombre_crm), crm_keys)
  conciliacion_crm <- ifelse(!is.na(crm_idx), crm_names[crm_idx], NA_character_)
  auto <- origenes_brokers_auto_conciliacion(nombre_app, crm_names)
  app_key <- brokers_norm_name(nombre_app)
  # Una persona = mismo nombre CRM / alias (p. ej. Daniel Pascal ≡ Daniel Cobo Pascal).
  person_key <- origenes_brokers_canon_key(
    dplyr::coalesce(dplyr::na_if(nombre_crm, ""), nombre_app),
    alias
  )

  first_nonempty <- function(x) {
    x <- stringr::str_squish(as.character(x))
    x <- x[!is.na(x) & nzchar(x)]
    if (!length(x)) NA_character_ else x[[1L]]
  }

  ov <- overrides |>
    dplyr::select(
      "nombre_key",
      ov_conciliacion = "conciliacion",
      ov_canal = "canal",
      ov_pertenece = "pertenece_equipo",
      ov_pertenece_manual = "pertenece_manual",
      ov_generacion = "generacion",
      ov_fecha_graduacion = "fecha_graduacion",
      ov_gen_registro = "gen_registro",
      ov_excluido = "excluido"
    )
  if (!"ov_gen_registro" %in% names(ov)) {
    ov$ov_gen_registro <- NA_character_
  }
  if (!"ov_fecha_graduacion" %in% names(ov)) {
    ov$ov_fecha_graduacion <- NA_character_
  }
  if (!"ov_pertenece_manual" %in% names(ov)) {
    ov$ov_pertenece_manual <- FALSE
  }
  if (!"ov_excluido" %in% names(ov)) {
    ov$ov_excluido <- FALSE
  }
  excluded_keys <- ov$nombre_key[dplyr::coalesce(as.logical(ov$ov_excluido), FALSE)]

  # Si el Sheets/override usa un nombre_raw del API, pegarlo al person_key CRM.
  raw_to_crm <- stats::setNames(
    origenes_brokers_canon_key(nombre_crm, alias),
    origenes_brokers_canon_key(nombre_app, alias)
  )
  raw_to_crm <- raw_to_crm[
    nzchar(names(raw_to_crm)) &
      nzchar(unname(raw_to_crm)) &
      names(raw_to_crm) != unname(raw_to_crm)
  ]

  # Overrides de aliases del mismo embajador se fusionan (p. ej. daniel pascal + cobo).
  ov <- ov |>
    dplyr::mutate(
      person_key = {
        pk <- origenes_brokers_canon_key(.data$nombre_key, alias)
        mapped <- unname(raw_to_crm[pk])
        ifelse(!is.na(mapped) & nzchar(mapped), mapped, pk)
      }
    ) |>
    dplyr::group_by(.data$person_key) |>
    dplyr::summarise(
      ov_conciliacion = first_nonempty(.data$ov_conciliacion),
      ov_canal = first_nonempty(.data$ov_canal),
      ov_pertenece = {
        v <- as.logical(.data$ov_pertenece)
        v <- v[!is.na(v)]
        if (!length(v)) NA else v[[1L]]
      },
      ov_pertenece_manual = any(
        dplyr::coalesce(as.logical(.data$ov_pertenece_manual), FALSE)
      ),
      ov_generacion = first_nonempty(.data$ov_generacion),
      ov_fecha_graduacion = first_nonempty(.data$ov_fecha_graduacion),
      ov_gen_registro = first_nonempty(.data$ov_gen_registro),
      ov_excluido = any(dplyr::coalesce(as.logical(.data$ov_excluido), FALSE)),
      .groups = "drop"
    )

  canal_sheet <- origenes_brokers_sheets_canal(root)
  grad_sheet <- origenes_brokers_graduacion_map(root)
  roster <- tibble::tibble(
    amb_contact_id = as.character(emb$amb_contact_id %||% NA_character_),
    person_key = person_key,
    nombre_key = app_key,
    nombre_app = nombre_app,
    conciliacion_crm = conciliacion_crm,
    conciliacion_auto = auto,
    fecha_registro = as.Date(emb$fecha_registro)
  ) |>
    dplyr::left_join(ov, by = "person_key") |>
    dplyr::mutate(
      conciliacion = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$ov_conciliacion)), ""),
        dplyr::na_if(stringr::str_squish(as.character(.data$conciliacion_crm)), ""),
        .data$conciliacion_auto,
        .data$nombre_app
      ),
      canal = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$ov_canal)), ""),
        unname(canal_sheet[.data$person_key]),
        unname(canal_sheet[origenes_brokers_canon_key(.data$nombre_key, alias)])
      ),
      # Manual del UI gana; si no, líderes de equipo del hub; si no, override/seed.
      pertenece_equipo = dplyr::case_when(
        dplyr::coalesce(as.logical(.data$ov_pertenece_manual), FALSE) ~
          dplyr::coalesce(as.logical(.data$ov_pertenece), FALSE),
        .data$person_key %in% origenes_brokers_canon_key(team_keys, alias) ~ TRUE,
        .data$nombre_key %in% team_keys ~ TRUE,
        !is.na(.data$ov_pertenece) ~ as.logical(.data$ov_pertenece),
        TRUE ~ FALSE
      ),
      generacion = dplyr::na_if(stringr::str_squish(as.character(.data$ov_generacion)), ""),
      fecha_graduacion = {
        ov_d <- suppressWarnings(as.Date(.data$ov_fecha_graduacion))
        n <- length(ov_d)
        sheet_by_person <- if (nrow(grad_sheet)) {
          grad_sheet$fecha_graduacion[match(.data$person_key, grad_sheet$embajador_key)]
        } else {
          as.Date(rep(NA, n))
        }
        sheet_by_app <- if (nrow(grad_sheet)) {
          grad_sheet$fecha_graduacion[match(.data$nombre_key, grad_sheet$embajador_key)]
        } else {
          as.Date(rep(NA, n))
        }
        sheet_by_conc <- if (nrow(grad_sheet)) {
          grad_sheet$fecha_graduacion[match(
            brokers_norm_name(.data$conciliacion),
            grad_sheet$embajador_key
          )]
        } else {
          as.Date(rep(NA, n))
        }
        dplyr::coalesce(ov_d, sheet_by_person, sheet_by_app, sheet_by_conc)
      },
      gen_registro = dplyr::na_if(stringr::str_squish(as.character(.data$ov_gen_registro)), ""),
      fuente_conciliacion = dplyr::case_when(
        !is.na(.data$ov_conciliacion) & nzchar(.data$ov_conciliacion) ~ "manual",
        !is.na(.data$conciliacion_crm) & nzchar(.data$conciliacion_crm) ~ "crm",
        !is.na(.data$conciliacion_auto) & nzchar(.data$conciliacion_auto) ~ "auto",
        TRUE ~ "nombre_app"
      ),
      .excluido = dplyr::coalesce(as.logical(.data$ov_excluido), FALSE)
    ) |>
    dplyr::filter(!.data$.excluido) |>
    dplyr::arrange(.data$fecha_registro, .data$nombre_app, .data$amb_contact_id) |>
    dplyr::group_by(.data$person_key) |>
    dplyr::summarise(
      amb_contact_id = dplyr::first(.data$amb_contact_id),
      # Clave de edición = nombre de app (primera inscripción Ambassadors).
      nombre_key = brokers_norm_name(dplyr::first(.data$nombre_app)),
      nombre_app = dplyr::first(.data$nombre_app),
      conciliacion = first_nonempty(.data$conciliacion),
      conciliacion_auto = first_nonempty(.data$conciliacion_auto),
      canal = first_nonempty(.data$canal),
      pertenece_equipo = any(dplyr::coalesce(.data$pertenece_equipo, FALSE)),
      generacion = first_nonempty(.data$generacion),
      fecha_graduacion = {
        d <- .data$fecha_graduacion[!is.na(.data$fecha_graduacion)]
        if (!length(d)) as.Date(NA) else d[[1L]]
      },
      gen_registro = first_nonempty(.data$gen_registro),
      fecha_registro = {
        d <- .data$fecha_registro[!is.na(.data$fecha_registro)]
        if (!length(d)) as.Date(NA) else min(d)
      },
      fuente_conciliacion = dplyr::first(.data$fuente_conciliacion),
      .groups = "drop"
    )

  alias <- origenes_brokers_fold_alias(root)
  # Históricos ya persistidos (no re-leer el Sheets en cada request).
  ya <- unique(c(
    origenes_brokers_canon_key(roster$nombre_app, alias),
    origenes_brokers_canon_key(roster$nombre_key, alias),
    origenes_brokers_canon_key(roster$conciliacion, alias),
    origenes_brokers_canon_key(emb$nombre, alias),
    origenes_brokers_canon_key(emb$nombre_raw, alias)
  ))
  hist <- origenes_brokers_load_historicos(root)
  hist <- hist[!hist$canon %in% ya, , drop = FALSE]
  if (nrow(hist)) {
    ov_hist <- overrides |>
      dplyr::mutate(person_key = origenes_brokers_canon_key(.data$nombre_key, alias))
    hist_rows <- hist |>
      dplyr::mutate(
        nombre_key = brokers_norm_name(.data$nombre),
        person_key = .data$canon
      ) |>
      dplyr::left_join(
        ov_hist |>
          dplyr::select(
            "person_key",
            ov_conciliacion = "conciliacion",
            ov_canal = "canal",
            ov_pertenece = "pertenece_equipo",
            ov_generacion = "generacion",
            ov_fecha_graduacion = "fecha_graduacion"
          ),
        by = "person_key"
      )
    roster <- dplyr::bind_rows(
      roster,
      tibble::tibble(
        amb_contact_id = paste0("historico-", hist_rows$canon),
        nombre_key = hist_rows$nombre_key,
        nombre_app = hist_rows$nombre,
        conciliacion = dplyr::coalesce(
          dplyr::na_if(stringr::str_squish(as.character(hist_rows$ov_conciliacion)), ""),
          hist_rows$nombre
        ),
        conciliacion_auto = NA_character_,
        canal = dplyr::coalesce(
          dplyr::na_if(stringr::str_squish(as.character(hist_rows$ov_canal)), ""),
          hist_rows$canal
        ),
        pertenece_equipo = dplyr::coalesce(
          as.logical(hist_rows$ov_pertenece),
          hist_rows$pertenece_equipo,
          FALSE
        ),
        generacion = dplyr::coalesce(
          dplyr::na_if(stringr::str_squish(as.character(hist_rows$ov_generacion)), ""),
          hist_rows$generacion
        ),
        fecha_graduacion = suppressWarnings(as.Date(hist_rows$ov_fecha_graduacion)),
        gen_registro = NA_character_,
        fecha_registro = hist_rows$fecha_registro,
        fuente_conciliacion = "historico"
      )
    )
  }
  roster <- roster |>
    dplyr::arrange(.data$nombre_app, .data$nombre_key)
  attr(roster, "excluded_keys") <- unique(excluded_keys[!is.na(excluded_keys) & nzchar(excluded_keys)])
  roster
}

#' Aplica overrides del roster a embajadores del origin (antes del enrich).
origenes_brokers_apply_roster_to_embajadores <- function(embajadores,
                                                         roster) {
  if (!nrow(embajadores) || is.null(roster)) {
    return(
      embajadores |>
        dplyr::mutate(
          pertenece_equipo = FALSE,
          generacion = NA_character_,
          canal = NA_character_
        )
    )
  }

  # Clave de app: preferir nombre_raw (hub) para pegar con overrides.
  emb <- embajadores |>
    dplyr::mutate(
      .app_key = brokers_norm_name(dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$nombre_raw)), ""),
        stringr::str_squish(as.character(.data$nombre))
      ))
    )
  excluded <- attr(roster, "excluded_keys", exact = TRUE)
  if (length(excluded)) {
    emb <- emb |>
      dplyr::filter(is.na(.data$.app_key) | !.data$.app_key %in% excluded)
  }
  if (!nrow(roster)) {
    return(
      emb |>
        dplyr::mutate(
          pertenece_equipo = FALSE,
          generacion = NA_character_,
          canal = NA_character_
        ) |>
        dplyr::select(-".app_key")
    )
  }

  map <- roster |>
    dplyr::select(
      .app_key = "nombre_key",
      conciliacion = "conciliacion",
      canal = "canal",
      pertenece_equipo = "pertenece_equipo",
      generacion = "generacion",
      gen_registro_meses = "gen_registro"
    ) |>
    dplyr::distinct(.data$.app_key, .keep_all = TRUE)

  emb <- emb |>
    dplyr::distinct(.data$.app_key, .keep_all = TRUE) |>
    dplyr::left_join(map, by = ".app_key", relationship = "one-to-one") |>
    dplyr::mutate(
      nombre = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$conciliacion)), ""),
        stringr::str_squish(as.character(.data$nombre))
      ),
      nombre_key = brokers_norm_name(.data$nombre),
      pertenece_equipo = dplyr::coalesce(as.logical(.data$pertenece_equipo), FALSE),
      generacion = dplyr::na_if(stringr::str_squish(as.character(.data$generacion)), ""),
      gen_registro_meses = dplyr::na_if(stringr::str_squish(as.character(.data$gen_registro_meses)), ""),
      canal = dplyr::na_if(stringr::str_squish(as.character(.data$canal)), "")
    ) |>
    dplyr::select(-".app_key", -dplyr::any_of("conciliacion"))
  origenes_brokers_collapse_same_person(emb)
}

#' Carga la gen de graduación y el mes de Gen Registro desde el export de Embajadores.
#' No toca conciliación, canal ni equipo. Quien no está en el export queda sin gen.
origenes_brokers_import_graduacion <- function(embajadores,
                                              root = Sys.getenv("ORIGENES_APP_ROOT", "."),
                                              sheet_path = NULL) {
  path <- sheet_path %||% origenes_brokers_embajadores_sheet_path(root)
  if (!file.exists(path)) {
    stop("No está el export de Embajadores", call. = FALSE)
  }
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  alias <- origenes_brokers_fold_alias(root)
  gen <- stringr::str_squish(gsub("[()]", "", as.character(raw[["Generación"]] %||% "")))
  gen[is.na(gen)] <- ""
  mes_m <- stringr::str_match(
    stringr::str_squish(as.character(raw[["Gen Registro"]] %||% "")),
    "(\\d{4})\\s*-\\s*(\\d{2})"
  )
  mes <- ifelse(is.na(mes_m[, 2]), "", paste(mes_m[, 2], mes_m[, 3], sep = "-"))
  nom <- origenes_brokers_canon_key(raw[["Nombre"]], alias)
  conc <- if ("Conciliacion Nombre" %in% names(raw)) {
    origenes_brokers_canon_key(raw[["Conciliacion Nombre"]], alias)
  } else {
    rep("", nrow(raw))
  }
  long <- tibble::tibble(
    canon = c(nom, conc),
    generacion = c(gen, gen),
    mes = c(mes, mes)
  ) |>
    dplyr::filter(nzchar(.data$canon)) |>
    dplyr::group_by(.data$canon) |>
    dplyr::summarise(
      gen_registro = paste(unique(mes[nzchar(mes) & nzchar(generacion)]), collapse = "|"),
      generacion = {
        g <- generacion[!is.na(generacion) & nzchar(generacion)]
        if (length(g)) g[[1]] else ""
      },
      .groups = "drop"
    )

  nombre_raw <- dplyr::coalesce(
    dplyr::na_if(stringr::str_squish(as.character(embajadores$nombre_raw %||% "")), ""),
    stringr::str_squish(as.character(embajadores$nombre))
  )
  api <- tibble::tibble(
    nombre_key = brokers_norm_name(nombre_raw),
    amb_contact_id = as.character(embajadores$amb_contact_id %||% NA_character_),
    nombre_app = stringr::str_squish(as.character(embajadores$nombre %||% nombre_raw)),
    canon = origenes_brokers_canon_key(nombre_raw, alias),
    canon_nom = origenes_brokers_canon_key(embajadores$nombre, alias)
  ) |>
    dplyr::filter(!is.na(.data$nombre_key), nzchar(.data$nombre_key)) |>
    dplyr::distinct(.data$nombre_key, .keep_all = TRUE)

  hit <- match(api$canon, long$canon)
  miss <- is.na(hit)
  hit[miss] <- match(api$canon_nom[miss], long$canon)
  api$on_sheet <- !is.na(hit)
  api$generacion <- ifelse(api$on_sheet, long$generacion[hit], "")
  api$gen_registro <- ifelse(api$on_sheet & nzchar(api$generacion), long$gen_registro[hit], "")
  api$generacion[is.na(api$generacion)] <- ""
  api$gen_registro[is.na(api$gen_registro)] <- ""

  ov <- origenes_brokers_load_overrides(root)
  if (!"gen_registro" %in% names(ov)) {
    ov$gen_registro <- NA_character_
  }
  known <- api$nombre_key
  ov_keep <- ov[!ov$nombre_key %in% known, , drop = FALSE]
  ov_hit <- ov[ov$nombre_key %in% known, , drop = FALSE]
  ov_hit$generacion <- api$generacion[match(ov_hit$nombre_key, api$nombre_key)]
  ov_hit$gen_registro <- api$gen_registro[match(ov_hit$nombre_key, api$nombre_key)]
  missing <- api[!api$nombre_key %in% ov$nombre_key & nzchar(api$generacion), , drop = FALSE]
  ov_new <- tibble::tibble(
    nombre_key = missing$nombre_key,
    amb_contact_id = missing$amb_contact_id,
    nombre_app = missing$nombre_app,
    conciliacion = NA_character_,
    canal = NA_character_,
    pertenece_equipo = NA,
    pertenece_manual = FALSE,
    generacion = missing$generacion,
    gen_registro = missing$gen_registro,
    excluido = FALSE,
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  out <- dplyr::bind_rows(ov_keep, ov_hit, ov_new)
  origenes_brokers_save_overrides(out, root)
  invisible(list(
    path = origenes_brokers_overrides_path(root),
    con_gen = sum(nzchar(api$generacion)),
    en_sheet = sum(api$on_sheet),
    filas_api = nrow(api)
  ))
}

#' Registrados sin equipos: todos excepto Pertenece a un equipo = "sí".
#' Cuenta "no" y vacío; solo excluye "sí".
origenes_brokers_es_registrado_sin_equipo <- function(pertenece_equipo, generacion = NULL) {
  # generacion se ignora (compat. de firma); el criterio es solo pertenece ≠ sí.
  !dplyr::coalesce(as.logical(pertenece_equipo), FALSE)
}
