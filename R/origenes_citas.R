# Tabla de detalle Citas (NxtGen / Brokers) alineada al Sheets.
# Fuente: Bubble meetings (Origin_OS). Embajador Registrado = nombre conciliado.
# Brokers: Gen Graduación / Post Graduación desde Fecha graduación de registros
# Academy (si el CSV local está disponible); si no, NA.

origenes_fmt_primera_cita <- function(x) {
  if (is.logical(x)) {
    return(dplyr::if_else(x, "si", "no", missing = NA_character_))
  }
  key <- origenes_normalize_key(x)
  dplyr::case_when(
    is.na(x) | !nzchar(as.character(x)) ~ NA_character_,
    key %in% c("true", "1", "yes", "si", "sí") ~ "si",
    key %in% c("false", "0", "no") ~ "no",
    TRUE ~ tolower(stringr::str_squish(as.character(x)))
  )
}

origenes_fmt_datetime_sheets <- function(d) {
  # Sheets muestra dd/mm/yy H:MM:SS en citas; usamos dd/mm/yyyy HH:MM
  if (inherits(d, "POSIXt")) {
    return(ifelse(is.na(d), NA_character_, format(d, "%d/%m/%Y %H:%M")))
  }
  origenes_fmt_date_sheets(d)
}

origenes_brokers_graduacion_map <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  path <- file.path(
    root, "data", "brokers", "raw",
    "Brokers Academy Analytics - Registros de embajadores.csv"
  )
  if (!file.exists(path)) {
    return(tibble::tibble(
      embajador_key = character(),
      fecha_graduacion = as.Date(character()),
      gen_graduacion = character()
    ))
  }
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  nombre <- stringr::str_squish(as.character(raw[["Nombre"]]))
  conc <- stringr::str_squish(as.character(raw[["Conciliacion Nombre"]]))
  fecha_grad <- suppressWarnings(lubridate::parse_date_time(
    as.character(raw[["Fecha graduación"]]),
    orders = c("dmy", "dmY", "Ymd"),
    quiet = TRUE,
    tz = "UTC"
  ))
  tibble::tibble(
    nombre = dplyr::coalesce(dplyr::na_if(conc, ""), nombre),
    fecha_graduacion = as.Date(fecha_grad)
  ) |>
    dplyr::filter(!is.na(.data$nombre), nzchar(.data$nombre), !is.na(.data$fecha_graduacion)) |>
    dplyr::mutate(
      embajador_key = brokers_norm_name(.data$nombre),
      gen_graduacion = origenes_ym_label(.data$fecha_graduacion)
    ) |>
    dplyr::group_by(.data$embajador_key) |>
    dplyr::summarise(
      fecha_graduacion = min(.data$fecha_graduacion),
      gen_graduacion = dplyr::first(.data$gen_graduacion[order(.data$fecha_graduacion)]),
      .groups = "drop"
    )
}

origenes_citas_empty <- function(include_graduacion = FALSE) {
  out <- tibble::tibble(
    `Nombre del Prospecto` = character(),
    `Fecha de Inicio` = character(),
    Estatus = character(),
    Embajador = character(),
    `Primera Cita` = character(),
    `Fecha de Creación` = character(),
    `Embajador Registrado` = character(),
    `Gen Cita` = character()
  )
  if (include_graduacion) {
    out[["Gen Graduación"]] <- character()
    out[["¿Fue Post Graduación?"]] <- character()
  }
  out
}

#' Tabla Citas con columnas del Sheets, filtrada por origen y rango de fechas.
origenes_citas_table <- function(origin_key,
                                 start = NULL,
                                 end = NULL,
                                 root = Sys.getenv("ORIGENES_APP_ROOT", "."),
                                 rscg = NULL,
                                 name_map = NULL,
                                 joined = NULL) {
  origin_key <- origenes_origin_key(origin_key)
  include_grad <- identical(origin_key, "broker")
  defaults <- origenes_default_date_range()
  start <- as.Date(start %||% defaults$start)
  end <- as.Date(end %||% defaults$end)

  if (is.null(rscg)) {
    rscg <- tryCatch(origenes_load_rscg_bundle(root), error = function(e) NULL)
  }
  if (is.null(rscg)) {
    return(origenes_citas_empty(include_grad))
  }

  mr <- rscg$meetings_raw %||% tibble::tibble()
  if (!nrow(mr)) {
    return(origenes_citas_empty(include_grad))
  }

  first_col <- intersect(c("First.meeting", "First.Meeting"), names(mr))
  first_col <- if (length(first_col)) first_col[[1]] else NA_character_

  start_raw <- mr$Start.date
  created_raw <- mr$Created.Date

  meetings <- tibble::tibble(
    prospecto = stringr::str_squish(as.character(mr$Lead.full.name)),
    fecha_inicio = origenes_parse_date(start_raw),
    fecha_inicio_ts = suppressWarnings(lubridate::parse_date_time(
      as.character(start_raw),
      orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmy HMS", "dmy HM", "dmy"),
      quiet = TRUE,
      tz = "UTC"
    )),
    estatus = stringr::str_squish(as.character(mr$Status_OS)),
    embajador_raw = stringr::str_squish(as.character(mr$Ambassador)),
    primera_cita = if (!is.na(first_col)) mr[[first_col]] else NA,
    fecha_creacion = origenes_parse_date(created_raw),
    origen_key = origenes_origin_key(mr$Origin_OS)
  ) |>
    dplyr::filter(.data$origen_key == !!origin_key) |>
    dplyr::filter(
      !is.na(.data$fecha_inicio),
      .data$fecha_inicio >= start,
      .data$fecha_inicio <= end
    )

  if (!nrow(meetings)) {
    return(origenes_citas_empty(include_grad))
  }

  if (is.null(name_map) && !is.null(joined)) {
    name_map <- joined$origins[[origin_key]]$name_map
  }
  if (is.null(name_map)) {
    name_map <- origenes_build_name_map(
      origin_key,
      observed_names = meetings$embajador_raw,
      root = root
    )
  }

  meetings <- meetings |>
    dplyr::mutate(
      embajador = origenes_resolve_names(.data$embajador_raw, name_map),
      embajador_registrado = .data$embajador,
      embajador_key = brokers_norm_name(.data$embajador),
      gen_cita = origenes_ym_label(.data$fecha_inicio),
      primera_label = origenes_fmt_primera_cita(.data$primera_cita)
    )

  if (include_grad) {
    grad <- origenes_brokers_graduacion_map(root)
    meetings <- meetings |>
      dplyr::left_join(grad, by = "embajador_key") |>
      dplyr::mutate(
        post_graduacion = dplyr::case_when(
          is.na(.data$fecha_graduacion) ~ NA_character_,
          .data$fecha_inicio > .data$fecha_graduacion ~ "si",
          TRUE ~ "no"
        )
      )
  }

  out <- meetings |>
    dplyr::arrange(dplyr::desc(.data$fecha_inicio), .data$prospecto) |>
    dplyr::transmute(
      `Nombre del Prospecto` = .data$prospecto,
      `Fecha de Inicio` = dplyr::coalesce(
        origenes_fmt_datetime_sheets(.data$fecha_inicio_ts),
        origenes_fmt_date_sheets(.data$fecha_inicio)
      ),
      Estatus = .data$estatus,
      Embajador = dplyr::na_if(.data$embajador_raw, ""),
      `Primera Cita` = .data$primera_label,
      `Fecha de Creación` = origenes_fmt_date_sheets(.data$fecha_creacion),
      `Embajador Registrado` = dplyr::na_if(.data$embajador_registrado, ""),
      `Gen Cita` = .data$gen_cita
    )

  if (include_grad) {
    out[["Gen Graduación"]] <- meetings$gen_graduacion
    out[["¿Fue Post Graduación?"]] <- meetings$post_graduacion
  }

  out
}
