# Resumen ejecutivo por origen (NxtGen / Broker).
# Tablas alineadas al Excel: Embajadores | Resultados/Actividad | Conversión Citas.
# Fuente: data/canonical/origenes_joined.rds

origenes_load_joined <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  path <- file.path(root, "data", "canonical", "origenes_joined.rds")
  if (!file.exists(path)) {
    return(NULL)
  }
  obj <- readRDS(path)
  attr(obj, "source_path") <- normalizePath(path, winslash = "/", mustWork = FALSE)
  attr(obj, "source_label") <- "canonical_joined"
  obj
}

origenes_sale_is_firmado <- function(estado) {
  grepl("firmad", origenes_normalize_key(estado))
}

origenes_sale_is_proceso <- function(estado) {
  key <- origenes_normalize_key(estado)
  grepl("apartado|carta|documento|intencion|proceso|aprobacion|porcentaje|[0-9]{2}", key) &
    !grepl("firmad|cancel|draft|borrador", key)
}

origenes_meeting_is_realizada <- function(status_os) {
  origenes_normalize_key(status_os) == "realizada"
}

#' Enriquece embajadores con actividad desde meetings RSCG.
origenes_enrich_embajadores <- function(embajadores, meetings, as_of = Sys.Date()) {
  if (!nrow(embajadores)) {
    return(embajadores |>
      dplyr::mutate(
        fecha_primera_cita = as.Date(NA),
        fecha_ultima_cita = as.Date(NA),
        tiene_cita_agendada = FALSE,
        tiene_primera_cita = FALSE,
        activo_30d = FALSE
      ))
  }

  active_days <- brokers_active_window_days()
  m <- meetings |>
    dplyr::filter(!is.na(.data$embajador_key), nzchar(.data$embajador_key), !is.na(.data$fecha)) |>
    dplyr::mutate(
      realizada = origenes_meeting_is_realizada(.data$status_os),
      es_primera = dplyr::coalesce(as.logical(.data$first_meeting), FALSE)
    )

  by_emb <- m |>
    dplyr::group_by(.data$embajador_key) |>
    dplyr::summarise(
      fecha_ultima_cita = max(.data$fecha, na.rm = TRUE),
      fecha_primera_cita = {
        fechas <- .data$fecha[.data$realizada & .data$es_primera]
        if (length(fechas)) min(fechas) else as.Date(NA)
      },
      .groups = "drop"
    )

  embajadores |>
    dplyr::left_join(by_emb, by = c("nombre_key" = "embajador_key")) |>
    dplyr::mutate(
      tiene_cita_agendada = !is.na(.data$fecha_ultima_cita),
      tiene_primera_cita = !is.na(.data$fecha_primera_cita),
      activo_30d = !is.na(.data$fecha_ultima_cita) &
        .data$fecha_ultima_cita >= (as_of - active_days) &
        .data$fecha_ultima_cita <= as_of
    )
}

origenes_resumen_date_span <- function(origin_obj) {
  dates <- c(
    origin_obj$embajadores$fecha_registro,
    origin_obj$meetings$fecha,
    origin_obj$sales$fecha
  )
  dates <- as.Date(dates)
  dates <- dates[!is.na(dates)]
  if (!length(dates)) {
    today <- Sys.Date()
    return(list(start = lubridate::floor_date(today, "month"), end = today))
  }
  list(
    start = lubridate::floor_date(min(dates), "month"),
    end = max(dates)
  )
}

origenes_resumen_prepare <- function(origin_obj, as_of = Sys.Date()) {
  emb <- origenes_enrich_embajadores(
    origin_obj$embajadores %||% tibble::tibble(),
    origin_obj$meetings %||% tibble::tibble(),
    as_of = as_of
  )
  meetings <- origin_obj$meetings %||% tibble::tibble()
  if (nrow(meetings)) {
    meetings <- meetings |>
      dplyr::mutate(
        fecha_cita = as.Date(.data$fecha),
        realizada = origenes_meeting_is_realizada(.data$status_os),
        primera_cita = dplyr::coalesce(as.logical(.data$first_meeting), FALSE)
      )
  }
  sales <- origin_obj$sales %||% tibble::tibble()
  if (nrow(sales)) {
    sales <- sales |>
      dplyr::mutate(
        fecha_firma = as.Date(.data$fecha),
        es_firmado = origenes_sale_is_firmado(.data$estado_venta),
        es_en_proceso = origenes_sale_is_proceso(.data$estado_venta),
        unidades = dplyr::coalesce(suppressWarnings(as.numeric(.data$n_unidades)), 1),
        precio = dplyr::coalesce(
          suppressWarnings(as.numeric(.data$monto_facturacion)),
          suppressWarnings(as.numeric(.data$monto)),
          0
        )
      )
  }
  list(embajadores = emb, meetings = meetings, sales = sales, as_of = as_of)
}

origenes_granularity_choices <- function() {
  c(
    "Semanal" = "semanal",
    "Mensual" = "mensual",
    "Trimestral" = "trimestral",
    "Semestral" = "semestral",
    "Anual" = "anual"
  )
}

origenes_granularity_values <- function() {
  unname(origenes_granularity_choices())
}

origenes_floor_semester <- function(d) {
  d <- as.Date(d)
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  if (m <= 6L) {
    as.Date(paste0(y, "-01-01"))
  } else {
    as.Date(paste0(y, "-07-01"))
  }
}

origenes_period_starts <- function(start, end, granularity = c("mensual", "semanal", "trimestral", "semestral", "anual")) {
  granularity <- match.arg(granularity)
  start <- as.Date(start)
  end <- as.Date(end)
  if (is.na(start) || is.na(end) || end < start) {
    return(as.Date(character()))
  }
  if (identical(granularity, "anual")) {
    seq(lubridate::floor_date(start, "year"), lubridate::floor_date(end, "year"), by = "year")
  } else if (identical(granularity, "semestral")) {
    seq(origenes_floor_semester(start), origenes_floor_semester(end), by = "6 months")
  } else if (identical(granularity, "trimestral")) {
    seq(lubridate::floor_date(start, "quarter"), lubridate::floor_date(end, "quarter"), by = "quarter")
  } else if (identical(granularity, "mensual")) {
    seq(lubridate::floor_date(start, "month"), lubridate::floor_date(end, "month"), by = "month")
  } else {
    seq(
      lubridate::floor_date(start, "week", week_start = 1),
      lubridate::floor_date(end, "week", week_start = 1),
      by = "week"
    )
  }
}

origenes_period_label <- function(period_start, granularity = c("mensual", "semanal", "trimestral", "semestral", "anual")) {
  granularity <- match.arg(granularity)
  period_start <- as.Date(period_start)
  if (identical(granularity, "anual")) {
    format(period_start, "%Y")
  } else if (identical(granularity, "semestral")) {
    semestre <- if (as.integer(format(period_start, "%m")) <= 6L) 1L else 2L
    paste0(format(period_start, "%Y"), " S", semestre)
  } else if (identical(granularity, "trimestral")) {
    paste0(format(period_start, "%Y"), " Q", lubridate::quarter(period_start))
  } else if (identical(granularity, "mensual")) {
    paste(format(period_start, "%Y"), format(period_start, "%m"), sep = " - ")
  } else {
    months_es <- c(
      "ene", "feb", "mar", "abr", "may", "jun",
      "jul", "ago", "sep", "oct", "nov", "dic"
    )
    pe <- period_start + 6
    paste0(
      format(period_start, "%d"), " ", months_es[as.integer(format(period_start, "%m"))],
      " · ",
      format(pe, "%d"), " ", months_es[as.integer(format(pe, "%m"))]
    )
  }
}

origenes_period_end <- function(period_start, granularity = c("mensual", "semanal", "trimestral", "semestral", "anual")) {
  granularity <- match.arg(granularity)
  period_start <- as.Date(period_start)
  if (identical(granularity, "anual")) {
    lubridate::ceiling_date(period_start, "year") - 1
  } else if (identical(granularity, "semestral")) {
    # Fin del semestre: 30 jun o 31 dic
    if (as.integer(format(period_start, "%m")) <= 6L) {
      as.Date(paste0(format(period_start, "%Y"), "-06-30"))
    } else {
      as.Date(paste0(format(period_start, "%Y"), "-12-31"))
    }
  } else if (identical(granularity, "trimestral")) {
    lubridate::ceiling_date(period_start, "quarter") - 1
  } else if (identical(granularity, "mensual")) {
    lubridate::ceiling_date(period_start, "month") - 1
  } else {
    period_start + 6
  }
}

origenes_period_label_es <- function(granularity) {
  switch(
    granularity,
    anual = "Anual",
    semestral = "Semestral",
    mensual = "Mensual",
    semanal = "Semanal",
    trimestral = "Trimestral",
    granularity
  )
}

origenes_normalize_granularity <- function(granularity, default = "mensual") {
  vals <- origenes_granularity_values()
  if (is.null(granularity) || !nzchar(granularity) || !(granularity %in% vals)) {
    return(default)
  }
  granularity
}

origenes_resumen_tabla_embajadores <- function(prep, start, end, granularity = "mensual") {
  granularity <- match.arg(granularity, origenes_granularity_values())
  periods <- origenes_period_starts(start, end, granularity)
  emb <- prep$embajadores
  active_days <- brokers_active_window_days()

  cols <- lapply(periods, function(ps) {
    pe <- origenes_period_end(ps, granularity)
    cohort <- emb |>
      dplyr::filter(
        !is.na(.data$fecha_registro),
        .data$fecha_registro >= ps,
        .data$fecha_registro <= pe
      )
    list(
      registrados = nrow(cohort),
      cita_agendada = sum(cohort$tiene_cita_agendada, na.rm = TRUE),
      primera_cita = sum(cohort$tiene_primera_cita, na.rm = TRUE),
      activos = sum(cohort$activo_30d, na.rm = TRUE)
    )
  })

  total_reg <- sum(vapply(cols, `[[`, integer(1), "registrados"))
  total_ag <- sum(vapply(cols, `[[`, integer(1), "cita_agendada"))
  total_1ra <- sum(vapply(cols, `[[`, integer(1), "primera_cita"))
  total_activos <- sum(emb$activo_30d, na.rm = TRUE)
  labels <- vapply(periods, origenes_period_label, character(1), granularity = granularity)

  tibble::tibble(
    metrica = c(
      "Registrados",
      "Cita agendada",
      "Primera cita",
      "Conversión primera cita / registrados",
      paste0("Activos (Cita últimos ", active_days, " días)"),
      "Conversión activo / primera cita"
    ),
    Total = c(
      as.character(total_reg),
      as.character(total_ag),
      as.character(total_1ra),
      brokers_fmt_pct(total_1ra, total_reg),
      as.character(total_activos),
      brokers_fmt_pct(total_activos, total_1ra)
    )
  ) |>
    dplyr::bind_cols(
      as.data.frame(
        stats::setNames(
          lapply(seq_along(cols), function(i) {
            c(
              as.character(cols[[i]]$registrados),
              as.character(cols[[i]]$cita_agendada),
              as.character(cols[[i]]$primera_cita),
              brokers_fmt_pct(cols[[i]]$primera_cita, cols[[i]]$registrados),
              as.character(cols[[i]]$activos),
              brokers_fmt_pct(cols[[i]]$activos, cols[[i]]$primera_cita)
            )
          }),
          labels
        ),
        check.names = FALSE
      )
    )
}

origenes_resumen_tabla_resultados <- function(prep, start, end, granularity = "mensual") {
  granularity <- match.arg(granularity, origenes_granularity_values())
  periods <- origenes_period_starts(start, end, granularity)
  meetings <- prep$meetings
  sales <- prep$sales

  cols <- lapply(periods, function(ps) {
    pe <- origenes_period_end(ps, granularity)
    m <- meetings |>
      dplyr::filter(!is.na(.data$fecha_cita), .data$fecha_cita >= ps, .data$fecha_cita <= pe)
    v <- sales |>
      dplyr::filter(
        .data$es_firmado,
        !is.na(.data$fecha_firma),
        .data$fecha_firma >= ps,
        .data$fecha_firma <= pe
      )
    list(
      primera_cita = sum(m$primera_cita & m$realizada, na.rm = TRUE),
      citas_agendadas = nrow(m),
      citas_nuevas = sum(m$primera_cita & m$realizada, na.rm = TRUE),
      total_realizadas = sum(m$realizada, na.rm = TRUE),
      unidades = sum(v$unidades, na.rm = TRUE),
      facturacion = sum(v$precio, na.rm = TRUE)
    )
  })

  labels <- vapply(periods, origenes_period_label, character(1), granularity = granularity)
  totals <- list(
    primera_cita = sum(vapply(cols, `[[`, numeric(1), "primera_cita")),
    citas_agendadas = sum(vapply(cols, `[[`, numeric(1), "citas_agendadas")),
    citas_nuevas = sum(vapply(cols, `[[`, numeric(1), "citas_nuevas")),
    total_realizadas = sum(vapply(cols, `[[`, numeric(1), "total_realizadas")),
    unidades = sum(vapply(cols, `[[`, numeric(1), "unidades")),
    facturacion = sum(vapply(cols, `[[`, numeric(1), "facturacion"))
  )

  tibble::tibble(
    metrica = c(
      "Primera cita",
      "Citas Agendadas",
      "Citas Nuevas Realizadas",
      "Total de citas realizadas",
      "Unidades Vendidas",
      "Facturación"
    ),
    Total = c(
      as.character(totals$primera_cita),
      as.character(totals$citas_agendadas),
      as.character(totals$citas_nuevas),
      as.character(totals$total_realizadas),
      as.character(totals$unidades),
      brokers_fmt_money(totals$facturacion)
    )
  ) |>
    dplyr::bind_cols(
      as.data.frame(
        stats::setNames(
          lapply(seq_along(cols), function(i) {
            c(
              as.character(cols[[i]]$primera_cita),
              as.character(cols[[i]]$citas_agendadas),
              as.character(cols[[i]]$citas_nuevas),
              as.character(cols[[i]]$total_realizadas),
              as.character(cols[[i]]$unidades),
              brokers_fmt_money(cols[[i]]$facturacion)
            )
          }),
          labels
        ),
        check.names = FALSE
      )
    )
}

origenes_resumen_tabla_conversion <- function(prep, start, end, granularity = "mensual") {
  granularity <- match.arg(granularity, origenes_granularity_values())
  periods <- origenes_period_starts(start, end, granularity)
  meetings <- prep$meetings
  sales <- prep$sales
  firmadas <- sales |> dplyr::filter(.data$es_firmado)
  procesos <- sales |> dplyr::filter(.data$es_en_proceso)

  cols <- lapply(periods, function(ps) {
    pe <- origenes_period_end(ps, granularity)
    m <- meetings |>
      dplyr::filter(!is.na(.data$fecha_cita), .data$fecha_cita >= ps, .data$fecha_cita <= pe)
    realizadas <- sum(m$primera_cita & m$realizada, na.rm = TRUE)
    v <- firmadas |>
      dplyr::filter(!is.na(.data$fecha_firma), .data$fecha_firma >= ps, .data$fecha_firma <= pe)
    p <- procesos |>
      dplyr::filter(!is.na(.data$fecha_firma), .data$fecha_firma >= ps, .data$fecha_firma <= pe)
    list(
      cita_realizada = realizadas,
      procesos = nrow(p),
      cierres = nrow(v),
      unidades = sum(v$unidades, na.rm = TRUE),
      facturacion = sum(v$precio, na.rm = TRUE)
    )
  })

  labels <- vapply(periods, origenes_period_label, character(1), granularity = granularity)
  tot_real <- sum(vapply(cols, `[[`, numeric(1), "cita_realizada"))
  tot_proc <- sum(vapply(cols, `[[`, numeric(1), "procesos"))
  tot_close <- sum(vapply(cols, `[[`, numeric(1), "cierres"))
  tot_units <- sum(vapply(cols, `[[`, numeric(1), "unidades"))
  tot_fact <- sum(vapply(cols, `[[`, numeric(1), "facturacion"))

  tibble::tibble(
    metrica = c(
      "Cita Realizada",
      "Procesos de venta",
      "Cierres",
      "Conversión cita realizada / venta",
      "Unidades Vendidas",
      "Facturación"
    ),
    Total = c(
      as.character(tot_real),
      as.character(tot_proc),
      as.character(tot_close),
      brokers_fmt_pct(tot_close, tot_real),
      as.character(tot_units),
      brokers_fmt_money(tot_fact)
    )
  ) |>
    dplyr::bind_cols(
      as.data.frame(
        stats::setNames(
          lapply(seq_along(cols), function(i) {
            c(
              as.character(cols[[i]]$cita_realizada),
              as.character(cols[[i]]$procesos),
              as.character(cols[[i]]$cierres),
              brokers_fmt_pct(cols[[i]]$cierres, cols[[i]]$cita_realizada),
              as.character(cols[[i]]$unidades),
              brokers_fmt_money(cols[[i]]$facturacion)
            )
          }),
          labels
        ),
        check.names = FALSE
      )
    )
}

origenes_resumen_kpis <- function(prep, start = NULL, end = NULL) {
  emb <- prep$embajadores
  meetings <- prep$meetings
  sales <- prep$sales
  firmadas <- sales |> dplyr::filter(.data$es_firmado)

  emb_cohort <- emb
  if (!is.null(start) && !is.null(end)) {
    emb_cohort <- emb |>
      dplyr::filter(
        !is.na(.data$fecha_registro),
        .data$fecha_registro >= as.Date(start),
        .data$fecha_registro <= as.Date(end)
      )
  }

  registrados <- nrow(emb_cohort)
  primera_cita_emb <- sum(emb_cohort$tiene_primera_cita, na.rm = TRUE)
  activos <- sum(emb$activo_30d, na.rm = TRUE)
  citas_nuevas <- sum(meetings$primera_cita & meetings$realizada, na.rm = TRUE)
  unidades <- sum(firmadas$unidades, na.rm = TRUE)
  facturacion <- sum(firmadas$precio, na.rm = TRUE)
  cierres <- nrow(firmadas)

  list(
    registrados = registrados,
    conv_primera_reg = if (registrados > 0) primera_cita_emb / registrados else NA_real_,
    primera_cita_emb = primera_cita_emb,
    activos = activos,
    conv_activo_primera = if (primera_cita_emb > 0) activos / primera_cita_emb else NA_real_,
    citas_nuevas = citas_nuevas,
    citas_agendadas = nrow(meetings),
    cierres = cierres,
    conv_cita_cierre = if (citas_nuevas > 0) cierres / citas_nuevas else NA_real_,
    unidades = unidades,
    facturacion = facturacion
  )
}

#' Rango por defecto del filtro (estilo IBR): lo que va del año en curso.
#' En 2026 → 01/01/2026 → hoy.
origenes_default_date_range <- function(as_of = Sys.Date()) {
  as_of <- as.Date(as_of)
  start <- as.Date(paste0(format(as_of, "%Y"), "-01-01"))
  end <- as_of
  if (end < start) {
    end <- start
  }
  list(start = start, end = end)
}

origenes_resumen_default_start <- function(origin_key = NULL, as_of = Sys.Date()) {
  origenes_default_date_range(as_of)$start
}

origenes_resumen_default_end <- function(as_of = Sys.Date()) {
  origenes_default_date_range(as_of)$end
}

origenes_calendar_max_date <- function(as_of = Sys.Date()) {
  as_of <- as.Date(as_of)
  year <- max(2026L, as.integer(format(as_of, "%Y")))
  as.Date(paste0(year, "-12-31"))
}

origenes_read_calendar_range <- function(start_input, end_input, as_of = Sys.Date()) {
  defaults <- origenes_default_date_range(as_of)
  start <- suppressWarnings(as.Date(start_input))
  end <- suppressWarnings(as.Date(end_input))
  if (length(start) != 1L || is.na(start)) {
    start <- defaults$start
  }
  if (length(end) != 1L || is.na(end)) {
    end <- defaults$end
  }
  if (end < start) {
    tmp <- start
    start <- end
    end <- tmp
  }
  list(start = start, end = end)
}

origenes_format_date_range_label <- function(start, end) {
  start <- suppressWarnings(as.Date(start))
  end <- suppressWarnings(as.Date(end))
  if (length(start) != 1L || is.na(start) || length(end) != 1L || is.na(end)) {
    return("Sin rango seleccionado")
  }
  paste0(format(start, "%d/%m/%Y"), " → ", format(end, "%d/%m/%Y"))
}

#' Bundle completo de Resumen para un origen del canónico unido.
origenes_resumen_bundle <- function(joined,
                                    origin_key,
                                    as_of = Sys.Date(),
                                    start = NULL,
                                    end = NULL,
                                    granularity = c("mensual", "semanal", "trimestral", "semestral", "anual")) {
  granularity <- match.arg(granularity)
  origin_key <- origenes_origin_key(origin_key)
  origin_obj <- joined$origins[[origin_key]]
  if (is.null(origin_obj)) {
    return(NULL)
  }
  as_of <- as.Date(as_of)
  defaults <- origenes_default_date_range(as_of)
  start <- as.Date(start %||% defaults$start)
  end <- as.Date(end %||% defaults$end)
  if (is.na(end) || end < start) {
    end <- defaults$end
  }

  prep <- origenes_resumen_prepare(origin_obj, as_of = as_of)

  # Recorta actividad comercial al periodo del Resumen (como el Excel).
  prep$meetings <- prep$meetings |>
    dplyr::filter(
      is.na(.data$fecha_cita) |
        (.data$fecha_cita >= start & .data$fecha_cita <= end)
    )
  prep$sales <- prep$sales |>
    dplyr::filter(
      is.na(.data$fecha_firma) |
        (.data$fecha_firma >= start & .data$fecha_firma <= end)
    )
  # Embajadores: se mantienen todos para activos globales; tablas filtran por cohorte.

  list(
    origin = origin_obj$origin,
    origin_key = origin_key,
    granularity = granularity,
    span = list(start = start, end = end),
    kpis = origenes_resumen_kpis(prep, start = start, end = end),
    tabla_embajadores = origenes_resumen_tabla_embajadores(
      prep, start, end, granularity = granularity
    ),
    tabla_resultados = origenes_resumen_tabla_resultados(
      prep, start, end, granularity = granularity
    ),
    tabla_conversion = origenes_resumen_tabla_conversion(
      prep, start, end, granularity = granularity
    ),
    source_label = attr(joined, "source_label", exact = TRUE) %||% "canonical_joined",
    built_at = joined$built_at %||% NA
  )
}

origenes_coming_soon_ui <- function(title, copy) {
  shiny::div(
    class = "or-empty",
    shiny::div(class = "or-empty__icon", "↗"),
    shiny::h3(title),
    shiny::p(copy)
  )
}
