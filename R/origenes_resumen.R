# Resumen ejecutivo por origen (NxtGen / Broker).
# Tablas alineadas al Excel: Embajadores | Resultados/Actividad | Conversión Citas.
# Fuente: data/canonical/origenes_joined.rds

origenes_load_joined <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  root_key <- normalizePath(root, winslash = "/", mustWork = FALSE)
  origenes_data_cached(paste0("joined::", root_key), function() {
    path <- file.path(root, "data", "canonical", "origenes_joined.rds")
    if (!file.exists(path)) {
      return(NULL)
    }
    obj <- readRDS(path)
    attr(obj, "source_path") <- normalizePath(path, winslash = "/", mustWork = FALSE)
    attr(obj, "source_label") <- "canonical_joined"
    obj
  })
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

#' Gen Cita y Fecha de Creación caen en el mismo mes calendario.
#' Sheets (Conversión / Citas nuevas Realizadas): no cuenta citas reagendadas
#' a otro mes distinto al de su Created.Date.
origenes_meeting_created_in_gen_cita <- function(fecha_cita, fecha_creacion) {
  fecha_cita <- as.Date(fecha_cita)
  fecha_creacion <- as.Date(fecha_creacion)
  !is.na(fecha_cita) &
    !is.na(fecha_creacion) &
    format(fecha_cita, "%Y-%m") == format(fecha_creacion, "%Y-%m")
}

#' Citas Agendadas (Actividad): First.meeting = TRUE, un lead una sola vez.
origenes_meetings_count_citas_agendadas <- function(meetings) {
  if (!nrow(meetings)) {
    return(0L)
  }
  m <- meetings
  primera <- if ("primera_cita" %in% names(m)) {
    dplyr::coalesce(as.logical(m$primera_cita), FALSE)
  } else {
    dplyr::coalesce(as.logical(m$first_meeting), FALSE)
  }
  m <- m[primera, , drop = FALSE]
  if (!nrow(m)) {
    return(0L)
  }
  if (!"lead_contact_id" %in% names(m)) {
    return(nrow(m))
  }
  ids <- as.character(m$lead_contact_id)
  has <- !is.na(ids) & nzchar(ids)
  as.integer(length(unique(ids[has])) + sum(!has))
}

#' Cita Realizada (Conversión / Citas nuevas Realizadas):
#' Primera=Sí ∧ Realizada (Start.date / Gen Cita en el mes de la columna).
#' No exige Created.Date en el mismo mes (eso dejaba el total ~107 vs Sheets ~124).
origenes_meeting_is_cita_realizada_conteo <- function(meetings) {
  if (!nrow(meetings)) {
    return(logical())
  }
  realizada <- if ("realizada" %in% names(meetings)) {
    dplyr::coalesce(as.logical(meetings$realizada), FALSE)
  } else {
    origenes_meeting_is_realizada(meetings$status_os)
  }
  primera <- if ("primera_cita" %in% names(meetings)) {
    dplyr::coalesce(as.logical(meetings$primera_cita), FALSE)
  } else if ("first_meeting" %in% names(meetings)) {
    dplyr::coalesce(as.logical(meetings$first_meeting), FALSE)
  } else {
    rep(FALSE, nrow(meetings))
  }
  primera & realizada
}

#' Adjunta Created.Date del CRM (Bubble) a meetings del joined.
origenes_meetings_attach_fecha_creacion <- function(meetings,
                                                   root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  if (!nrow(meetings)) {
    meetings$fecha_creacion <- as.Date(character())
    return(meetings)
  }
  if ("fecha_creacion" %in% names(meetings) && any(!is.na(meetings$fecha_creacion))) {
    meetings$fecha_creacion <- as.Date(meetings$fecha_creacion)
    return(meetings)
  }
  rscg <- tryCatch(origenes_load_rscg_bundle(root), error = function(e) NULL)
  mr <- rscg$meetings_raw %||% tibble::tibble()
  if (!nrow(mr) || !"Created.Date" %in% names(mr)) {
    meetings$fecha_creacion <- as.Date(rep(NA, nrow(meetings)))
    return(meetings)
  }
  id_raw <- origenes_id_norm(mr$X_id %||% mr$`_id` %||% mr$id)
  created <- origenes_parse_date(mr$Created.Date)
  lookup <- tibble::tibble(
    .join_id = id_raw,
    fecha_creacion = as.Date(created)
  ) |>
    dplyr::filter(!is.na(.data$.join_id), nzchar(.data$.join_id)) |>
    dplyr::distinct(.data$.join_id, .keep_all = TRUE)

  mid <- origenes_id_norm(
    meetings$rscg_meeting_id %||% meetings$id %||% rep(NA_character_, nrow(meetings))
  )
  meetings$fecha_creacion <- lookup$fecha_creacion[match(mid, lookup$.join_id)]
  meetings
}

#' Fecha de referencia para L (días desde última realizada) y para el fin del
#' rango visible. Si el cache está atrasado vs Sys.Date(), usa el último día
#' con actividad/registro para no inventar meses vacíos ni Activos=0 vacuamente.
origenes_activity_as_of <- function(meetings,
                                    sales = NULL,
                                    embajadores = NULL,
                                    as_of = Sys.Date()) {
  as_of <- as.Date(as_of)
  dates <- as.Date((meetings %||% tibble::tibble())$fecha %||% as.Date(character()))
  if (!is.null(sales) && nrow(sales) && "fecha" %in% names(sales)) {
    dates <- c(dates, as.Date(sales$fecha))
  }
  if (!is.null(embajadores) && nrow(embajadores) && "fecha_registro" %in% names(embajadores)) {
    dates <- c(dates, as.Date(embajadores$fecha_registro))
  }
  dates <- dates[!is.na(dates)]
  if (!length(dates)) {
    return(as_of)
  }
  data_end <- max(dates)
  if (is.na(as_of) || as_of > data_end) data_end else as_of
}

#' Última fecha con datos de un origen dentro del joined.
origenes_origin_activity_as_of <- function(joined, origin_key, as_of = Sys.Date()) {
  if (is.null(joined) || is.null(joined$origins)) {
    return(as.Date(as_of))
  }
  key <- origenes_origin_key(origin_key)
  origin_obj <- joined$origins[[key]]
  if (is.null(origin_obj)) {
    return(as.Date(as_of))
  }
  origenes_activity_as_of(
    origin_obj$meetings %||% tibble::tibble(),
    origin_obj$sales %||% tibble::tibble(),
    embajadores = origin_obj$embajadores %||% tibble::tibble(),
    as_of = as_of
  )
}

#' Última fecha con datos en el joined (todos los orígenes).
origenes_joined_activity_as_of <- function(joined, as_of = Sys.Date()) {
  if (is.null(joined) || is.null(joined$origins) || !length(joined$origins)) {
    return(as.Date(as_of))
  }
  ends <- vapply(
    names(joined$origins),
    function(key) as.character(origenes_origin_activity_as_of(joined, key, as_of = as_of)),
    character(1)
  )
  ends <- as.Date(ends)
  ends <- ends[!is.na(ends)]
  if (!length(ends)) {
    return(as.Date(as_of))
  }
  max(ends)
}

#' Enriquece embajadores con actividad desde meetings RSCG.
#' Activos = COUNTIFS(Gen Registro, periodo, L, "<31")
#' L Sheets "Activos (Cita últimos 30 días)" = días desde última cita
#' agendada/realizada (col M), no solo realizadas.
#'
#' `primera_cita_desde`: ventana del VLOOKUP Sheets (Citas Brokers Academy).
#' Si se setea (p.ej. 2026-01-01), col I = primera **Realizada** en/desde esa
#' fecha — no la primera de toda la vida CRM (que deja fuera a Angela/Daniel).
origenes_enrich_embajadores <- function(embajadores,
                                        meetings,
                                        as_of = Sys.Date(),
                                        primera_cita_desde = NULL) {
  if (!nrow(embajadores)) {
    return(embajadores |>
      dplyr::mutate(
        fecha_primera_cita = as.Date(NA),
        fecha_ultima_cita = as.Date(NA),
        fecha_ultima_realizada = as.Date(NA),
        gen_primera_cita_realizada = NA_character_,
        dias_desde_ultima_realizada = NA_integer_,
        n_citas = 0L,
        tiene_cita_agendada = FALSE,
        tiene_primera_cita = FALSE,
        tiene_mas_de_una_cita = FALSE,
        activo_30d = FALSE
      ))
  }

  as_of <- as.Date(as_of)
  primera_cita_desde <- if (is.null(primera_cita_desde)) {
    as.Date(NA)
  } else {
    as.Date(primera_cita_desde)
  }
  active_days <- brokers_active_window_days()
  # Sheets: L < 31 con ventana de 30 días ⇒ dias en 0..30
  l_lt <- as.integer(active_days) + 1L

  alias <- origenes_brokers_fold_alias()
  m <- meetings |>
    dplyr::filter(!is.na(.data$embajador_key), nzchar(.data$embajador_key), !is.na(.data$fecha)) |>
    dplyr::mutate(
      fecha = as.Date(.data$fecha),
      realizada = origenes_meeting_is_realizada(.data$status_os),
      es_primera = dplyr::coalesce(as.logical(.data$first_meeting), FALSE),
      .canon = origenes_brokers_canon_key(.data$embajador, alias)
    )

  by_emb <- m |>
    dplyr::group_by(.data$.canon) |>
    dplyr::summarise(
      n_citas = dplyr::n(),
      # Sheets col M: Fecha de última cita agendada/realizada
      fecha_ultima_cita = as.Date(max(.data$fecha, na.rm = TRUE)),
      # Sheets col I: Fecha primera cita realizada (CRM).
      # Ventana academy: 1ª Realizada desde esa fecha. Sin ventana: First+Realizada
      # (fallback First.meeting). Huecos se completan luego con I del Sheets.
      fecha_primera_cita = {
        if (!is.na(primera_cita_desde)) {
          fechas <- .data$fecha[.data$realizada & .data$fecha >= primera_cita_desde]
          if (length(fechas)) as.Date(min(fechas)) else as.Date(NA)
        } else {
          fechas_r <- .data$fecha[.data$realizada & .data$es_primera]
          if (length(fechas_r)) {
            as.Date(min(fechas_r))
          } else {
            fechas <- .data$fecha[.data$es_primera]
            if (length(fechas)) as.Date(min(fechas)) else as.Date(NA)
          }
        }
      },
      # Sheets col: fecha de última cita realizada
      fecha_ultima_realizada = {
        fechas <- .data$fecha[.data$realizada]
        if (length(fechas)) as.Date(max(fechas)) else as.Date(NA)
      },
      .groups = "drop"
    )

  id_src <- dplyr::coalesce(
    dplyr::na_if(stringr::str_squish(as.character(embajadores$nombre_raw %||% NA_character_)), ""),
    stringr::str_squish(as.character(embajadores$nombre))
  )
  embajadores$.canon <- origenes_brokers_canon_key(id_src, alias)
  embajadores |>
    dplyr::left_join(by_emb, by = ".canon") |>
    dplyr::select(-".canon") |>
    dplyr::mutate(
      n_citas = as.integer(dplyr::coalesce(.data$n_citas, 0L)),
      fecha_ultima_cita = as.Date(.data$fecha_ultima_cita),
      fecha_primera_cita = as.Date(.data$fecha_primera_cita),
      fecha_ultima_realizada = as.Date(.data$fecha_ultima_realizada),
      # Sheets col N: Gen primera cita realizada
      gen_primera_cita_realizada = origenes_ym_label(.data$fecha_primera_cita),
      # COUNTIFS(..., M>0)
      tiene_cita_agendada = !is.na(.data$fecha_ultima_cita),
      # COUNTIFS(..., I>0)
      tiene_primera_cita = !is.na(.data$fecha_primera_cita),
      tiene_mas_de_una_cita = .data$n_citas > 1L,
      dias_desde_ultima_realizada = dplyr::if_else(
        is.na(.data$fecha_ultima_realizada),
        NA_integer_,
        as.integer(as_of - .data$fecha_ultima_realizada)
      ),
      # Activos: L sobre última cita cualquier status (alineado a label Sheets)
      activo_30d = !is.na(.data$fecha_ultima_cita) &
        as.integer(as_of - .data$fecha_ultima_cita) >= 0L &
        as.integer(as_of - .data$fecha_ultima_cita) < l_lt
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

origenes_resumen_prepare <- function(origin_obj,
                                     as_of = Sys.Date(),
                                     roster = NULL) {
  emb_raw <- origin_obj$embajadores %||% tibble::tibble()
  meetings_raw <- origin_obj$meetings %||% tibble::tibble()
  sales_raw <- origin_obj$sales %||% tibble::tibble()
  # L / fin de datos se ancla a la última actividad/registro del cache.
  as_of <- origenes_activity_as_of(
    meetings_raw,
    sales_raw,
    embajadores = emb_raw,
    as_of = as_of
  )
  origin_key <- origenes_origin_key(origin_obj$origin_key %||% origin_obj$origin %||% "")
  if (identical(origin_key, "broker") && !is.null(roster)) {
    emb_raw <- origenes_brokers_apply_roster_to_embajadores(emb_raw, roster)
    emb_raw <- origenes_brokers_append_historicos(emb_raw)
  } else if (nrow(emb_raw) && !"pertenece_equipo" %in% names(emb_raw)) {
    emb_raw <- emb_raw |>
      dplyr::mutate(
        pertenece_equipo = FALSE,
        generacion = if ("generacion" %in% names(emb_raw)) .data$generacion else NA_character_,
        canal = if ("canal" %in% names(emb_raw)) .data$canal else NA_character_
      )
  }

  # Re-VLOOKUP nombres con name_map actual (el joined puede traer
  # lista_match viejos / demasiado agresivos, p.ej. Maria Jose → Angeles).
  if (identical(origin_key, "broker")) {
    observed <- c(
      if (nrow(meetings_raw) && "embajador_raw" %in% names(meetings_raw)) {
        meetings_raw$embajador_raw
      } else if (nrow(meetings_raw)) {
        meetings_raw$embajador
      } else {
        NULL
      },
      if ("nombre_raw" %in% names(emb_raw)) emb_raw$nombre_raw else NULL,
      if ("nombre" %in% names(emb_raw)) emb_raw$nombre else NULL
    )
    name_map <- origenes_build_name_map("broker", observed_names = observed)
    if (nrow(meetings_raw) && "embajador" %in% names(meetings_raw)) {
      raw_emb <- if ("embajador_raw" %in% names(meetings_raw)) {
        dplyr::coalesce(
          dplyr::na_if(stringr::str_squish(as.character(meetings_raw$embajador_raw)), ""),
          dplyr::na_if(stringr::str_squish(as.character(meetings_raw$embajador)), "")
        )
      } else {
        dplyr::na_if(stringr::str_squish(as.character(meetings_raw$embajador)), "")
      }
      meetings_raw$embajador <- origenes_resolve_names(raw_emb, name_map)
      meetings_raw$embajador_key <- brokers_norm_name(meetings_raw$embajador)
    }
    if (nrow(emb_raw) && "nombre" %in% names(emb_raw)) {
      raw_nom <- if ("nombre_raw" %in% names(emb_raw)) {
        dplyr::coalesce(
          dplyr::na_if(stringr::str_squish(as.character(emb_raw$nombre_raw)), ""),
          dplyr::na_if(stringr::str_squish(as.character(emb_raw$nombre)), "")
        )
      } else {
        dplyr::na_if(stringr::str_squish(as.character(emb_raw$nombre)), "")
      }
      emb_raw$nombre <- origenes_resolve_names(raw_nom, name_map)
      if ("nombre_key" %in% names(emb_raw)) {
        emb_raw$nombre_key <- brokers_norm_name(emb_raw$nombre)
      }
    }
  }

  primera_desde <- if (identical(origin_key, "broker")) {
    origenes_brokers_history_start()
  } else {
    NULL
  }
  emb <- origenes_enrich_embajadores(
    emb_raw,
    meetings_raw,
    as_of = as_of,
    primera_cita_desde = primera_desde
  )
  if (identical(origin_key, "broker") && nrow(emb)) {
    # COUNTIFS(K, I>0): rellena I desde Embajadores Sheets si el CRM no la trae.
    emb <- origenes_brokers_apply_sheets_primera_cita(emb)
  }
  if (nrow(emb)) {
    if (!"pertenece_equipo" %in% names(emb)) {
      emb$pertenece_equipo <- FALSE
    }
    if (!"generacion" %in% names(emb)) {
      emb$generacion <- NA_character_
    }
    emb <- emb |>
      dplyr::mutate(
        pertenece_equipo = dplyr::coalesce(as.logical(.data$pertenece_equipo), FALSE),
        generacion = dplyr::na_if(stringr::str_squish(as.character(.data$generacion)), ""),
        es_registrado_sin_equipo = origenes_brokers_es_registrado_sin_equipo(
          .data$pertenece_equipo,
          .data$generacion
        ),
        # COUNTIFS(Gen Registro, periodo, Generación, "<>")
        en_capacitacion = !is.na(.data$generacion) & nzchar(.data$generacion)
      )
    lookup <- origenes_brokers_sheets_gen_registro()
    alias <- origenes_brokers_fold_alias()
    raw_key <- if ("nombre_raw" %in% names(emb)) {
      origenes_brokers_canon_key(emb$nombre_raw, alias)
    } else {
      rep("", nrow(emb))
    }
    nom_key <- origenes_brokers_canon_key(emb$nombre, alias)
    meses_raw <- lookup$gen_registro_meses[match(raw_key, lookup$nombre_key)]
    meses_nom <- lookup$gen_registro_meses[match(nom_key, lookup$nombre_key)]
    desde_roster <- if ("gen_registro_meses" %in% names(emb)) {
      dplyr::na_if(stringr::str_squish(as.character(emb$gen_registro_meses)), "")
    } else {
      rep(NA_character_, nrow(emb))
    }
    emb$gen_registro_meses <- dplyr::coalesce(desde_roster, meses_raw, meses_nom)
    if (identical(origin_key, "broker")) {
      emb <- origenes_brokers_assign_fechas_cohorte(emb)
    }
  }

  meetings <- meetings_raw
  if (nrow(meetings)) {
    meetings <- origenes_meetings_attach_fecha_creacion(meetings)
    meetings <- meetings |>
      dplyr::mutate(
        fecha_cita = as.Date(.data$fecha),
        realizada = origenes_meeting_is_realizada(.data$status_os),
        primera_cita = dplyr::coalesce(as.logical(.data$first_meeting), FALSE),
        # Sheets: Gen Cita (Start) + Created.Date en el mismo mes
        created_in_gen_cita = origenes_meeting_created_in_gen_cita(
          .data$fecha_cita,
          .data$fecha_creacion
        ),
        # Conversión / Citas nuevas Realizadas: Primera=Sí ∧ Realizada
        cita_realizada_conteo = .data$primera_cita & .data$realizada
      )
  }
  sales <- sales_raw
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
    if (
      nrow(meetings) &&
        "lead_contact_id" %in% names(meetings) &&
        "client_contact_id" %in% names(sales)
    ) {
      # Gen Cita (Conversión Citas): First.meeting + Realizada del lead.
      # Brokers: ventana academy 2026-01-01 (hoja Citas del Sheets). Si el lead
      # no tiene esa cita en ventana → Gen vacío (no cuenta en columnas Gen Cita).
      m_gen <- meetings |>
        dplyr::filter(
          dplyr::coalesce(.data$primera_cita, FALSE),
          dplyr::coalesce(.data$realizada, FALSE),
          !is.na(.data$fecha_cita),
          !is.na(.data$lead_contact_id),
          nzchar(.data$lead_contact_id)
        )
      if (!is.null(primera_desde)) {
        m_gen <- m_gen |>
          dplyr::filter(.data$fecha_cita >= as.Date(primera_desde))
      }
      primera <- m_gen |>
        dplyr::group_by(.data$lead_contact_id) |>
        dplyr::summarise(gen_cita_fecha = min(.data$fecha_cita), .groups = "drop")
      sales <- sales |>
        dplyr::left_join(primera, by = c("client_contact_id" = "lead_contact_id"))
      # Gen Cita no puede ser posterior a la firma (cita de otro deal / dato raro).
      if ("fecha_firma" %in% names(sales) && "gen_cita_fecha" %in% names(sales)) {
        sales <- sales |>
          dplyr::mutate(
            gen_cita_fecha = dplyr::if_else(
              !is.na(.data$gen_cita_fecha) &
                !is.na(.data$fecha_firma) &
                .data$gen_cita_fecha > .data$fecha_firma,
              as.Date(NA),
              .data$gen_cita_fecha
            )
          )
      }
    }
    if (!"gen_cita_fecha" %in% names(sales)) {
      sales$gen_cita_fecha <- as.Date(NA)
    }
  }
  list(embajadores = emb, meetings = meetings, sales = sales, as_of = as_of)
}

#' Recorta embajadores, sus citas y las ventas de la primera cita realizada de esa persona.
origenes_resumen_filter_personas <- function(prep, keys) {
  keys <- unique(as.character(keys))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  alias <- origenes_brokers_fold_alias()
  canon <- function(x) origenes_brokers_canon_key(x, alias)

  meetings <- prep$meetings %||% tibble::tibble()
  owners <- stats::setNames(character(), character())
  if (
    nrow(meetings) &&
      "lead_contact_id" %in% names(meetings) &&
      "fecha_cita" %in% names(meetings)
  ) {
    primera <- meetings[
      dplyr::coalesce(meetings$primera_cita, FALSE) &
        dplyr::coalesce(meetings$realizada, FALSE) &
        !is.na(meetings$fecha_cita) &
        !is.na(meetings$lead_contact_id) &
        nzchar(meetings$lead_contact_id),
      ,
      drop = FALSE
    ]
    if (nrow(primera)) {
      primera <- primera[order(primera$fecha_cita), , drop = FALSE]
      primera <- primera[!duplicated(primera$lead_contact_id), , drop = FALSE]
      owners <- stats::setNames(
        canon(primera$embajador %||% NA_character_),
        primera$lead_contact_id
      )
    }
  }

  emb <- prep$embajadores %||% tibble::tibble()
  if (nrow(emb)) {
    id <- if ("nombre" %in% names(emb)) emb$nombre else emb$nombre_key %||% NA_character_
    prep$embajadores <- emb[canon(id) %in% keys, , drop = FALSE]
  }
  if (nrow(meetings)) {
    owner <- canon(meetings$embajador %||% NA_character_)
    prep$meetings <- meetings[owner %in% keys, , drop = FALSE]
  }
  sales <- prep$sales %||% tibble::tibble()
  if (nrow(sales) && "client_contact_id" %in% names(sales)) {
    sale_owner <- unname(owners[sales$client_contact_id])
    prep$sales <- sales[!is.na(sale_owner) & sale_owner %in% keys, , drop = FALSE]
  } else if (nrow(sales)) {
    prep$sales <- sales[0, , drop = FALSE]
  }
  prep
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

#' TRUE si alguna fecha de cohorte cae en el periodo.
#' Brokers: esas fechas son el Gen Registro del Sheets (una por mes).
#' Sin Sheets, es la fecha en que el API creó al embajador.
origenes_en_cohorte <- function(emb, start, end) {
  n <- if (is.null(emb)) 0L else nrow(emb)
  if (!n) {
    return(logical())
  }
  start <- as.Date(start)
  end <- as.Date(end)
  if (!"fechas_cohorte" %in% names(emb)) {
    fecha <- as.Date(emb$fecha_registro)
    return(!is.na(fecha) & fecha >= start & fecha <= end)
  }
  vapply(strsplit(as.character(emb$fechas_cohorte), "|", fixed = TRUE), function(xs) {
    fechas <- as.Date(xs)
    any(!is.na(fechas) & fechas >= start & fechas <= end)
  }, logical(1), USE.NAMES = FALSE)
}

#' Cuenta persona-mes: quien está en dos meses del Sheets cuenta en los dos.
origenes_cohorte_n <- function(emb, start, end) {
  n <- if (is.null(emb)) 0L else nrow(emb)
  if (!n) {
    return(0L)
  }
  start <- as.Date(start)
  end <- as.Date(end)
  if (!"fechas_cohorte" %in% names(emb)) {
    fecha <- as.Date(emb$fecha_registro)
    return(sum(!is.na(fecha) & fecha >= start & fecha <= end))
  }
  sum(vapply(strsplit(as.character(emb$fechas_cohorte), "|", fixed = TRUE), function(xs) {
    fechas <- as.Date(xs)
    sum(!is.na(fechas) & fechas >= start & fechas <= end)
  }, integer(1)))
}

origenes_resumen_en_capacitacion_mask <- function(emb, start, end) {
  if (!nrow(emb) || !"en_capacitacion" %in% names(emb)) {
    return(rep(FALSE, nrow(emb)))
  }
  start <- as.Date(start)
  end <- as.Date(end)
  mes_periodo <- format(start, "%Y-%m")
  has_gen <- dplyr::coalesce(emb$en_capacitacion, FALSE)
  meses <- emb$gen_registro_meses %||% rep(NA_character_, nrow(emb))
  meses <- ifelse(is.na(meses), "", as.character(meses))
  en_mes_sheets <- vapply(
    strsplit(meses, "|", fixed = TRUE),
    function(ms) mes_periodo %in% ms,
    logical(1)
  )
  if (!length(en_mes_sheets)) {
    en_mes_sheets <- rep(FALSE, nrow(emb))
  }
  fecha <- as.Date(emb$fecha_registro)
  # Histórico del Sheets trae el mes (Gen Registro). Un alta nueva en el dash,
  # sin ese mes, cuenta en el mes en que el API creó al embajador.
  en_mes_alta <- !nzchar(meses) & !is.na(fecha) & fecha >= start & fecha <= end
  has_gen & (en_mes_sheets | en_mes_alta)
}

origenes_resumen_en_capacitacion_n <- function(emb, start, end) {
  sum(origenes_resumen_en_capacitacion_mask(emb, start, end), na.rm = TRUE)
}

origenes_resumen_en_capacitacion_rows <- function(emb, start, end) {
  emb[origenes_resumen_en_capacitacion_mask(emb, start, end), , drop = FALSE]
}

#' Tabla Embajadores (Analytics) — reglas por fila
#'
#' Cohorte del periodo = embajadores con Gen Registro / fecha_registro en la columna.
#' Base sin equipo = vacío/no + Pertenece=sí con Generación (capacitaron).
#' Solo se excluye a quien pertenece a un equipo y aún no tiene Generación.
#'
#' 1. Registrados totales
#'    COUNTIF(Gen Registro = periodo), una vez por persona y mes.
#'    Si el Sheets trae ese mes, se usa el suyo. Si no, la fecha del API.
#'
#' 2. Registrados sin equipos
#'    cohorte: Pertenece ≠ "sí", o Pertenece=sí con Generación no vacía
#'    → nrow(base). Las filas 3–5 y 7–11 se calculan sobre esta base.
#'
#' 3–5. Arturo Cole / Eduardo Salazar / Otros
#'    Sobre sin equipos: Canal o referrer_nombre cae en los alias de esa fila
#'    (Salazar incluye "Eduardo Javier Salazar Perez"; Otros incluye Marketing).
#'
#' 6. En capacitación
#'    COUNTIFS(Embajadores!K, periodo, Embajadores!H, "<>")
#'    → Gen Registro del mes y Generación no vacía, con o sin equipo.
#'
#' 7. Cita agendada
#'    COUNTIFS(Gen Registro, M > 0) sobre la base
#'    → M = fecha de última cita agendada/realizada (cualquier status).
#'
#' 8. Primera cita
#'    =COUNTIFS(Embajadores!K:K, periodo, Embajadores!I:I, ">0")
#'    → filas del Sheets (duplicados cuentan); fallback cohort$tiene_primera_cita.
#'    → Gen Registro = periodo y Fecha primera cita realizada no vacía.
#'      (toda la cohorte; no filtra por equipo).
#'
#' 9. Conversión primera cita / registrados
#'    Primera cita / Registrados totales.
#'
#' 10. Activos (últimos N días)
#'    COUNTIFS(Gen Registro, L < 31) sobre la base
#'    → L = as_of − fecha última cita agendada/realizada (col M);
#'      as_of = última actividad del cache.
#'
#' 11. Conversión activo / primera cita
#'    Activos / Primera cita.
origenes_resumen_canales_conversion <- function() {
  c("Arturo Cole", "Eduardo Salazar", "Otros")
}

#' Alias de texto (Canal Sheets / referrer User) → fila de la tabla.
origenes_resumen_canal_aliases <- function() {
  list(
    "Arturo Cole" = c("Arturo Cole"),
    "Eduardo Salazar" = c(
      "Eduardo Salazar",
      "Eduardo Javier Salazar Perez",
      "Eduardo Salazar Yemi Alfie"
    ),
    "Otros" = c(
      "Otros",
      "Marketing",
      "Mauricio Sanchez",
      "Mau Sanchez",
      "Moi Salame",
      "Diego Espinosa"
    )
  )
}

origenes_resumen_canal_fold <- function(x) {
  # Misma idea que origenes_normalize_key: sin acentos, solo [a-z0-9].
  origenes_normalize_key(x)
}

origenes_resumen_canal_match_mask <- function(df, canal_name) {
  if (!nrow(df)) {
    return(logical())
  }
  aliases <- origenes_resumen_canal_aliases()
  target <- origenes_brokers_canal_label(canal_name)
  if (is.na(target) || !nzchar(target) || !target %in% names(aliases)) {
    return(rep(FALSE, nrow(df)))
  }
  want <- unique(na.omit(origenes_resumen_canal_fold(aliases[[target]])))
  hit <- rep(FALSE, nrow(df))
  if ("canal" %in% names(df)) {
    canal_f <- origenes_resumen_canal_fold(origenes_brokers_canal_label(df$canal))
    hit <- hit | (!is.na(canal_f) & canal_f %in% want)
  }
  # Referrer del User (Owner → Referrer_R resuelto a nombre), si ya está en el prep.
  if ("referrer_nombre" %in% names(df)) {
    ref <- origenes_resumen_canal_fold(df$referrer_nombre)
    hit <- hit | (!is.na(ref) & ref %in% want)
  }
  hit
}

origenes_resumen_count_canal <- function(df, canal_name) {
  as.integer(sum(origenes_resumen_canal_match_mask(df, canal_name), na.rm = TRUE))
}

origenes_resumen_tabla_embajadores <- function(prep, start, end, granularity = "mensual") {
  granularity <- match.arg(granularity, origenes_granularity_values())
  periods <- origenes_period_starts(start, end, granularity)
  emb <- prep$embajadores
  active_days <- brokers_active_window_days()
  canales <- origenes_resumen_canales_conversion()

  cols <- lapply(periods, function(ps) {
    pe <- origenes_period_end(ps, granularity)
    # (1) Cohorte por Gen Registro del Sheets, o fecha del API si no está en el Sheets.
    cohort <- emb[origenes_en_cohorte(emb, ps, pe), , drop = FALSE]
    # (2) Base: Pertenece ≠ sí
    base <- if ("es_registrado_sin_equipo" %in% names(cohort)) {
      cohort |> dplyr::filter(.data$es_registrado_sin_equipo)
    } else {
      cohort |>
        dplyr::filter(origenes_brokers_es_registrado_sin_equipo(
          .data$pertenece_equipo,
          if ("generacion" %in% names(cohort)) .data$generacion else NULL
        ))
    }
    # (6) Gen Registro = periodo y Generación <> "". No filtra equipo.
    en_cap <- origenes_resumen_en_capacitacion_n(emb, ps, pe)
    canal_ns <- stats::setNames(
      lapply(canales, function(cn) origenes_resumen_count_canal(base, cn)),
      paste0("canal_", seq_along(canales))
    )
    c(
      list(
        registrados = nrow(cohort),
        sin_equipos = nrow(base)
      ),
      canal_ns,
      list(
        en_capacitacion = as.integer(en_cap),
        # (7) M > 0 sobre sin equipos
        cita_agendada = sum(base$tiene_cita_agendada, na.rm = TRUE),
        # (8) COUNTIFS(Embajadores!K, periodo, Embajadores!I, ">0") — filas Sheets
        primera_cita = {
          n_sheets <- origenes_brokers_sheets_primera_cita_n(format(ps, "%Y-%m"))
          if (!is.na(n_sheets)) {
            as.integer(n_sheets)
          } else {
            as.integer(sum(cohort$tiene_primera_cita, na.rm = TRUE))
          }
        },
        # (10) L < 31 sobre sin equipos
        activos = sum(base$activo_30d, na.rm = TRUE)
      )
    )
  })

  # Totales = suma de columnas de periodo (igual que Sheets).
  total_reg <- sum(vapply(cols, `[[`, integer(1), "registrados"))
  total_sin <- sum(vapply(cols, `[[`, integer(1), "sin_equipos"))
  total_canales <- vapply(
    seq_along(canales),
    function(j) sum(vapply(cols, function(c) as.integer(c[[paste0("canal_", j)]]), integer(1))),
    integer(1)
  )
  total_cap <- sum(vapply(cols, `[[`, integer(1), "en_capacitacion"))
  total_ag <- sum(vapply(cols, `[[`, integer(1), "cita_agendada"))
  total_1ra <- sum(vapply(cols, `[[`, integer(1), "primera_cita"))
  total_activos <- sum(vapply(cols, `[[`, integer(1), "activos"))
  labels <- vapply(periods, origenes_period_label, character(1), granularity = granularity)

  tibble::tibble(
    metrica = c(
      "Registrados totales",
      "Registrados sin equipos",
      canales,
      "En capacitación",
      "Cita agendada",
      "Primera cita",
      "Conversión primera cita / registrados",
      paste0("Activos (últimos ", active_days, " días)"),
      "Conversión activo / primera cita"
    ),
    Total = c(
      as.character(total_reg),
      as.character(total_sin),
      as.character(total_canales),
      as.character(total_cap),
      as.character(total_ag),
      as.character(total_1ra),
      # (9) primera / registrados totales
      brokers_fmt_pct(total_1ra, total_reg),
      as.character(total_activos),
      # (11) activos / primera
      brokers_fmt_pct(total_activos, total_1ra)
    )
  ) |>
    dplyr::bind_cols(
      as.data.frame(
        stats::setNames(
          lapply(seq_along(cols), function(i) {
            c(
              as.character(cols[[i]]$registrados),
              as.character(cols[[i]]$sin_equipos),
              vapply(
                seq_along(canales),
                function(j) as.character(cols[[i]][[paste0("canal_", j)]]),
                character(1)
              ),
              as.character(cols[[i]]$en_capacitacion),
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

#' Tabla Actividad — columnas = mes de actividad (no de registro).
#'
#' 1. Embajadores que trajeron su primer lead
#'    =COUNTIF(Embajadores!N:N, periodo)
#'    N = Gen primera cita realizada
#'
#' 2. Citas Agendadas
#'    First.meeting = TRUE con Start.date en el mes; un lead cuenta una sola vez.
#'
#' 3. Citas nuevas Realizadas
#'    Primera=Sí ∧ Realizada (Start.date en el mes).
#'
#' 4. Total de citas realizadas
#'    Status = Realizada con Start.date en el mes (sin filtro de creación).
#'
#' 5. Unidades Vendidas
#'    ventas Firmado con fecha de firma en el mes; suma de unidades.
#'
#' 6. Facturación
#'    misma base que (5); suma de precio.
origenes_resumen_tabla_resultados <- function(prep, start, end, granularity = "mensual") {
  granularity <- match.arg(granularity, origenes_granularity_values())
  periods <- origenes_period_starts(start, end, granularity)
  meetings <- prep$meetings
  sales <- prep$sales
  emb <- prep$embajadores %||% tibble::tibble()
  # Sheets col N
  if (nrow(emb) && !"gen_primera_cita_realizada" %in% names(emb)) {
    emb$gen_primera_cita_realizada <- origenes_ym_label(emb$fecha_primera_cita)
  }
  gen_n <- if (nrow(emb) && "gen_primera_cita_realizada" %in% names(emb)) {
    stringr::str_squish(as.character(emb$gen_primera_cita_realizada))
  } else {
    character()
  }

  cols <- lapply(seq_along(periods), function(i) {
    ps <- periods[[i]]
    pe <- origenes_period_end(ps, granularity)
    lab <- origenes_period_label(ps, granularity)
    # Corte por mes de actividad: Start.date / fecha_firma
    m <- meetings |>
      dplyr::filter(!is.na(.data$fecha_cita), .data$fecha_cita >= ps, .data$fecha_cita <= pe)
    v <- sales |>
      dplyr::filter(
        .data$es_firmado,
        !is.na(.data$fecha_firma),
        .data$fecha_firma >= ps,
        .data$fecha_firma <= pe
      )
    # (3) Realizada ∧ Created en el mismo mes que Gen Cita
    nuevas <- if ("cita_realizada_conteo" %in% names(m)) {
      m |> dplyr::filter(.data$cita_realizada_conteo)
    } else {
      m |> dplyr::filter(origenes_meeting_is_cita_realizada_conteo(m))
    }
    list(
      # (1) COUNTIF(Embajadores!N:N, lab)
      primera_cita = sum(gen_n == lab, na.rm = TRUE),
      # (2) primeras citas, distinct por lead
      citas_agendadas = origenes_meetings_count_citas_agendadas(m),
      citas_nuevas = nrow(nuevas),
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
      "Embajadores que trajeron su primer lead",
      "Citas Agendadas",
      "Citas nuevas Realizadas",
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

origenes_resumen_fmt_ciclo <- function(total, n) {
  if (!is.finite(n) || n <= 0 || !is.finite(total)) {
    return("—")
  }
  paste0(round(total / n), " d")
}

#' Tercera tabla Brokers: columnas = mes de la primera cita realizada (CRM).
#' La venta se cuenta en esa generación, no en el mes de la firma.
origenes_resumen_tabla_citas_gen <- function(prep, start, end, granularity = "mensual") {
  granularity <- match.arg(granularity, origenes_granularity_values())
  periods <- origenes_period_starts(start, end, granularity)
  meetings <- prep$meetings %||% tibble::tibble()
  sales <- prep$sales_gen %||% prep$sales %||% tibble::tibble()
  if (!"gen_cita_fecha" %in% names(sales)) {
    sales$gen_cita_fecha <- as.Date(NA)
  }

  # Cita Realizada = Realizada ∧ Created.Date en el mismo mes que Gen Cita (Start).
  n_citas <- function(m) {
    if (!nrow(m)) {
      return(0L)
    }
    if ("cita_realizada_conteo" %in% names(m)) {
      return(sum(dplyr::coalesce(m$cita_realizada_conteo, FALSE)))
    }
    sum(origenes_meeting_is_cita_realizada_conteo(m))
  }

  cols <- lapply(periods, function(ps) {
    pe <- origenes_period_end(ps, granularity)
    m <- meetings
    if (nrow(m) && "fecha_cita" %in% names(m)) {
      m <- m[!is.na(m$fecha_cita) & m$fecha_cita >= ps & m$fecha_cita <= pe, , drop = FALSE]
    } else {
      m <- m[0, , drop = FALSE]
    }
    en_gen <- if (nrow(sales) && "gen_cita_fecha" %in% names(sales)) {
      !is.na(sales$gen_cita_fecha) & sales$gen_cita_fecha >= ps & sales$gen_cita_fecha <= pe
    } else {
      rep(FALSE, nrow(sales))
    }
    if (!nrow(sales) || !"es_firmado" %in% names(sales)) {
      firmadas <- sales[0, , drop = FALSE]
      procesos <- firmadas
      ciclo <- numeric()
    } else {
      firmadas <- sales[en_gen & dplyr::coalesce(sales$es_firmado, FALSE), , drop = FALSE]
      procesos <- sales[en_gen & dplyr::coalesce(sales$es_en_proceso, FALSE), , drop = FALSE]
      ciclo <- as.numeric(firmadas$fecha_firma - firmadas$gen_cita_fecha)
      ciclo <- ciclo[is.finite(ciclo)]
    }
    list(
      cita_realizada = n_citas(m),
      procesos = nrow(procesos),
      cierres = nrow(firmadas),
      unidades = sum(firmadas$unidades, na.rm = TRUE),
      facturacion = sum(firmadas$precio, na.rm = TRUE),
      ciclo_sum = sum(ciclo),
      ciclo_n = length(ciclo)
    )
  })

  labels <- vapply(periods, origenes_period_label, character(1), granularity = granularity)
  tot_real <- sum(vapply(cols, `[[`, numeric(1), "cita_realizada"))
  tot_proc <- sum(vapply(cols, `[[`, numeric(1), "procesos"))
  tot_close <- sum(vapply(cols, `[[`, numeric(1), "cierres"))
  tot_units <- sum(vapply(cols, `[[`, numeric(1), "unidades"))
  tot_fact <- sum(vapply(cols, `[[`, numeric(1), "facturacion"))
  tot_ciclo_sum <- sum(vapply(cols, `[[`, numeric(1), "ciclo_sum"))
  tot_ciclo_n <- sum(vapply(cols, `[[`, numeric(1), "ciclo_n"))

  tibble::tibble(
    metrica = c(
      "Cita Realizada",
      "Procesos de venta",
      "Cierres",
      "Conversión cita realizada / venta",
      "Unidades Vendidas",
      "Facturación",
      "Ciclo promedio"
    ),
    Total = c(
      as.character(tot_real),
      as.character(tot_proc),
      as.character(tot_close),
      brokers_fmt_pct(tot_close, tot_real),
      as.character(tot_units),
      brokers_fmt_money(tot_fact),
      origenes_resumen_fmt_ciclo(tot_ciclo_sum, tot_ciclo_n)
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
              brokers_fmt_money(cols[[i]]$facturacion),
              origenes_resumen_fmt_ciclo(cols[[i]]$ciclo_sum, cols[[i]]$ciclo_n)
            )
          }),
          labels
        ),
        check.names = FALSE
      )
    )
}

origenes_resumen_tabla_conversion <- function(prep,
                                               start,
                                               end,
                                               granularity = "mensual",
                                               origin_key = NULL) {
  if (identical(origenes_origin_key(origin_key %||% ""), "broker")) {
    return(origenes_resumen_tabla_citas_gen(prep, start, end, granularity))
  }
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
    realizadas <- if ("cita_realizada_conteo" %in% names(m)) {
      sum(m$cita_realizada_conteo, na.rm = TRUE)
    } else {
      sum(origenes_meeting_is_cita_realizada_conteo(m))
    }
    v <- firmadas |>
      dplyr::filter(!is.na(.data$fecha_firma), .data$fecha_firma >= ps, .data$fecha_firma <= pe)
    p <- procesos |>
      dplyr::filter(!is.na(.data$fecha_firma), .data$fecha_firma >= ps, .data$fecha_firma <= pe)
    list(
      cita_realizada = realizadas,
      procesos = nrow(p),
      cierres = nrow(v),
      citas_realizadas_todas = sum(m$realizada, na.rm = TRUE),
      unidades = sum(v$unidades, na.rm = TRUE),
      facturacion = sum(v$precio, na.rm = TRUE)
    )
  })

  labels <- vapply(periods, origenes_period_label, character(1), granularity = granularity)
  tot_real <- sum(vapply(cols, `[[`, numeric(1), "cita_realizada"))
  tot_proc <- sum(vapply(cols, `[[`, numeric(1), "procesos"))
  tot_close <- sum(vapply(cols, `[[`, numeric(1), "cierres"))
  tot_real_all <- sum(vapply(cols, `[[`, numeric(1), "citas_realizadas_todas"))
  tot_units <- sum(vapply(cols, `[[`, numeric(1), "unidades"))
  tot_fact <- sum(vapply(cols, `[[`, numeric(1), "facturacion"))

  tibble::tibble(
    metrica = c(
      "Cita Realizada",
      "Procesos de venta",
      "Cierres",
      "Conversión cita realizada / venta",
      "Conversión ventas",
      "Unidades Vendidas",
      "Facturación",
      "Ticket promedio"
    ),
    Total = c(
      as.character(tot_real),
      as.character(tot_proc),
      as.character(tot_close),
      brokers_fmt_pct(tot_close, tot_real),
      brokers_fmt_pct(tot_close, tot_real_all),
      as.character(tot_units),
      brokers_fmt_money(tot_fact),
      brokers_fmt_money(if (tot_units > 0) tot_fact / tot_units else NA_real_)
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
              brokers_fmt_pct(cols[[i]]$cierres, cols[[i]]$citas_realizadas_todas),
              as.character(cols[[i]]$unidades),
              brokers_fmt_money(cols[[i]]$facturacion),
              brokers_fmt_money(
                if (cols[[i]]$unidades > 0) cols[[i]]$facturacion / cols[[i]]$unidades else NA_real_
              )
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
  as_of <- as.Date(prep$as_of %||% Sys.Date())

  emb_cohort <- emb
  if (!is.null(start) && !is.null(end)) {
    emb_cohort <- emb[origenes_en_cohorte(emb, start, end), , drop = FALSE]
  }

  ytd <- origenes_ytd_date_range(as_of)
  emb_ytd <- emb[origenes_en_cohorte(emb, ytd$start, ytd$end), , drop = FALSE]

  registrados <- if (!is.null(start) && !is.null(end)) {
    origenes_cohorte_n(emb, start, end)
  } else {
    nrow(emb_cohort)
  }
  registrados_ytd <- origenes_cohorte_n(emb, ytd$start, ytd$end)
  # Métricas de embajadores posteriores a totales: solo sin equipo.
  base_cohort <- if ("es_registrado_sin_equipo" %in% names(emb_cohort)) {
    emb_cohort |> dplyr::filter(.data$es_registrado_sin_equipo)
  } else if ("pertenece_equipo" %in% names(emb_cohort)) {
    emb_cohort |> dplyr::filter(origenes_brokers_es_registrado_sin_equipo(
      .data$pertenece_equipo,
      if ("generacion" %in% names(emb_cohort)) .data$generacion else NULL
    ))
  } else {
    emb_cohort
  }
  base_all <- if ("es_registrado_sin_equipo" %in% names(emb)) {
    emb |> dplyr::filter(.data$es_registrado_sin_equipo)
  } else if ("pertenece_equipo" %in% names(emb)) {
    emb |> dplyr::filter(origenes_brokers_es_registrado_sin_equipo(
      .data$pertenece_equipo,
      if ("generacion" %in% names(emb)) .data$generacion else NULL
    ))
  } else {
    emb
  }
  base_ytd <- if ("es_registrado_sin_equipo" %in% names(emb_ytd)) {
    emb_ytd |> dplyr::filter(.data$es_registrado_sin_equipo)
  } else if ("pertenece_equipo" %in% names(emb_ytd)) {
    emb_ytd |> dplyr::filter(origenes_brokers_es_registrado_sin_equipo(
      .data$pertenece_equipo,
      if ("generacion" %in% names(emb_ytd)) .data$generacion else NULL
    ))
  } else {
    emb_ytd
  }
  sin_equipos <- nrow(base_cohort)
  # Primera cita KPI: COUNTIFS(K, I>0) sobre cohorte (no solo sin equipo).
  primera_cita_emb <- sum(emb_cohort$tiene_primera_cita, na.rm = TRUE)
  activos <- sum(base_all$activo_30d, na.rm = TRUE)
  retencion_n <- sum(base_cohort$tiene_mas_de_una_cita, na.rm = TRUE)
  retencion_ytd_n <- sum(base_ytd$tiene_mas_de_una_cita, na.rm = TRUE)
  citas_nuevas <- if ("cita_realizada_conteo" %in% names(meetings)) {
    sum(meetings$cita_realizada_conteo, na.rm = TRUE)
  } else {
    sum(origenes_meeting_is_cita_realizada_conteo(meetings))
  }
  citas_agendadas <- origenes_meetings_count_citas_agendadas(meetings)
  citas_realizadas <- sum(meetings$realizada, na.rm = TRUE)
  citas_retencion <- sum(!dplyr::coalesce(meetings$primera_cita, FALSE), na.rm = TRUE)
  unidades <- sum(firmadas$unidades, na.rm = TRUE)
  facturacion <- sum(firmadas$precio, na.rm = TRUE)
  cierres <- nrow(firmadas)

  list(
    registrados = registrados,
    sin_equipos = sin_equipos,
    conv_primera_reg = if (registrados > 0) primera_cita_emb / registrados else NA_real_,
    primera_cita_emb = primera_cita_emb,
    activos = activos,
    conv_activo_primera = if (primera_cita_emb > 0) activos / primera_cita_emb else NA_real_,
    tasa_retencion = if (sin_equipos > 0) retencion_n / sin_equipos else NA_real_,
    retencion_n = retencion_n,
    tasa_retencion_ytd = if (nrow(base_ytd) > 0) retencion_ytd_n / nrow(base_ytd) else NA_real_,
    retencion_ytd_n = retencion_ytd_n,
    registrados_ytd = registrados_ytd,
    citas_nuevas = citas_nuevas,
    citas_agendadas = citas_agendadas,
    citas_realizadas = citas_realizadas,
    conv_citas_realizadas = if (citas_agendadas > 0) citas_realizadas / citas_agendadas else NA_real_,
    conv_retencion = if (citas_agendadas > 0) citas_retencion / citas_agendadas else NA_real_,
    citas_retencion = citas_retencion,
    cierres = cierres,
    conv_cita_cierre = if (citas_nuevas > 0) cierres / citas_nuevas else NA_real_,
    conv_ventas = if (citas_realizadas > 0) cierres / citas_realizadas else NA_real_,
    unidades = unidades,
    facturacion = facturacion,
    ticket = if (unidades > 0) facturacion / unidades else NA_real_
  )
}

#' Anual del dash: 1 ene 2026 → hoy.
origenes_ytd_date_range <- function(as_of = Sys.Date()) {
  origenes_default_date_range_for_granularity("anual", as_of = as_of)
}

#' Brokers / dash: corte anual por defecto (ene 2026 → hoy).
origenes_brokers_history_start <- function() {
  as.Date("2026-01-01")
}

#' Rango por defecto según Periodo.
#' - mensual (y demás): últimos 3 meses + mes actual
#' - semanal: últimas 4 semanas + semana actual
#' - anual (todo el dash): 1 ene 2026 → hoy
origenes_default_date_range_for_granularity <- function(granularity = "mensual",
                                                       as_of = Sys.Date(),
                                                       origin_key = NULL) {
  granularity <- origenes_normalize_granularity(granularity)
  as_of <- as.Date(as_of)
  if (identical(granularity, "anual")) {
    start <- origenes_brokers_history_start()
    end <- as_of
    if (length(end) != 1L || is.na(end) || end < start) {
      end <- start
    }
    return(list(start = start, end = end))
  }
  if (identical(granularity, "semanal")) {
    week_start <- as.Date(lubridate::floor_date(as_of, "week", week_start = 1))
    start <- week_start - (4L * 7L)
    end <- as_of
    if (end < start) {
      end <- start
    }
    return(list(start = start, end = end))
  }
  origenes_chart_date_range(as_of, n_months = 4L)
}

#' Rango por defecto del filtro (mensual: 3 meses previos + actual).
origenes_default_date_range <- function(as_of = Sys.Date()) {
  origenes_default_date_range_for_granularity("mensual", as_of)
}

#' Ventana fija para gráficas de Resumen: últimos N meses calendario (incl. mes actual).
#' Ej. hoy 2026-08-05 y N=6 → 2026-03-01 → 2026-08-05.
origenes_chart_date_range <- function(as_of = Sys.Date(), n_months = 6L) {
  as_of <- as.Date(as_of)
  n_months <- max(1L, as.integer(n_months))
  end <- as_of
  month_start <- as.Date(format(as_of, "%Y-%m-01"))
  start <- seq(month_start, by = paste0("-", n_months - 1L, " months"), length.out = 2L)[[2]]
  start <- as.Date(start)
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
                                    granularity = c("mensual", "semanal", "trimestral", "semestral", "anual"),
                                    roster = NULL,
                                    embajador_keys = NULL) {
  granularity <- match.arg(granularity)
  origin_key <- origenes_origin_key(origin_key)
  origin_obj <- joined$origins[[origin_key]]
  if (is.null(origin_obj)) {
    return(NULL)
  }
  # Ancla "hoy" a la última actividad DE ESTE origen (evita meses vacíos).
  as_of <- origenes_origin_activity_as_of(joined, origin_key, as_of = as_of)
  defaults <- origenes_default_date_range_for_granularity(granularity, as_of = as_of)
  start <- as.Date(start %||% defaults$start)
  end <- as.Date(end %||% defaults$end)
  if (is.na(end) || end < start) {
    end <- defaults$end
  }
  # No proyectar columnas más allá de los datos disponibles de este origen.
  if (!is.na(as_of) && end > as_of) {
    end <- as_of
  }
  if (!is.na(start) && start > end) {
    start <- as.Date(format(end, "%Y-01-01"))
    if (start > end) {
      start <- end
    }
  }

  prep <- origenes_resumen_prepare(origin_obj, as_of = as_of, roster = roster)
  if (!is.null(embajador_keys)) {
    prep <- origenes_resumen_filter_personas(prep, embajador_keys)
  }
  prep$sales_gen <- prep$sales

  # Recorta actividad comercial al periodo del Resumen (como el Excel).
  # sales_gen conserva la venta para atribuirla a la generación de la cita.
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

  # Anual: columnas = meses del año en curso (dentro del rango de fechas).
  table_granularity <- if (identical(granularity, "anual")) "mensual" else granularity

  list(
    origin = origin_obj$origin,
    origin_key = origin_key,
    granularity = granularity,
    table_granularity = table_granularity,
    span = list(start = start, end = end),
    kpis = origenes_resumen_kpis(prep, start = start, end = end),
    tabla_embajadores = origenes_resumen_tabla_embajadores(
      prep, start, end, granularity = table_granularity
    ),
    tabla_resultados = origenes_resumen_tabla_resultados(
      prep, start, end, granularity = table_granularity
    ),
    tabla_conversion = origenes_resumen_tabla_conversion(
      prep, start, end, granularity = table_granularity, origin_key = origin_key
    ),
    prep = prep,
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

#' Resuelve bounds de una columna de periodo (o todo el span si es Total).
origenes_resumen_period_bounds <- function(period_col, bundle) {
  gran <- bundle$table_granularity %||% {
    if (identical(bundle$granularity, "anual")) "mensual" else bundle$granularity
  }
  start <- as.Date(bundle$span$start)
  end <- as.Date(bundle$span$end)
  if (is.null(period_col) || !nzchar(period_col) || identical(period_col, "Total")) {
    return(list(start = start, end = end, label = "Total"))
  }
  periods <- origenes_period_starts(start, end, gran)
  labels <- vapply(periods, origenes_period_label, character(1), granularity = gran)
  idx <- match(period_col, labels)
  if (is.na(idx)) {
    return(list(start = start, end = end, label = period_col))
  }
  list(
    start = periods[[idx]],
    end = min(origenes_period_end(periods[[idx]], gran), end),
    label = period_col
  )
}

origenes_resumen_cohort <- function(emb, bounds) {
  emb[origenes_en_cohorte(emb, bounds$start, bounds$end), , drop = FALSE]
}

#' Filas de embajadores que componen una celda de la tabla Embajadores.
origenes_resumen_embajadores_cell_rows <- function(bundle, metric, period_col) {
  emb <- bundle$prep$embajadores %||% tibble::tibble()
  if (!nrow(emb)) {
    return(tibble::tibble())
  }
  bounds <- origenes_resumen_period_bounds(period_col, bundle)
  cohort <- origenes_resumen_cohort(emb, bounds)
  metric <- as.character(metric %||% "")

  # Filas posteriores a "Registrados totales" se basan en sin equipos.
  base <- if ("es_registrado_sin_equipo" %in% names(cohort)) {
    cohort |> dplyr::filter(.data$es_registrado_sin_equipo)
  } else if ("pertenece_equipo" %in% names(cohort)) {
    cohort |> dplyr::filter(origenes_brokers_es_registrado_sin_equipo(
      .data$pertenece_equipo,
      if ("generacion" %in% names(cohort)) .data$generacion else NULL
    ))
  } else {
    cohort
  }

  canal_hit <- origenes_brokers_canal_label(metric)
  canal_names <- origenes_brokers_canal_label(origenes_resumen_canales_conversion())
  is_canal_row <- !is.na(canal_hit) && nzchar(canal_hit) && canal_hit %in% canal_names

  filtered <- if (grepl("^Registrados totales$", metric, ignore.case = TRUE)) {
    cohort
  } else if (grepl("sin equipos", metric, ignore.case = TRUE)) {
    base
  } else if (is_canal_row) {
    base[origenes_resumen_canal_match_mask(base, canal_hit), , drop = FALSE]
  } else if (grepl("capacitaci", metric, ignore.case = TRUE)) {
    origenes_resumen_en_capacitacion_rows(emb, bounds$start, bounds$end)
  } else if (grepl("^Cita agendada$", metric, ignore.case = TRUE)) {
    base |> dplyr::filter(.data$tiene_cita_agendada)
  } else if (grepl("^Primera cita$", metric, ignore.case = TRUE)) {
    # COUNTIFS(K, I>0): cohorte completa, no solo sin equipos
    cohort |> dplyr::filter(.data$tiene_primera_cita)
  } else if (grepl("^Activos", metric, ignore.case = TRUE)) {
    base |> dplyr::filter(.data$activo_30d)
  } else if (grepl("Conversi[oó]n primera", metric, ignore.case = TRUE)) {
    base |> dplyr::filter(.data$tiene_primera_cita)
  } else if (grepl("Conversi[oó]n activo", metric, ignore.case = TRUE)) {
    base |> dplyr::filter(.data$activo_30d)
  } else {
    base
  }

  filtered$fecha_en_periodo <- vapply(seq_len(nrow(filtered)), function(i) {
    fechas <- if ("fechas_cohorte" %in% names(filtered)) {
      as.Date(strsplit(as.character(filtered$fechas_cohorte[i]), "|", fixed = TRUE)[[1]])
    } else {
      as.Date(filtered$fecha_registro[i])
    }
    hit <- fechas[!is.na(fechas) & fechas >= bounds$start & fechas <= bounds$end]
    mostrada <- if (length(hit)) min(hit) else as.Date(filtered$fecha_registro[i])
    if (length(mostrada) != 1L || is.na(mostrada)) "" else format(mostrada, "%Y-%m-%d")
  }, character(1))
  filtered |>
    dplyr::transmute(
      Nombre = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$nombre)), ""),
        as.character(.data$nombre_raw %||% "")
      ),
      `Fecha registro` = .data$fecha_en_periodo,
      `1ª cita` = .data$fecha_primera_cita,
      `Última cita` = .data$fecha_ultima_cita,
      `Días desde realizada` = .data$dias_desde_ultima_realizada %||% NA_integer_,
      `N citas` = .data$n_citas
    ) |>
    dplyr::arrange(.data$Nombre, dplyr::desc(.data$`Fecha registro`))
}

#' Detalle de celda Actividad (citas del periodo de la columna).
origenes_resumen_actividad_cell_rows <- function(bundle, metric, period_col) {
  meetings <- bundle$prep$meetings %||% tibble::tibble()
  sales <- bundle$prep$sales %||% tibble::tibble()
  bounds <- origenes_resumen_period_bounds(period_col, bundle)
  metric <- as.character(metric %||% "")

  m <- meetings |>
    dplyr::filter(
      !is.na(.data$fecha_cita),
      .data$fecha_cita >= bounds$start,
      .data$fecha_cita <= bounds$end
    )
  v <- sales |>
    dplyr::filter(
      dplyr::coalesce(.data$es_firmado, FALSE),
      !is.na(.data$fecha_firma),
      .data$fecha_firma >= bounds$start,
      .data$fecha_firma <= bounds$end
    )

  if (grepl("^Primera cita$|Embajadores que trajeron|Embajadores con Primera", metric, ignore.case = TRUE)) {
    emb <- bundle$prep$embajadores %||% tibble::tibble()
    if (!nrow(emb)) {
      return(tibble::tibble())
    }
    # COUNTIF(Embajadores!N:N, periodo) — no usar `==` con NA (incluye filas fantasma).
    if (!"gen_primera_cita_realizada" %in% names(emb)) {
      emb$gen_primera_cita_realizada <- origenes_ym_label(emb$fecha_primera_cita)
    }
    lab <- bounds$label %||% ""
    gen_n <- stringr::str_squish(as.character(emb$gen_primera_cita_realizada))
    gen_n[is.na(gen_n) | !nzchar(gen_n)] <- ""
    if (!identical(lab, "Total") && nzchar(lab)) {
      emb <- emb[gen_n == lab, , drop = FALSE]
    } else {
      emb <- emb[
        !is.na(emb$fecha_primera_cita) &
          emb$fecha_primera_cita >= bounds$start &
          emb$fecha_primera_cita <= bounds$end,
        ,
        drop = FALSE
      ]
    }
    if (!nrow(emb)) {
      return(tibble::tibble())
    }
    return(
      emb |>
        dplyr::transmute(
          Embajador = dplyr::coalesce(
            dplyr::na_if(stringr::str_squish(as.character(.data$nombre)), ""),
            as.character(.data$nombre_raw %||% "")
          ),
          `Gen 1ª cita` = .data$gen_primera_cita_realizada,
          `1ª cita` = .data$fecha_primera_cita,
          `Última cita` = .data$fecha_ultima_cita,
          `N citas` = .data$n_citas
        ) |>
        dplyr::arrange(.data$Embajador)
    )
  } else if (grepl("Agendadas", metric, ignore.case = TRUE)) {
    # Primeras citas; un lead una vez (primera por fecha si hay varias).
    m <- m |> dplyr::filter(dplyr::coalesce(.data$primera_cita, FALSE))
    if (nrow(m) && "lead_contact_id" %in% names(m)) {
      m <- m[order(m$fecha_cita), , drop = FALSE]
      ids <- as.character(m$lead_contact_id)
      keep <- is.na(ids) | !nzchar(ids) | !duplicated(ids)
      m <- m[keep, , drop = FALSE]
    }
  } else if (grepl("nuevas realizadas", metric, ignore.case = TRUE)) {
    m <- if ("cita_realizada_conteo" %in% names(m)) {
      m |> dplyr::filter(.data$cita_realizada_conteo)
    } else {
      m |> dplyr::filter(origenes_meeting_is_cita_realizada_conteo(m))
    }
  } else if (grepl("realizadas", metric, ignore.case = TRUE)) {
    m <- m |> dplyr::filter(.data$realizada)
  } else if (grepl("Unidades|Facturaci", metric, ignore.case = TRUE)) {
    return(
      v |>
        dplyr::transmute(
          Cliente = .data$cliente %||% NA_character_,
          `Fecha firma` = .data$fecha_firma,
          Embajador = .data$embajador %||% .data$vendedor %||% NA_character_,
          Unidades = .data$unidades,
          Precio = brokers_fmt_money(.data$precio)
        )
    )
  }

  if (!nrow(m)) {
    return(tibble::tibble())
  }
  m |>
    dplyr::transmute(
      Prospecto = .data$lead_nombre %||% NA_character_,
      Fecha = .data$fecha_cita,
      Estatus = .data$status_os %||% NA_character_,
      Embajador = .data$embajador %||% NA_character_,
      `Primera cita` = dplyr::if_else(dplyr::coalesce(.data$primera_cita, FALSE), "si", "no")
    ) |>
    dplyr::arrange(.data$Embajador, dplyr::desc(.data$Fecha), .data$Prospecto)
}

#' Detalle de la tabla Citas por generación (mes de la primera cita realizada).
origenes_resumen_citas_gen_cell_rows <- function(bundle, metric, period_col) {
  meetings <- bundle$prep$meetings %||% tibble::tibble()
  sales <- bundle$prep$sales_gen %||% bundle$prep$sales %||% tibble::tibble()
  bounds <- origenes_resumen_period_bounds(period_col, bundle)
  metric <- as.character(metric %||% "")
  if (!"gen_cita_fecha" %in% names(sales)) {
    sales$gen_cita_fecha <- as.Date(NA)
  }

  m <- meetings
  if (nrow(m) && "fecha_cita" %in% names(m)) {
    m <- m |>
      dplyr::filter(
        !is.na(.data$fecha_cita),
        .data$fecha_cita >= bounds$start,
        .data$fecha_cita <= bounds$end
      )
    m <- if ("cita_realizada_conteo" %in% names(m)) {
      m |> dplyr::filter(.data$cita_realizada_conteo)
    } else {
      m |> dplyr::filter(origenes_meeting_is_cita_realizada_conteo(m))
    }
  }
  v <- sales |>
    dplyr::filter(
      !is.na(.data$gen_cita_fecha),
      .data$gen_cita_fecha >= bounds$start,
      .data$gen_cita_fecha <= bounds$end
    )

  if (grepl("^Cita Realizada$", metric, ignore.case = TRUE)) {
    if (!nrow(m)) {
      return(tibble::tibble())
    }
    if (!"vendedor" %in% names(m)) m$vendedor <- NA_character_
    if (!"lead_nombre" %in% names(m)) m$lead_nombre <- NA_character_
    if (!"embajador" %in% names(m)) m$embajador <- NA_character_
    if (!"fecha_creacion" %in% names(m)) m$fecha_creacion <- as.Date(NA)
    return(
      m |>
        dplyr::transmute(
          Prospecto = .data$lead_nombre %||% NA_character_,
          `Gen cita` = .data$fecha_cita,
          `Fecha creación` = .data$fecha_creacion,
          Embajador = .data$embajador %||% NA_character_,
          Vendedor = .data$vendedor %||% NA_character_
        ) |>
        dplyr::arrange(.data$Embajador, .data$`Gen cita`, .data$Prospecto)
    )
  }

  if (grepl("Procesos", metric, ignore.case = TRUE)) {
    v <- v |> dplyr::filter(dplyr::coalesce(.data$es_en_proceso, FALSE))
  } else if (grepl("Cierres|Unidades|Facturaci|Conversi|Ciclo", metric, ignore.case = TRUE)) {
    v <- v |> dplyr::filter(dplyr::coalesce(.data$es_firmado, FALSE))
  } else {
    return(tibble::tibble())
  }

  if (!nrow(v)) {
    return(tibble::tibble())
  }
  if (!"cliente" %in% names(v)) {
    v$cliente <- if ("cliente_nombre" %in% names(v)) v$cliente_nombre else NA_character_
  }
  if (!"vendedor" %in% names(v)) v$vendedor <- NA_character_
  v |>
    dplyr::transmute(
      Cliente = .data$cliente %||% NA_character_,
      `Gen cita` = .data$gen_cita_fecha,
      `Fecha firma` = .data$fecha_firma,
      `Ciclo (d)` = as.integer(.data$fecha_firma - .data$gen_cita_fecha),
      Vendedor = .data$vendedor %||% NA_character_,
      Unidades = .data$unidades,
      Precio = brokers_fmt_money(.data$precio)
    ) |>
    dplyr::arrange(.data$`Gen cita`, .data$`Fecha firma`)
}

origenes_resumen_cell_drill_show_modal <- function(title, subtitle, tbl, session = NULL) {
  title_txt <- sprintf("Detalle · %s", title)
  if (is.null(tbl) || !is.data.frame(tbl) || !nrow(tbl)) {
    shiny::showModal(shiny::modalDialog(
      title = title_txt,
      shiny::tags$p(class = "or-drill-empty", "Sin filas para esta celda."),
      if (!is.null(subtitle) && nzchar(subtitle)) {
        shiny::tags$p(class = "or-drill-modal-hint", subtitle)
      },
      easyClose = TRUE,
      size = "l",
      footer = shiny::modalButton("Cerrar")
    ))
    return(invisible(NULL))
  }
  shiny::showModal(shiny::modalDialog(
    title = title_txt,
    shiny::tags$p(class = "or-drill-modal-hint", subtitle),
    shiny::tags$p(
      class = "or-drill-modal-meta",
      sprintf("%s fila(s)", format(as.integer(nrow(tbl)), big.mark = ",", scientific = FALSE))
    ),
    htmltools::div(
      class = "or-drill-reactable-wrap",
      style = "max-height:70vh;overflow:auto;",
      origenes_detail_reactable(tbl)
    ),
    easyClose = TRUE,
    size = "l",
    footer = shiny::modalButton("Cerrar")
  ))
  origenes_htmlwidgets_static_render(session)
  invisible(NULL)
}
