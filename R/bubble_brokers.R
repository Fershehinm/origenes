# Cliente Bubble Data API para Orígenes / Brokers.
# Misma autenticación que vendedores_dashboard: BUBBLE_BASE + BUBBLE_TOKEN.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

origenes_bubble_root <- function(api_base_url) {
  u <- sub("/+$", "", api_base_url)
  if (grepl("/obj$", u, ignore.case = TRUE)) {
    sub("/obj$", "", u, ignore.case = TRUE)
  } else {
    u
  }
}

origenes_bubble_credentials <- function() {
  list(
    base = Sys.getenv("BUBBLE_BASE", unset = ""),
    token = Sys.getenv("BUBBLE_TOKEN", unset = "")
  )
}

origenes_bubble_configured <- function() {
  creds <- origenes_bubble_credentials()
  nzchar(creds$base) && nzchar(creds$token)
}

origenes_bubble_cell_chr <- function(v) {
  if (is.null(v)) {
    return(NA_character_)
  }
  if (is.atomic(v) && length(v) == 1L) {
    if (is.na(v)) {
      return(NA_character_)
    }
    return(as.character(v))
  }
  if (is.atomic(v) && length(v) > 1L) {
    return(paste(as.character(v), collapse = ","))
  }
  if (is.list(v) && length(v) == 0L) {
    return(NA_character_)
  }
  if (is.list(v) && length(v) == 1L) {
    return(origenes_bubble_cell_chr(v[[1L]]))
  }
  as.character(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null"))
}

origenes_bubble_records_to_df <- function(records) {
  if (!length(records)) {
    return(tibble::tibble())
  }
  df <- dplyr::bind_rows(lapply(records, function(r) {
    if (!is.list(r)) {
      return(tibble::tibble())
    }
    tibble::as_tibble(
      lapply(r, origenes_bubble_cell_chr),
      .name_repair = "unique"
    )
  }))
  names(df) <- make.names(names(df), unique = TRUE)
  df
}

origenes_bubble_extract_records <- function(body) {
  if (!is.null(body$response$results) && is.list(body$response$results)) {
    return(body$response$results)
  }
  if (!is.null(body$results) && is.list(body$results)) {
    return(body$results)
  }
  if (!is.null(body$data) && is.list(body$data)) {
    return(body$data)
  }
  list()
}

origenes_bubble_extract_remaining <- function(body) {
  if (!is.null(body$response$remaining)) {
    return(as.integer(body$response$remaining))
  }
  if (!is.null(body$remaining)) {
    return(as.integer(body$remaining))
  }
  NA_integer_
}

origenes_bubble_get <- function(api_base, token, endpoint, limit = 100L, cursor = 0L) {
  root <- origenes_bubble_root(api_base)
  url <- paste0(root, "/obj/", endpoint)
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_url_query(limit = limit, cursor = cursor) |>
      httr2::req_headers(
        Authorization = paste("Bearer", token),
        Accept = "application/json"
      ) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_timeout(seconds = 120) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) {
    return(list(status = NA_integer_, body = list()))
  }
  body <- tryCatch(
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE),
    error = function(e) list()
  )
  list(status = httr2::resp_status(resp), body = body)
}

origenes_bubble_fetch_endpoint <- function(endpoint,
                                           api_base,
                                           token,
                                           page_size = 100L,
                                           max_pages = 200L) {
  all <- list()
  cursor <- 0L
  pages <- 0L
  repeat {
    if (pages >= max_pages) {
      warning("max_pages alcanzado: ", endpoint, call. = FALSE)
      break
    }
    res <- origenes_bubble_get(api_base, token, endpoint, limit = page_size, cursor = cursor)
    recs <- origenes_bubble_extract_records(res$body)
    rem <- origenes_bubble_extract_remaining(res$body)
    if (!length(recs)) {
      break
    }
    all <- c(all, recs)
    pages <- pages + 1L
    n <- length(recs)
    if (!is.na(rem) && rem <= 0L) {
      break
    }
    if (n < page_size) {
      break
    }
    cursor <- cursor + n
  }
  origenes_bubble_records_to_df(all)
}

#' Descarga meetings + sales desde Bubble (misma API que vendedores).
origenes_bubble_fetch_bundle <- function(page_size = 100L, max_pages = 200L) {
  creds <- origenes_bubble_credentials()
  if (!nzchar(creds$base) || !nzchar(creds$token)) {
    stop("Define BUBBLE_BASE y BUBBLE_TOKEN en .Renviron", call. = FALSE)
  }
  list(
    meetings_raw = origenes_bubble_fetch_endpoint(
      "meeting", creds$base, creds$token, page_size, max_pages
    ),
    sales_raw = origenes_bubble_fetch_endpoint(
      "sale", creds$base, creds$token, page_size, max_pages
    ),
    fetched_at = Sys.time(),
    source = "bubble_live"
  )
}

origenes_pick_col <- function(df, candidates) {
  if (is.null(df) || !nrow(df)) {
    return(rep(NA_character_, if (is.null(df)) 0L else nrow(df)))
  }
  nms <- names(df)
  for (cand in candidates) {
    idx <- which(tolower(nms) == tolower(cand))
    if (length(idx)) {
      return(as.character(df[[nms[[idx[[1L]]]]]]))
    }
  }
  rep(NA_character_, nrow(df))
}

origenes_is_broker_origin <- function(x) {
  key <- origenes_normalize_key(x)
  key %in% c("broker", "brokers")
}

#' Construye el canónico Brokers desde un bundle Bubble (raw o cache vendedores).
brokers_canonical_from_bubble <- function(bundle, as_of = Sys.Date()) {
  meetings <- bundle$meetings_raw %||% bundle$meetings %||% tibble::tibble()
  sales <- bundle$sales_raw %||% tibble::tibble()
  sales_clean <- bundle$sales_clean %||% NULL

  if (nrow(meetings) && "Origin_OS" %in% names(meetings)) {
    meetings <- meetings[origenes_is_broker_origin(meetings$Origin_OS), , drop = FALSE]
  } else if (nrow(meetings) && "origen" %in% names(meetings)) {
    meetings <- meetings[origenes_is_broker_origin(meetings$origen), , drop = FALSE]
  }

  # --- Citas ---
  if (nrow(meetings)) {
    fecha_inicio <- brokers_parse_datetime(origenes_pick_col(
      meetings, c("Start.date", "Start_date", "fecha", "Created.Date")
    ))
    estatus <- stringr::str_squish(origenes_pick_col(
      meetings, c("Status_OS", "Status", "estatus")
    ))
    vendedor <- stringr::str_squish(origenes_pick_col(
      meetings, c("Seller.full.name", "Seller", "vendedor")
    ))
    embajador <- stringr::str_squish(origenes_pick_col(
      meetings, c("Ambassador", "Embajador", "embajador")
    ))
    primera_raw <- tolower(stringr::str_squish(origenes_pick_col(
      meetings, c("First.meeting", "First_meeting", "primera_cita")
    )))
    prospecto <- stringr::str_squish(origenes_pick_col(
      meetings, c("Lead.full.name", "Lead_full_name", "prospecto", "cliente")
    ))
    fecha_creacion <- brokers_parse_date(origenes_pick_col(
      meetings, c("Created.Date", "Created_Date", "fecha_creacion")
    ))

    flags_key <- origenes_normalize_key(estatus)
    citas <- tibble::tibble(
      cita_id = paste0("bubble_cita_", dplyr::coalesce(
        origenes_pick_col(meetings, c("unique.id", "_id", "id")),
        as.character(seq_len(nrow(meetings)))
      )),
      prospecto = dplyr::na_if(prospecto, ""),
      prospecto_key = brokers_norm_name(prospecto),
      fecha_inicio = fecha_inicio,
      fecha_cita = as.Date(fecha_inicio),
      fecha_creacion = fecha_creacion,
      gen_cita = brokers_parse_gen(as.Date(fecha_inicio)),
      mes_incompleto = brokers_month_incomplete_flag(as.Date(fecha_inicio), as_of = as_of),
      dia_semana = dplyr::case_when(
        is.na(as.Date(fecha_inicio)) ~ NA_character_,
        lubridate::wday(as.Date(fecha_inicio), week_start = 1) == 1 ~ "lunes",
        lubridate::wday(as.Date(fecha_inicio), week_start = 1) == 2 ~ "martes",
        lubridate::wday(as.Date(fecha_inicio), week_start = 1) == 3 ~ "miercoles",
        lubridate::wday(as.Date(fecha_inicio), week_start = 1) == 4 ~ "jueves",
        lubridate::wday(as.Date(fecha_inicio), week_start = 1) == 5 ~ "viernes",
        lubridate::wday(as.Date(fecha_inicio), week_start = 1) == 6 ~ "sabado",
        TRUE ~ "domingo"
      ),
      es_fin_semana = lubridate::wday(as.Date(fecha_inicio), week_start = 1) >= 6,
      estatus = dplyr::na_if(estatus, ""),
      realizada = flags_key == "realizada",
      cancelada = flags_key == "cancelada",
      agendada_pendiente = flags_key == "agendada",
      primera_cita = primera_raw %in% c("true", "si", "sí", "yes", "1"),
      vendedor = dplyr::na_if(vendedor, ""),
      vendedor_rol = brokers_vendedor_rol(vendedor),
      embajador_raw = dplyr::na_if(embajador, ""),
      embajador = dplyr::na_if(embajador, ""),
      embajador_key = brokers_norm_name(embajador),
      embajador_registrado_raw = dplyr::na_if(embajador, ""),
      fuente = "bubble_meeting"
    )
  } else {
    citas <- brokers_empty_citas()
  }

  # --- Ventas ---
  if (!is.null(sales_clean) && nrow(sales_clean)) {
    sc <- sales_clean
    if ("origen" %in% names(sc)) {
      sc <- sc[origenes_is_broker_origin(sc$origen), , drop = FALSE]
    }
    estatus_raw <- if ("estado_venta" %in% names(sc)) {
      stringr::str_squish(as.character(sc$estado_venta))
    } else {
      rep(NA_character_, nrow(sc))
    }
    estatus_key <- origenes_normalize_key(estatus_raw)
    es_firmado <- grepl("firmado", estatus_key)
    es_en_proceso <- grepl("apartado|carta|documentos|intencion|proceso", estatus_key) & !es_firmado
    precio <- if ("monto_facturacion" %in% names(sc)) {
      suppressWarnings(as.numeric(sc$monto_facturacion))
    } else if ("monto" %in% names(sc)) {
      suppressWarnings(as.numeric(sc$monto))
    } else {
      rep(NA_real_, nrow(sc))
    }
    cliente <- if ("cliente_nombre" %in% names(sc)) {
      stringr::str_squish(as.character(sc$cliente_nombre))
    } else {
      rep(NA_character_, nrow(sc))
    }
    vendedor <- if ("vendedor" %in% names(sc)) {
      stringr::str_squish(as.character(sc$vendedor))
    } else {
      rep(NA_character_, nrow(sc))
    }
    proyecto <- if ("proyectos" %in% names(sc)) {
      stringr::str_squish(as.character(sc$proyectos))
    } else {
      rep(NA_character_, nrow(sc))
    }
    recompra_raw <- if ("repurchase" %in% names(sc)) {
      tolower(as.character(sc$repurchase))
    } else {
      rep("", nrow(sc))
    }
    vid <- if ("id" %in% names(sc)) as.character(sc$id) else as.character(seq_len(nrow(sc)))
    fecha_firma <- brokers_parse_date(sc$fecha)
    ventas <- tibble::tibble(
      venta_id = paste0("bubble_venta_", vid),
      fecha_firma = fecha_firma,
      gen_venta = brokers_parse_gen(fecha_firma),
      mes_incompleto = brokers_month_incomplete_flag(fecha_firma, as_of = as_of),
      cliente = dplyr::na_if(cliente, ""),
      cliente_key = brokers_norm_name(cliente),
      proyecto = dplyr::na_if(proyecto, ""),
      id_propiedad = NA_character_,
      vendedor = dplyr::na_if(vendedor, ""),
      vendedor_rol = brokers_vendedor_rol(vendedor),
      estatus_raw = dplyr::na_if(estatus_raw, ""),
      es_firmado = es_firmado,
      es_en_proceso = es_en_proceso,
      cuenta_conversion = es_firmado,
      cuenta_proyeccion = es_firmado | es_en_proceso,
      precio = precio,
      m2 = NA_real_,
      intencion = NA_character_,
      recompra = recompra_raw %in% c("true", "si", "sí", "yes", "1"),
      conciliacion_lead = NA_character_,
      fecha_primera_cita = as.Date(NA),
      gen_cita = NA_character_,
      ciclo_venta = NA_real_,
      match_cita_id = NA_character_,
      match_confianza = "bubble_periodo"
    )
  } else if (nrow(sales)) {
    if ("Origin_OS" %in% names(sales)) {
      sales <- sales[origenes_is_broker_origin(sales$Origin_OS), , drop = FALSE]
    }
    fecha_firma <- brokers_parse_date(origenes_pick_col(
      sales, c("Date.of.sale", "fecha", "Closed.date", "Created.Date")
    ))
    estatus_raw <- stringr::str_squish(origenes_pick_col(
      sales, c("Status_OS", "Status", "estado_venta")
    ))
    estatus_key <- origenes_normalize_key(estatus_raw)
    es_firmado <- grepl("firmado", estatus_key)
    es_en_proceso <- grepl("apartado|carta|documentos|intencion|proceso", estatus_key) & !es_firmado
    precio <- brokers_parse_money(origenes_pick_col(
      sales, c("Purchase.price", "Sale.price", "monto", "Amount", "Price")
    ))
    ventas <- tibble::tibble(
      venta_id = paste0("bubble_venta_", dplyr::coalesce(
        origenes_pick_col(sales, c("unique.id", "_id", "id")),
        as.character(seq_len(nrow(sales)))
      )),
      fecha_firma = fecha_firma,
      gen_venta = brokers_parse_gen(fecha_firma),
      mes_incompleto = brokers_month_incomplete_flag(fecha_firma, as_of = as_of),
      cliente = dplyr::na_if(stringr::str_squish(origenes_pick_col(
        sales, c("Client.full.name", "Client", "cliente")
      )), ""),
      cliente_key = brokers_norm_name(origenes_pick_col(
        sales, c("Client.full.name", "Client", "cliente")
      )),
      proyecto = dplyr::na_if(stringr::str_squish(origenes_pick_col(
        sales, c("Project_OSL", "Project_OS", "proyecto")
      )), ""),
      id_propiedad = NA_character_,
      vendedor = dplyr::na_if(stringr::str_squish(origenes_pick_col(
        sales, c("Seller.full.name", "Seller", "vendedor")
      )), ""),
      vendedor_rol = brokers_vendedor_rol(origenes_pick_col(
        sales, c("Seller.full.name", "Seller", "vendedor")
      )),
      estatus_raw = dplyr::na_if(estatus_raw, ""),
      es_firmado = es_firmado,
      es_en_proceso = es_en_proceso,
      cuenta_conversion = es_firmado,
      cuenta_proyeccion = es_firmado | es_en_proceso,
      precio = precio,
      m2 = NA_real_,
      intencion = NA_character_,
      recompra = tolower(origenes_pick_col(sales, c("Repurchase", "Repurchase."))) %in%
        c("true", "si", "sí", "yes", "1"),
      conciliacion_lead = NA_character_,
      fecha_primera_cita = as.Date(NA),
      gen_cita = NA_character_,
      ciclo_venta = NA_real_,
      match_cita_id = NA_character_,
      match_confianza = "bubble_periodo"
    )
  } else {
    ventas <- brokers_empty_ventas()
  }

  # --- Embajadores (proxy: primera cita Bubble como registro) ---
  if (nrow(citas) && any(!is.na(citas$embajador) & nzchar(citas$embajador))) {
    embajadores <- citas |>
      dplyr::filter(!is.na(.data$embajador), nzchar(.data$embajador)) |>
      dplyr::group_by(.data$embajador_key, .data$embajador) |>
      dplyr::summarise(
        fecha_registro = suppressWarnings(min(.data$fecha_cita, na.rm = TRUE)),
        fecha_primera_cita = {
          r <- .data$fecha_cita[.data$realizada]
          if (!length(r) || all(is.na(r))) as.Date(NA) else min(r, na.rm = TRUE)
        },
        fecha_ultima_cita_realizada = {
          r <- .data$fecha_cita[.data$realizada]
          if (!length(r) || all(is.na(r))) as.Date(NA) else max(r, na.rm = TRUE)
        },
        fecha_ultima_cita_cualquier = suppressWarnings(max(.data$fecha_cita, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        embajador_id = paste0("emb_", dplyr::row_number(), "_", substr(.data$embajador_key, 1, 20)),
        nombre_raw = .data$embajador,
        gen_registro = brokers_parse_gen(.data$fecha_registro),
        generacion = NA_character_,
        fecha_graduacion = as.Date(NA),
        graduado = FALSE,
        dias_desde_ultima_realizada = as.numeric(as_of - .data$fecha_ultima_cita_realizada),
        ciclo_registro_primera_cita = as.numeric(.data$fecha_primera_cita - .data$fecha_registro),
        tiene_primera_cita = !is.na(.data$fecha_primera_cita),
        activo_30d = !is.na(.data$fecha_ultima_cita_cualquier) &
          .data$fecha_ultima_cita_cualquier >= (as_of - brokers_active_window_days())
      ) |>
      dplyr::select(
        "embajador_id", "nombre_raw", "embajador", "embajador_key", "fecha_registro",
        "gen_registro", "generacion", "fecha_graduacion", "graduado",
        "fecha_primera_cita", "fecha_ultima_cita_realizada", "fecha_ultima_cita_cualquier",
        "dias_desde_ultima_realizada", "ciclo_registro_primera_cita",
        "tiene_primera_cita", "activo_30d"
      )
  } else {
    embajadores <- brokers_empty_embajadores()
  }

  list(
    citas = citas,
    embajadores = embajadores,
    ventas = ventas,
    name_map = tibble::tibble(
      nombre_key = character(),
      embajador = character(),
      fuente_mapa = character(),
      embajador_key = character()
    ),
    quality = brokers_quality_report(citas, embajadores, ventas, tibble::tibble()),
    meta = list(
      built_at = Sys.time(),
      as_of = as_of,
      source = bundle$source %||% "bubble",
      definitions = list(
        origen_filtro = "Origin_OS = Broker",
        activos = paste0("cita últimos ", brokers_active_window_days(), " días"),
        embajadores = "derivados del campo Ambassador en meetings Broker"
      )
    )
  )
}
