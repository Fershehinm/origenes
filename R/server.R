origenes_money <- function(x, compact = FALSE) {
  if (!length(x) || !is.finite(x)) {
    return("—")
  }
  if (compact) {
    return(scales::label_number(
      prefix = "$",
      scale_cut = scales::cut_short_scale(),
      accuracy = 0.1
    )(x))
  }
  scales::label_dollar(prefix = "$", big.mark = ",", accuracy = 1)(x)
}

origenes_period_header_ui <- function(label, span_label, range_label) {
  shiny::div(
    class = "or-resumen__period",
    shiny::span(class = "or-resumen__period-label", label),
    shiny::span(class = "or-resumen__period-value", span_label),
    shiny::span(class = "or-resumen__period-range", range_label)
  )
}

origenes_resumen_header_ui <- function(bundle, prefix) {
  if (is.null(bundle)) {
    return(origenes_empty_ui())
  }
  k <- bundle$kpis
  active_days <- tryCatch(brokers_active_window_days(), error = function(e) 30L)
  shiny::div(
    class = "or-resumen",
    origenes_resumen_section(
      "01",
      "Embajadores",
      origenes_resumen_kpi(
        "Registrados totales",
        nxtgen_fmt_num(k$registrados),
        paste0(nxtgen_fmt_pct(k$conv_primera_reg), " con 1ª cita"),
        accent = TRUE,
        id = paste0(prefix, "_kpi_resumen_registrados")
      ),
      origenes_resumen_kpi(
        paste0("Activos (", active_days, " días)"),
        nxtgen_fmt_num(k$activos),
        id = paste0(prefix, "_kpi_resumen_activos")
      ),
      origenes_resumen_kpi(
        "Tasa de retención",
        nxtgen_fmt_pct(k$tasa_retencion),
        id = paste0(prefix, "_kpi_resumen_retencion")
      )
    ),
    origenes_resumen_section(
      "02",
      "Citas",
      origenes_resumen_kpi(
        "Agendadas",
        nxtgen_fmt_num(k$citas_agendadas),
        paste0(nxtgen_fmt_num(k$citas_nuevas), " primeras realizadas"),
        id = paste0(prefix, "_kpi_resumen_agendadas")
      ),
      origenes_resumen_kpi(
        "Realizadas",
        nxtgen_fmt_num(k$citas_realizadas),
        paste0(nxtgen_fmt_pct(k$conv_citas_realizadas), " de agendadas"),
        id = paste0(prefix, "_kpi_resumen_realizadas")
      ),
      origenes_resumen_kpi(
        "Conv. retención",
        nxtgen_fmt_pct(k$conv_retencion),
        id = paste0(prefix, "_kpi_resumen_conv_retencion")
      )
    ),
    origenes_resumen_section(
      "03",
      "Ventas",
      origenes_resumen_kpi(
        "Cierres",
        nxtgen_fmt_num(k$cierres),
        paste0(nxtgen_fmt_pct(k$conv_ventas), " de realizadas"),
        id = paste0(prefix, "_kpi_resumen_cierres")
      ),
      origenes_resumen_kpi(
        "Unidades",
        nxtgen_fmt_num(k$unidades),
        id = paste0(prefix, "_kpi_resumen_unidades")
      ),
      origenes_resumen_kpi(
        "Facturación",
        origenes_money(k$facturacion, compact = TRUE),
        accent = TRUE,
        id = paste0(prefix, "_kpi_resumen_facturacion")
      ),
      origenes_resumen_kpi(
        "Ticket promedio",
        origenes_money(k$ticket, compact = TRUE),
        id = paste0(prefix, "_kpi_resumen_ticket")
      )
    )
  )
}

#' Cablea insights + drills (modales) + tablas de un origen (nxtgen | brokers).
#' Las tablas de abajo siempre reflejan solo el filtro de fechas; el detalle
#' de KPIs/gráficas se abre en modal.
origenes_bind_origin_server <- function(prefix,
                                        input,
                                        output,
                                        session,
                                        resumen_reactive,
                                        resumen_charts_reactive = NULL,
                                        citas_reactive,
                                        citas_charts_reactive = NULL,
                                        ventas_reactive,
                                        ventas_charts_reactive = NULL,
                                        date_range_reactive,
                                        include_postgrad = FALSE) {
  channel_title <- if (identical(prefix, "brokers")) "Brokers" else "NxtGen"
  # Gráficas con ventana fija (últimos 6 meses); tablas/KPIs usan el filtro de fechas.
  if (is.null(resumen_charts_reactive)) {
    resumen_charts_reactive <- resumen_reactive
  }
  if (is.null(citas_charts_reactive)) {
    citas_charts_reactive <- citas_reactive
  }
  if (is.null(ventas_charts_reactive)) {
    ventas_charts_reactive <- ventas_reactive
  }

  # --- KPIs (sin franja de fechas) ----------------------------------------
  output[[paste0(prefix, "_citas_kpis")]] <- shiny::renderUI({
    origenes_citas_kpis_ui(origenes_citas_kpis(citas_reactive()), prefix)
  })
  output[[paste0(prefix, "_ventas_kpis")]] <- shiny::renderUI({
    origenes_ventas_kpis_ui(origenes_ventas_kpis(ventas_reactive()), prefix)
  })

  # --- Citas charts --------------------------------------------------------
  src_est <- paste0(prefix, "_citas_estatus")
  src_emb <- paste0(prefix, "_citas_emb")

  output[[paste0(prefix, "_citas_chart_estatus")]] <- plotly::renderPlotly({
    # Gen Cita: últimos 6 meses (no el filtro corto de tablas).
    origenes_citas_chart_estatus(citas_charts_reactive(), source = src_est)
  })
  output[[paste0(prefix, "_citas_chart_emb")]] <- plotly::renderPlotly({
    origenes_citas_chart_embajadores(citas_reactive(), source = src_emb)
  })
  output[[paste0(prefix, "_citas_primera_progress")]] <- shiny::renderUI({
    origenes_citas_primera_progress_ui(citas_reactive(), prefix)
  })
  if (include_postgrad) {
    output[[paste0(prefix, "_citas_postgrad_progress")]] <- shiny::renderUI({
      origenes_citas_postgrad_progress_ui(citas_reactive(), prefix)
    })
  }

  shiny::observeEvent(plotly::event_data("plotly_click", source = src_est), {
    ed <- origenes_parse_click_key(plotly::event_data("plotly_click", source = src_est))
    if (is.null(ed)) {
      return()
    }
    gen <- ed$x
    est <- ed$key
    rules <- list()
    bits <- character()
    if (!is.na(gen) && nzchar(gen)) {
      rules <- c(rules, list(list(field = "Gen Cita", value = gen)))
      bits <- c(bits, paste("Gen", gen))
    }
    if (!is.na(est) && nzchar(est)) {
      rules <- c(rules, list(list(field = "Estatus", value = est)))
      bits <- c(bits, est)
    }
    origenes_citas_open_drill(
      citas_charts_reactive(),
      rules,
      paste(bits, collapse = " · "),
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(plotly::event_data("plotly_click", source = src_emb), {
    ed <- origenes_parse_click_key(plotly::event_data("plotly_click", source = src_emb))
    if (is.null(ed)) {
      return()
    }
    emb <- ed$key
    if (is.na(emb) || !nzchar(emb)) {
      emb <- ed$y
    }
    field <- if ("Embajador Registrado" %in% names(citas_reactive())) {
      "Embajador Registrado"
    } else {
      "Embajador"
    }
    origenes_citas_open_drill(
      citas_reactive(),
      list(list(field = field, value = emb)),
      paste("Embajador:", emb),
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_primera_si")]], {
    origenes_citas_open_drill(
      citas_reactive(),
      list(list(field = "Primera Cita", value = "si")),
      "Primera cita: Sí",
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_primera_no")]], {
    origenes_citas_open_drill(
      citas_reactive(),
      list(list(field = "Primera Cita", value = "no")),
      "Primera cita: No",
      session = session
    )
  }, ignoreInit = TRUE)

  if (include_postgrad) {
    shiny::observeEvent(input[[paste0(prefix, "_postgrad_si")]], {
      origenes_citas_open_drill(
        citas_reactive(),
        list(list(field = "¿Fue Post Graduación?", value = "si")),
        "Post graduación: Sí",
      session = session
    )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input[[paste0(prefix, "_postgrad_no")]], {
      origenes_citas_open_drill(
        citas_reactive(),
        list(list(field = "¿Fue Post Graduación?", value = "no")),
        "Post graduación: No",
      session = session
    )
    }, ignoreInit = TRUE)
  }

  # --- Citas KPI card drills -----------------------------------------------
  shiny::observeEvent(input[[paste0(prefix, "_kpi_citas_total")]], {
    origenes_citas_open_drill(citas_reactive(), list(), "Todas las citas", session = session)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_citas_realizadas")]], {
    origenes_citas_open_drill(
      citas_reactive(),
      list(list(field = "Estatus", value = "Realizada")),
      "Estatus: Realizada",
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_citas_primera")]], {
    origenes_citas_open_drill(
      citas_reactive(),
      list(list(field = "Primera Cita", value = "si")),
      "Primera cita: Sí",
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_citas_top_emb")]], {
    k <- origenes_citas_kpis(citas_reactive())
    field <- k$emb_col %||% "Embajador Registrado"
    origenes_citas_open_drill(
      citas_reactive(),
      list(list(field = field, value = k$top_embajador)),
      paste("Embajador:", k$top_embajador),
      session = session
    )
  }, ignoreInit = TRUE)

  if (include_postgrad) {
    shiny::observeEvent(input[[paste0(prefix, "_kpi_citas_postgrad")]], {
      origenes_citas_open_drill(
        citas_reactive(),
        list(list(field = "¿Fue Post Graduación?", value = "si")),
        "Post graduación: Sí",
      session = session
    )
    }, ignoreInit = TRUE)
  }

  # --- Ventas charts -------------------------------------------------------
  src_proy <- paste0(prefix, "_ventas_proyecto")
  src_gen <- paste0(prefix, "_ventas_gen")
  src_ciclo <- paste0(prefix, "_ventas_ciclo")
  src_vend <- paste0(prefix, "_ventas_vendedor")

  output[[paste0(prefix, "_ventas_chart_proyecto")]] <- plotly::renderPlotly({
    origenes_ventas_chart_proyecto(ventas_reactive(), source = src_proy)
  })
  output[[paste0(prefix, "_ventas_chart_gen")]] <- plotly::renderPlotly({
    # Gen Venta: últimos 6 meses (no el filtro corto de tablas).
    origenes_ventas_chart_gen(ventas_charts_reactive(), source = src_gen)
  })
  output[[paste0(prefix, "_ventas_chart_ciclo")]] <- plotly::renderPlotly({
    origenes_ventas_chart_ciclo(ventas_reactive(), source = src_ciclo)
  })
  output[[paste0(prefix, "_ventas_chart_vendedor")]] <- plotly::renderPlotly({
    origenes_ventas_chart_vendedor(ventas_reactive(), source = src_vend)
  })

  shiny::observeEvent(plotly::event_data("plotly_click", source = src_proy), {
    ed <- origenes_parse_click_key(plotly::event_data("plotly_click", source = src_proy))
    if (is.null(ed)) {
      return()
    }
    val <- ed$key
    if (is.na(val) || !nzchar(val)) {
      val <- ed$y
    }
    origenes_ventas_open_drill(
      ventas_reactive(),
      list(list(field = "Proyecto", value = val)),
      paste("Proyecto:", val),
      channel_title,
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(plotly::event_data("plotly_click", source = src_gen), {
    ed <- origenes_parse_click_key(plotly::event_data("plotly_click", source = src_gen))
    if (is.null(ed)) {
      return()
    }
    val <- ed$key
    if (is.na(val) || !nzchar(val)) {
      val <- ed$x
    }
    origenes_ventas_open_drill(
      ventas_charts_reactive(),
      list(list(field = "Gen Venta", value = val)),
      paste("Gen Venta:", val),
      channel_title,
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(plotly::event_data("plotly_click", source = src_ciclo), {
    ed <- origenes_parse_click_key(plotly::event_data("plotly_click", source = src_ciclo))
    if (is.null(ed)) {
      return()
    }
    val <- ed$key
    if (is.na(val) || !nzchar(val)) {
      val <- ed$x
    }
    origenes_ventas_open_drill(
      ventas_reactive(),
      list(list(field = ".ciclo_bucket", value = val)),
      paste("Ciclo:", val),
      channel_title,
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(plotly::event_data("plotly_click", source = src_vend), {
    ed <- origenes_parse_click_key(plotly::event_data("plotly_click", source = src_vend))
    if (is.null(ed)) {
      return()
    }
    val <- ed$key
    if (is.na(val) || !nzchar(val)) {
      val <- ed$y
    }
    origenes_ventas_open_drill(
      ventas_reactive(),
      list(list(field = "Vendedor", value = val)),
      paste("Vendedor:", val),
      channel_title,
      session = session
    )
  }, ignoreInit = TRUE)

  # --- Ventas KPI card drills ----------------------------------------------
  shiny::observeEvent(input[[paste0(prefix, "_kpi_ventas_ops")]], {
    origenes_ventas_open_drill(ventas_reactive(), list(), "Operaciones", channel_title, session = session)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input[[paste0(prefix, "_kpi_ventas_fact")]], {
    origenes_ventas_open_drill(ventas_reactive(), list(), "Facturación", channel_title, session = session)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input[[paste0(prefix, "_kpi_ventas_ticket")]], {
    origenes_ventas_open_drill(ventas_reactive(), list(), "Ticket", channel_title, session = session)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input[[paste0(prefix, "_kpi_ventas_ciclo")]], {
    k <- origenes_ventas_kpis(ventas_reactive())
    if (!is.finite(k$ciclo_med)) {
      return()
    }
    med <- k$ciclo_med
    bucket <- dplyr::case_when(
      med <= 7 ~ "0-7 d",
      med <= 14 ~ "8-14 d",
      med <= 30 ~ "15-30 d",
      med <= 60 ~ "31-60 d",
      med <= 90 ~ "61-90 d",
      TRUE ~ "90+ d"
    )
    origenes_ventas_open_drill(
      ventas_reactive(),
      list(list(field = ".ciclo_bucket", value = bucket)),
      paste("Ciclo:", bucket),
      channel_title,
      session = session
    )
  }, ignoreInit = TRUE)

  # --- Resumen charts (solo vista anual: meses del año en curso) -----------
  series <- shiny::reactive({
    origenes_resumen_series_from_tables(resumen_charts_reactive())
  })

  src_series <- paste0(prefix, "_resumen_series")
  src_conv <- paste0(prefix, "_resumen_conv")

  output[[paste0(prefix, "_resumen_chart_series")]] <- plotly::renderPlotly({
    metrics <- input[[paste0(prefix, "_resumen_series_metrics")]]
    if (is.null(metrics) || !length(metrics)) {
      metrics <- origenes_resumen_series_defaults()
    }
    origenes_resumen_chart_series(
      series(),
      source = src_series,
      metrics = metrics
    )
  })
  output[[paste0(prefix, "_resumen_chart_conv")]] <- plotly::renderPlotly({
    origenes_resumen_chart_conversiones(series(), source = src_conv)
  })

  open_resumen_period_drill <- function(period) {
    if (is.na(period) || !nzchar(period)) {
      return()
    }
    drill <- list(
      rules = list(list(field = "periodo", value = period)),
      label = paste("Periodo:", period)
    )
    origenes_resumen_drill_show_modal(
      paste(channel_title, "·", period),
      resumen_charts_reactive(),
      drill,
      session = session
    )
  }

  open_resumen_metric_drill <- function(patterns, label) {
    rules <- lapply(patterns, function(p) list(field = "metrica", value = p))
    drill <- list(rules = rules, label = label)
    origenes_resumen_drill_show_modal(paste(channel_title, "·", label), resumen_reactive(), drill, session = session)
  }

  shiny::observeEvent(plotly::event_data("plotly_click", source = src_series), {
    ed <- origenes_parse_click_key(plotly::event_data("plotly_click", source = src_series))
    if (is.null(ed)) {
      return()
    }
    period <- ed$key
    if (is.na(period) || !nzchar(period)) {
      period <- ed$x
    }
    open_resumen_period_drill(period)
  }, ignoreInit = TRUE)

  shiny::observeEvent(plotly::event_data("plotly_click", source = src_conv), {
    ed <- origenes_parse_click_key(plotly::event_data("plotly_click", source = src_conv))
    if (is.null(ed)) {
      return()
    }
    period <- ed$key
    if (is.na(period) || !nzchar(period)) {
      period <- ed$x
    }
    open_resumen_period_drill(period)
  }, ignoreInit = TRUE)

  # --- Resumen KPI card drills ---------------------------------------------
  shiny::observeEvent(input[[paste0(prefix, "_kpi_resumen_registrados")]], {
    open_resumen_metric_drill(
      c("^Registrados totales", "Registrados sin equipos", "Cita agendada", "Primera cita", "Conversión primera"),
      "Embajadores registrados"
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_resumen_activos")]], {
    open_resumen_metric_drill(
      c("^Activos", "Conversión activo"),
      "Embajadores activos"
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_resumen_retencion")]], {
    open_resumen_metric_drill(
      c("Tasa de retención"),
      "Tasa de retención"
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_resumen_agendadas")]], {
    origenes_citas_open_drill(
      citas_reactive(),
      list(),
      "Citas agendadas",
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_resumen_realizadas")]], {
    origenes_citas_open_drill(
      citas_reactive(),
      list(list(field = "Estatus", value = "Realizada")),
      "Citas realizadas",
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_resumen_conv_retencion")]], {
    origenes_citas_open_drill(
      citas_reactive(),
      list(list(field = "Primera Cita", value = "no")),
      "Citas de retención",
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_resumen_cierres")]], {
    origenes_ventas_drill_show_modal(
      title = channel_title,
      tbl = ventas_reactive(),
      metric_lbl = "Cierres",
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_resumen_unidades")]], {
    origenes_ventas_drill_show_modal(
      title = channel_title,
      tbl = ventas_reactive(),
      metric_lbl = "Unidades",
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_resumen_facturacion")]], {
    origenes_ventas_drill_show_modal(
      title = channel_title,
      tbl = ventas_reactive(),
      metric_lbl = "Facturación",
      session = session
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_resumen_ticket")]], {
    origenes_ventas_drill_show_modal(
      title = channel_title,
      tbl = ventas_reactive(),
      metric_lbl = "Ticket promedio",
      session = session
    )
  }, ignoreInit = TRUE)

  # --- Tables (siempre periodo del filtro de fechas) -----------------------
  output[[paste0(prefix, "_tbl_citas")]] <- DT::renderDT({
    origenes_datatable(citas_reactive(), style = "detail")
  }, server = FALSE)

  output[[paste0(prefix, "_tbl_ventas")]] <- reactable::renderReactable({
    origenes_ventas_cierre_reactable(ventas_reactive())
  })

  output[[paste0(prefix, "_tbl_embajadores")]] <- DT::renderDT({
    bundle <- resumen_reactive()
    shiny::req(bundle)
    origenes_datatable(bundle$tabla_embajadores, style = "metric")
  }, server = FALSE)

  output[[paste0(prefix, "_tbl_resultados")]] <- DT::renderDT({
    bundle <- resumen_reactive()
    shiny::req(bundle)
    origenes_datatable(bundle$tabla_resultados, style = "metric")
  }, server = FALSE)

  output[[paste0(prefix, "_tbl_conversion")]] <- DT::renderDT({
    bundle <- resumen_reactive()
    shiny::req(bundle)
    origenes_datatable(bundle$tabla_conversion, style = "metric")
  }, server = FALSE)

  origenes_bind_metric_cell_drill <- function(table_id, kind = c("embajadores", "actividad", "conversion")) {
    kind <- match.arg(kind)
    shiny::observeEvent(input[[paste0(table_id, "_cells_selected")]], {
      sel <- input[[paste0(table_id, "_cells_selected")]]
      shiny::req(!is.null(sel), length(sel) >= 2L || (is.matrix(sel) && nrow(sel) >= 1L))
      bundle <- resumen_reactive()
      shiny::req(bundle)

      if (is.matrix(sel)) {
        row_i <- as.integer(sel[1L, 1L])
        # DT: filas 1-based, columnas 0-based en cells_selected (client-side).
        col_i <- as.integer(sel[1L, 2L]) + 1L
      } else {
        row_i <- as.integer(sel[[1]])
        col_i <- as.integer(sel[[2]]) + 1L
      }
      shiny::req(is.finite(row_i), is.finite(col_i), row_i >= 1L, col_i >= 1L)

      tbl <- switch(
        kind,
        embajadores = bundle$tabla_embajadores,
        actividad = bundle$tabla_resultados,
        conversion = bundle$tabla_conversion
      )
      shiny::req(!is.null(tbl), nrow(tbl) >= row_i, ncol(tbl) >= col_i)

      metric <- as.character(tbl$metrica[[row_i]])
      period_col <- names(tbl)[[col_i]]
      # Clic en la columna métrica: interpretar como Total
      if (identical(period_col, "metrica")) {
        period_col <- "Total"
      }

      detail <- if (identical(kind, "embajadores")) {
        origenes_resumen_embajadores_cell_rows(bundle, metric, period_col)
      } else if (identical(kind, "conversion") && identical(bundle$origin_key, "broker")) {
        origenes_resumen_citas_gen_cell_rows(bundle, metric, period_col)
      } else {
        origenes_resumen_actividad_cell_rows(bundle, metric, period_col)
      }

      origenes_resumen_cell_drill_show_modal(
        title = paste(channel_title, "·", metric),
        subtitle = paste0("Columna: ", period_col),
        tbl = detail,
        session = session
      )
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  }

  origenes_bind_metric_cell_drill(paste0(prefix, "_tbl_embajadores"), "embajadores")
  origenes_bind_metric_cell_drill(paste0(prefix, "_tbl_resultados"), "actividad")
  origenes_bind_metric_cell_drill(paste0(prefix, "_tbl_conversion"), "conversion")

  invisible(NULL)
}

origenes_server <- function(input, output, session) {
  shiny::observeEvent(input$open_nxtgen, {
    bslib::nav_select("main_nav", "nxtgen", session = session)
  })
  shiny::observeEvent(input$home_nxtgen_card, {
    bslib::nav_select("main_nav", "nxtgen", session = session)
  })
  shiny::observeEvent(input$open_brokers, {
    bslib::nav_select("main_nav", "brokers", session = session)
  })
  shiny::observeEvent(input$home_brokers_card, {
    bslib::nav_select("main_nav", "brokers", session = session)
  })

  joined <- shiny::reactive({
    origenes_load_joined(Sys.getenv("ORIGENES_APP_ROOT", "."))
  })

  rscg_bundle <- shiny::reactive({
    tryCatch(
      origenes_load_rscg_bundle(Sys.getenv("ORIGENES_APP_ROOT", ".")),
      error = function(e) NULL
    )
  })

  origenes_bind_date_range <- function(prefix, origin_key) {
    shiny::reactive({
      data_as_of <- origenes_origin_activity_as_of(joined(), origin_key)
      defaults <- origenes_default_date_range_for_granularity(
        input[[paste0(prefix, "_granularity")]] %||% "anual",
        as_of = data_as_of,
        origin_key = origin_key
      )
      start <- input[[paste0(prefix, "_date_start")]]
      end <- input[[paste0(prefix, "_date_end")]]
      if (is.null(start)) start <- defaults$start
      if (is.null(end)) end <- defaults$end
      start <- as.Date(start)
      end <- as.Date(end)
      if (is.na(start)) start <- defaults$start
      if (is.na(end)) end <- defaults$end
      # No pedir fechas posteriores a la última actividad del origen.
      if (!is.na(data_as_of) && end > data_as_of) {
        end <- data_as_of
      }
      if (start > end) {
        tmp <- start
        start <- end
        end <- tmp
      }
      list(start = start, end = end)
    })
  }

  nxtgen_date_range <- origenes_bind_date_range("nxtgen", "nxtgen")
  brokers_date_range <- origenes_bind_date_range("brokers", "broker")

  # Al cambiar Periodo, aplica el default de fechas de esa vista.
  # Si el usuario luego mueve inicio/fin, se respetan hasta el próximo cambio de Periodo.
  origenes_sync_dates_to_granularity <- function(prefix, origin_key) {
    shiny::observeEvent(input[[paste0(prefix, "_granularity")]], {
      gran <- input[[paste0(prefix, "_granularity")]] %||% "anual"
      data_as_of <- origenes_origin_activity_as_of(joined(), origin_key)
      dr <- origenes_default_date_range_for_granularity(
        gran,
        as_of = data_as_of,
        origin_key = origin_key
      )
      shiny::updateDateInput(session, paste0(prefix, "_date_start"), value = dr$start)
      shiny::updateDateInput(session, paste0(prefix, "_date_end"), value = dr$end)
    }, ignoreInit = TRUE)
  }
  origenes_sync_dates_to_granularity("nxtgen", "nxtgen")
  origenes_sync_dates_to_granularity("brokers", "broker")

  # Al cargar: alinea el fin del calendario a la última actividad del origen.
  shiny::observeEvent(joined(), {
    j <- joined()
    shiny::req(!is.null(j))
    for (pair in list(
      list(prefix = "nxtgen", origin = "nxtgen"),
      list(prefix = "brokers", origin = "broker")
    )) {
      data_as_of <- origenes_origin_activity_as_of(j, pair$origin)
      gran <- input[[paste0(pair$prefix, "_granularity")]] %||% "anual"
      dr <- origenes_default_date_range_for_granularity(
        gran,
        as_of = data_as_of,
        origin_key = pair$origin
      )
      shiny::updateDateInput(session, paste0(pair$prefix, "_date_start"), value = dr$start)
      shiny::updateDateInput(session, paste0(pair$prefix, "_date_end"), value = dr$end)
    }
  }, once = TRUE)

  nxtgen_granularity <- shiny::reactive({
    input$nxtgen_granularity %||% "anual"
  })
  brokers_granularity <- shiny::reactive({
    input$brokers_granularity %||% "anual"
  })

  # --- Brokers roster (Nombre app / Conciliación CRM / Canal) ----------------
  app_root <- Sys.getenv("ORIGENES_APP_ROOT", ".")

  brokers_overrides_rv <- shiny::reactiveVal({
    ov <- origenes_brokers_load_overrides(app_root)
    if (!nrow(ov)) {
      ov <- origenes_brokers_seed_overrides_from_sheets(app_root)
      if (nrow(ov)) {
        origenes_brokers_save_overrides(ov, app_root)
      }
    }
    ov
  })

  brokers_origin_obj <- shiny::reactive({
    j <- joined()
    shiny::req(j)
    j$origins$broker
  })

  brokers_roster <- shiny::reactive({
    origin_obj <- brokers_origin_obj()
    shiny::req(origin_obj)
    origenes_brokers_roster_build(
      origin_obj,
      overrides = brokers_overrides_rv(),
      root = app_root
    )
  })

  brokers_roster_page_size <- 50L
  brokers_roster_visible_n <- shiny::reactiveVal(brokers_roster_page_size)
  shiny::observeEvent(
    brokers_roster(),
    {
      brokers_roster_visible_n(brokers_roster_page_size)
    },
    ignoreNULL = TRUE
  )
  shiny::observeEvent(input$brokers_roster_load_more, {
    brokers_roster_visible_n(brokers_roster_visible_n() + brokers_roster_page_size)
  })
  shiny::observeEvent(input$brokers_roster_load_all, {
    brokers_roster_visible_n(.Machine$integer.max)
  })

  brokers_upsert_override <- function(key, patch) {
    roster <- brokers_roster()
    shiny::req(nzchar(key %||% ""), nrow(roster) > 0)
    row <- roster[roster$nombre_key == key, , drop = FALSE]
    shiny::req(nrow(row) > 0)
    fecha_grad_cur <- if ("fecha_graduacion" %in% names(row)) {
      as.character(row$fecha_graduacion[[1]] %||% "")
    } else {
      ""
    }
    if (inherits(row$fecha_graduacion[[1]], "Date") && !is.na(row$fecha_graduacion[[1]])) {
      fecha_grad_cur <- format(row$fecha_graduacion[[1]], "%Y-%m-%d")
    }
    new_row <- tibble::tibble(
      nombre_key = key,
      amb_contact_id = row$amb_contact_id[[1]] %||% NA_character_,
      nombre_app = row$nombre_app[[1]],
      conciliacion = stringr::str_squish(
        patch$conciliacion %||% row$conciliacion[[1]] %||% ""
      ),
      canal = stringr::str_squish(patch$canal %||% row$canal[[1]] %||% ""),
      pertenece_equipo = if (!is.null(patch$pertenece_equipo)) {
        isTRUE(patch$pertenece_equipo)
      } else {
        isTRUE(row$pertenece_equipo[[1]])
      },
      pertenece_manual = TRUE,
      generacion = stringr::str_squish(
        patch$generacion %||% row$generacion[[1]] %||% ""
      ),
      fecha_graduacion = stringr::str_squish(
        patch$fecha_graduacion %||% fecha_grad_cur
      ),
      gen_registro = row$gen_registro[[1]] %||% "",
      excluido = isTRUE(patch$excluido %||% FALSE),
      updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    )
    ov <- brokers_overrides_rv()
    ov <- ov[ov$nombre_key != key, , drop = FALSE]
    ov <- dplyr::bind_rows(ov, new_row)
    saved <- tryCatch({
      origenes_brokers_save_overrides(ov, app_root)
      TRUE
    }, error = function(e) FALSE)
    brokers_overrides_rv(ov)
    saved
  }

  shiny::observeEvent(input$brokers_roster_cell_edit, {
    edit <- input$brokers_roster_cell_edit
    shiny::req(!is.null(edit), nzchar(edit$key %||% ""))
    field <- as.character(edit$field %||% "")
    value <- stringr::str_squish(as.character(edit$value %||% ""))
    patch <- list()
    if (identical(field, "conciliacion")) {
      patch$conciliacion <- value
    } else if (identical(field, "canal")) {
      patch$canal <- value
    } else if (identical(field, "generacion")) {
      patch$generacion <- value
    } else if (identical(field, "fecha_graduacion")) {
      d <- origenes_brokers_parse_sheet_fecha(value)
      if (nzchar(value) && is.na(d)) {
        d <- suppressWarnings(as.Date(value))
      }
      if (nzchar(value) && is.na(d)) {
        shiny::showNotification(
          "Usa fecha dd/mm/yyyy (ej. 04/05/2026)",
          type = "warning",
          duration = 3
        )
        return(invisible(NULL))
      }
      patch$fecha_graduacion <- if (is.na(d)) "" else format(d, "%Y-%m-%d")
    } else {
      return(invisible(NULL))
    }
    saved <- brokers_upsert_override(edit$key, patch)
    shiny::showNotification(
      if (isTRUE(saved)) "Guardado" else "Guardado en sesión (no se pudo escribir el archivo)",
      type = if (isTRUE(saved)) "message" else "warning",
      duration = 2
    )
  }, ignoreInit = TRUE)

  brokers_delete_key <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$brokers_roster_delete_key, {
    key <- input$brokers_roster_delete_key
    roster <- brokers_roster()
    shiny::req(nzchar(key %||% ""), nrow(roster) > 0)
    row <- roster[roster$nombre_key == key, , drop = FALSE]
    shiny::req(nrow(row) > 0)
    brokers_delete_key(key)
    nombre <- row$nombre_app[[1]]
    shiny::showModal(shiny::modalDialog(
      title = "Eliminar fila",
      shiny::p(
        "¿Quitar a ", shiny::strong(nombre),
        " de la conciliación? Deja de aparecer en esta lista y en los conteos de embajadores."
      ),
      footer = shiny::tagList(
        shiny::modalButton("Cancelar"),
        shiny::actionButton(
          "brokers_roster_delete_confirm",
          "Eliminar",
          class = "btn-danger"
        )
      ),
      easyClose = TRUE
    ))
  })

  shiny::observeEvent(input$brokers_roster_delete_confirm, {
    shiny::removeModal()
    key <- brokers_delete_key()
    roster <- brokers_roster()
    shiny::req(nzchar(key %||% ""), nrow(roster) > 0)
    row <- roster[roster$nombre_key == key, , drop = FALSE]
    shiny::req(nrow(row) > 0)

    saved <- brokers_upsert_override(key, list(excluido = TRUE))
    shiny::showNotification(
      if (isTRUE(saved)) {
        paste0(row$nombre_app[[1]], " eliminado de la conciliación")
      } else {
        "Quitado en sesión (no se pudo escribir el archivo)"
      },
      type = if (isTRUE(saved)) "message" else "warning",
      duration = 3
    )
  })

  brokers_roster_show <- shiny::reactive({
    roster <- brokers_roster()
    origin <- brokers_origin_obj()
    shiny::req(nrow(roster) > 0, !is.null(origin))
    as_of <- origenes_origin_activity_as_of(joined(), "broker")
    origenes_brokers_embajadores_tabla(
      origin,
      roster,
      as_of = as_of,
      root = app_root
    )
  })

  output$brokers_crm_datalist <- shiny::renderUI({
    shiny::req(identical(input$brokers_subtab %||% "", "embajadores"))
    crm <- origenes_brokers_conciliacion_choices(brokers_origin_obj())
    htmltools::HTML(origenes_brokers_datalist_html("brokers_crm_choices", crm))
  })

  brokers_roster_filtered <- shiny::reactive({
    show_all <- brokers_roster_show()
    shiny::req(nrow(show_all) > 0)
    # Fecha ordenable: usamos .fecha_reg (Date); ocultamos el texto dd/mm/yyyy.
    show_all <- show_all |>
      dplyr::select(-dplyr::any_of("Fecha de registro"))

    col <- as.character(input$brokers_roster_col %||% "")
    q <- stringr::str_squish(as.character(input$brokers_roster_col_filter %||% ""))
    if (nzchar(col) && nzchar(q) && col %in% names(show_all)) {
      vals <- show_all[[col]]
      if (inherits(vals, "Date")) {
        vals_chr <- origenes_fmt_date_sheets(vals)
        vals_chr[is.na(vals_chr)] <- ""
        hit <- grepl(tolower(q), tolower(vals_chr), fixed = TRUE) |
          grepl(tolower(q), tolower(as.character(vals)), fixed = TRUE)
      } else {
        vals_chr <- as.character(vals)
        vals_chr[is.na(vals_chr)] <- ""
        hit <- grepl(tolower(q), tolower(vals_chr), fixed = TRUE)
      }
      show_all <- show_all[hit, , drop = FALSE]
    }

    # Orden por defecto; el usuario ordena picando el encabezado de columna.
    show_all |>
      dplyr::arrange(
        dplyr::desc(.data$.fecha_reg),
        .data$Nombre,
        .data$nombre_key
      )
  })

  shiny::observeEvent(
    list(input$brokers_roster_col, input$brokers_roster_col_filter),
    {
      brokers_roster_visible_n(brokers_roster_page_size)
    },
    ignoreInit = TRUE
  )

  output$brokers_roster_load_more_ui <- shiny::renderUI({
    shiny::req(identical(input$brokers_subtab %||% "", "embajadores"))
    show <- brokers_roster_filtered()
    shiny::req(nrow(show) >= 0)
    visible <- min(brokers_roster_visible_n(), nrow(show))
    remaining <- nrow(show) - visible
    if (remaining <= 0) {
      return(shiny::div(
        class = "or-brokers-roster__more",
        shiny::span(
          class = "or-brokers-roster__more-meta",
          paste0("Mostrando ", nrow(show), " de ", nrow(show))
        )
      ))
    }
    next_chunk <- min(brokers_roster_page_size, remaining)
    shiny::div(
      class = "or-brokers-roster__more",
      shiny::span(
        class = "or-brokers-roster__more-meta",
        paste0("Mostrando ", visible, " de ", nrow(show))
      ),
      shiny::actionButton(
        "brokers_roster_load_more",
        paste0("Cargar más (+", next_chunk, ")"),
        class = "btn-sm or-brokers-roster__more-btn"
      ),
      shiny::actionButton(
        "brokers_roster_load_all",
        paste0("Mostrar todos (", remaining, " restantes)"),
        class = "btn-sm btn-link or-brokers-roster__more-all"
      )
    )
  })

  output$brokers_roster_table <- reactable::renderReactable({
    # No construir la tabla hasta abrir la pestaña (evita colgar el arranque).
    shiny::req(identical(input$brokers_subtab %||% "", "embajadores"))
    show_all <- brokers_roster_filtered()
    shiny::req(nrow(show_all) > 0)
    visible_n <- min(brokers_roster_visible_n(), nrow(show_all))
    show <- utils::head(show_all, visible_n)
    keys <- as.character(show$nombre_key)
    header_style <- list(whiteSpace = "normal", lineHeight = "1.3")
    reactable::reactable(
      show,
      filterable = FALSE,
      searchable = FALSE,
      pagination = FALSE,
      defaultSorted = list(.fecha_reg = "desc", Nombre = "asc"),
      highlight = TRUE,
      compact = TRUE,
      wrap = FALSE,
      defaultColDef = reactable::colDef(
        headerStyle = header_style,
        sortNALast = TRUE
      ),
      columns = list(
        nombre_key = reactable::colDef(show = FALSE),
        .fecha_reg = reactable::colDef(
          name = "Fecha de registro",
          minWidth = 150,
          headerStyle = header_style,
          cell = function(value) {
            out <- origenes_fmt_date_sheets(value)
            if (is.na(out) || identical(out, "NA")) "" else out
          }
        ),
        Nombre = reactable::colDef(name = "Nombre", minWidth = 180),
        `Pertenece a un equipo` = reactable::colDef(
          name = "Pertenece a un equipo",
          minWidth = 170
        ),
        Role = reactable::colDef(name = "Role", minWidth = 120),
        `Conciliacion Nombre` = reactable::colDef(
          name = "Conciliacion Nombre",
          minWidth = 200,
          html = TRUE,
          cell = function(value, index) {
            origenes_brokers_text_cell(
              keys[[index]],
              "conciliacion",
              value %||% "",
              placeholder = "Elegir o escribir",
              list_id = "brokers_crm_choices"
            )
          }
        ),
        Canal = reactable::colDef(
          name = "Canal",
          minWidth = 160,
          html = TRUE,
          cell = function(value, index) {
            origenes_brokers_text_cell(
              keys[[index]],
              "canal",
              value %||% "",
              placeholder = "Canal",
              list_id = "brokers_canal_choices"
            )
          }
        ),
        `Fecha graduación` = reactable::colDef(
          name = "Fecha graduación",
          minWidth = 150,
          html = TRUE,
          cell = function(value, index) {
            origenes_brokers_text_cell(
              keys[[index]],
              "fecha_graduacion",
              value %||% "",
              placeholder = "dd/mm/yyyy"
            )
          }
        ),
        Generación = reactable::colDef(
          name = "Generación",
          minWidth = 120,
          html = TRUE,
          cell = function(value, index) {
            origenes_brokers_text_cell(
              keys[[index]],
              "generacion",
              value %||% "",
              placeholder = "Gen 1",
              list_id = "brokers_gen_choices"
            )
          }
        ),
        `Fecha primera cita realizada` = reactable::colDef(
          name = "Fecha primera cita realizada", minWidth = 190
        ),
        `Fecha de última cita realizada` = reactable::colDef(
          name = "Fecha de última cita realizada", minWidth = 190
        ),
        `Gen Registro` = reactable::colDef(name = "Gen Registro", minWidth = 120),
        `Días desde la última cita realizadas` = reactable::colDef(
          name = "Días desde la última cita realizadas", minWidth = 200
        ),
        `Fecha de última cita agendada/realizada` = reactable::colDef(
          name = "Fecha de última cita agendada/realizada", minWidth = 220
        ),
        `Gen primera cita realizada` = reactable::colDef(
          name = "Gen primera cita realizada", minWidth = 180
        ),
        `Ciclo Registro / Primera Cita` = reactable::colDef(
          name = "Ciclo Registro / Primera Cita", minWidth = 190
        )
      )
    )
  })

  nxtgen_resumen <- shiny::reactive({
    dr <- nxtgen_date_range()
    data_as_of <- origenes_origin_activity_as_of(joined(), "nxtgen")
    origenes_resumen_bundle(
      joined(),
      "nxtgen",
      as_of = data_as_of,
      start = dr$start,
      end = dr$end,
      granularity = nxtgen_granularity()
    )
  })

  brokers_persona_keys <- shiny::reactive({
    emb <- input$brokers_filter_embajador %||% ""
    canal <- input$brokers_filter_canal %||% ""
    if (nzchar(emb)) {
      return(emb)
    }
    if (!nzchar(canal)) {
      return(NULL)
    }
    personas <- origenes_brokers_personas(brokers_roster())
    personas$key[!is.na(personas$canal) & personas$canal == canal]
  })

  shiny::observeEvent(brokers_roster(), {
    roster <- brokers_roster()
    shiny::req(nrow(roster) > 0)
    canales <- sort(unique(roster$canal[!is.na(roster$canal) & nzchar(roster$canal)]))
    selected <- shiny::isolate(input$brokers_filter_canal %||% "")
    if (!selected %in% c("", canales)) {
      selected <- ""
    }
    shiny::updateSelectizeInput(
      session,
      "brokers_filter_canal",
      choices = c("Todos" = "", stats::setNames(canales, canales)),
      selected = selected
    )
  }, ignoreInit = FALSE)

  shiny::observeEvent(
    list(brokers_roster(), input$brokers_filter_canal),
    {
      roster <- brokers_roster()
      shiny::req(nrow(roster) > 0)
      personas <- origenes_brokers_personas(
        roster,
        con_cita = origenes_brokers_app_cita_keys(brokers_origin_obj())
      )
      canal <- input$brokers_filter_canal %||% ""
      if (nzchar(canal)) {
        personas <- personas[!is.na(personas$canal) & personas$canal == canal, , drop = FALSE]
      }
      personas <- personas[
        !is.na(personas$key) & nzchar(personas$key) &
          !is.na(personas$label) & nzchar(personas$label),
        ,
        drop = FALSE
      ]
      selected <- shiny::isolate(input$brokers_filter_embajador %||% "")
      if (!selected %in% c("", personas$key)) {
        selected <- ""
      }
      shiny::updateSelectizeInput(
        session,
        "brokers_filter_embajador",
        choices = c("Todos" = "", stats::setNames(personas$key, personas$label)),
        selected = selected
      )
    },
    ignoreInit = FALSE
  )

  brokers_resumen <- shiny::reactive({
    dr <- brokers_date_range()
    data_as_of <- origenes_origin_activity_as_of(joined(), "broker")
    origenes_resumen_bundle(
      joined(),
      "broker",
      as_of = data_as_of,
      start = dr$start,
      end = dr$end,
      granularity = brokers_granularity(),
      roster = brokers_roster(),
      embajador_keys = brokers_persona_keys()
    )
  })

  # Gráficas Resumen: visibles solo en Anual, con un punto por mes del año.
  nxtgen_resumen_charts <- shiny::reactive({
    if (!identical(nxtgen_granularity(), "anual")) {
      return(NULL)
    }
    data_as_of <- origenes_origin_activity_as_of(joined(), "nxtgen")
    dr <- origenes_ytd_date_range(data_as_of)
    origenes_resumen_bundle(
      joined(),
      "nxtgen",
      as_of = data_as_of,
      start = dr$start,
      end = dr$end,
      granularity = "mensual"
    )
  })

  brokers_resumen_charts <- shiny::reactive({
    if (!identical(brokers_granularity(), "anual")) {
      return(NULL)
    }
    data_as_of <- origenes_origin_activity_as_of(joined(), "broker")
    dr <- origenes_default_date_range_for_granularity(
      "anual",
      as_of = data_as_of,
      origin_key = "broker"
    )
    origenes_resumen_bundle(
      joined(),
      "broker",
      as_of = data_as_of,
      start = dr$start,
      end = dr$end,
      granularity = "mensual",
      roster = brokers_roster()
    )
  })

  nxtgen_ventas <- shiny::reactive({
    dr <- nxtgen_date_range()
    origenes_ventas_table(
      "nxtgen",
      start = dr$start,
      end = dr$end,
      rscg = rscg_bundle()
    )
  })

  brokers_ventas <- shiny::reactive({
    dr <- brokers_date_range()
    origenes_ventas_table(
      "broker",
      start = dr$start,
      end = dr$end,
      rscg = rscg_bundle(),
      joined = joined(),
      embajador_keys = brokers_persona_keys()
    )
  })

  # Gen Venta / Estatus×gen: últimos 6 meses.
  nxtgen_ventas_charts <- shiny::reactive({
    dr <- origenes_chart_date_range(n_months = 6L)
    origenes_ventas_table(
      "nxtgen",
      start = dr$start,
      end = dr$end,
      rscg = rscg_bundle()
    )
  })

  brokers_ventas_charts <- shiny::reactive({
    dr <- origenes_chart_date_range(n_months = 6L)
    origenes_ventas_table(
      "broker",
      start = dr$start,
      end = dr$end,
      rscg = rscg_bundle()
    )
  })

  nxtgen_citas <- shiny::reactive({
    dr <- nxtgen_date_range()
    origenes_citas_table(
      "nxtgen",
      start = dr$start,
      end = dr$end,
      rscg = rscg_bundle(),
      joined = joined()
    )
  })

  brokers_citas <- shiny::reactive({
    dr <- brokers_date_range()
    origenes_citas_table(
      "broker",
      start = dr$start,
      end = dr$end,
      rscg = rscg_bundle(),
      joined = joined(),
      embajador_keys = brokers_persona_keys()
    )
  })

  nxtgen_citas_charts <- shiny::reactive({
    dr <- origenes_chart_date_range(n_months = 6L)
    origenes_citas_table(
      "nxtgen",
      start = dr$start,
      end = dr$end,
      rscg = rscg_bundle(),
      joined = joined()
    )
  })

  brokers_citas_charts <- shiny::reactive({
    dr <- origenes_chart_date_range(n_months = 6L)
    origenes_citas_table(
      "broker",
      start = dr$start,
      end = dr$end,
      rscg = rscg_bundle(),
      joined = joined()
    )
  })

  output$nxtgen_resumen_header <- shiny::renderUI({
    origenes_resumen_header_ui(nxtgen_resumen(), "nxtgen")
  })
  output$brokers_resumen_header <- shiny::renderUI({
    origenes_resumen_header_ui(brokers_resumen(), "brokers")
  })

  origenes_bind_origin_server(
    prefix = "nxtgen",
    input = input,
    output = output,
    session = session,
    resumen_reactive = nxtgen_resumen,
    resumen_charts_reactive = nxtgen_resumen_charts,
    citas_reactive = nxtgen_citas,
    citas_charts_reactive = nxtgen_citas_charts,
    ventas_reactive = nxtgen_ventas,
    ventas_charts_reactive = nxtgen_ventas_charts,
    date_range_reactive = nxtgen_date_range,
    include_postgrad = FALSE
  )

  origenes_bind_origin_server(
    prefix = "brokers",
    input = input,
    output = output,
    session = session,
    resumen_reactive = brokers_resumen,
    resumen_charts_reactive = brokers_resumen_charts,
    citas_reactive = brokers_citas,
    citas_charts_reactive = brokers_citas_charts,
    ventas_reactive = brokers_ventas,
    ventas_charts_reactive = brokers_ventas_charts,
    date_range_reactive = brokers_date_range,
    include_postgrad = TRUE
  )
}
