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
  gran_label <- origenes_period_label_es(bundle$granularity %||% "mensual")
  active_days <- tryCatch(brokers_active_window_days(), error = function(e) 30L)
  shiny::div(
    class = "or-resumen",
    origenes_period_header_ui(
      "PERIODO",
      origenes_format_date_range_label(bundle$span$start, bundle$span$end),
      paste("Vista", tolower(gran_label))
    ),
    shiny::div(
      class = "or-resumen-grid or-resumen-grid--3",
      origenes_resumen_kpi(
        "Embajadores registrados",
        nxtgen_fmt_num(k$registrados),
        paste0(
          nxtgen_fmt_num(k$primera_cita_emb),
          " con primera cita · ",
          nxtgen_fmt_pct(k$conv_primera_reg),
          " conversión"
        ),
        accent = TRUE,
        id = paste0(prefix, "_kpi_resumen_registrados")
      ),
      origenes_resumen_kpi(
        paste0("Activos (", active_days, " días)"),
        nxtgen_fmt_num(k$activos),
        paste0(
          "Stock del canal: embajadores con cita en los últimos ",
          active_days,
          " días (no solo la cohorte del filtro)"
        ),
        id = paste0(prefix, "_kpi_resumen_activos")
      ),
      origenes_resumen_kpi(
        "Citas nuevas realizadas",
        nxtgen_fmt_num(k$citas_nuevas),
        paste0(nxtgen_fmt_num(k$citas_agendadas), " agendadas"),
        id = paste0(prefix, "_kpi_resumen_citas_nuevas")
      ),
      origenes_resumen_kpi(
        "Cierres",
        nxtgen_fmt_num(k$cierres),
        paste0(nxtgen_fmt_pct(k$conv_cita_cierre), " conversión cita→venta"),
        id = paste0(prefix, "_kpi_resumen_cierres")
      ),
      origenes_resumen_kpi(
        "Unidades vendidas",
        nxtgen_fmt_num(k$unidades),
        "firmadas del canal",
        id = paste0(prefix, "_kpi_resumen_unidades")
      ),
      origenes_resumen_kpi(
        "Facturación",
        origenes_money(k$facturacion, compact = TRUE),
        "valor acumulado firmado",
        id = paste0(prefix, "_kpi_resumen_facturacion")
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

  # --- Headers / KPIs ------------------------------------------------------
  output[[paste0(prefix, "_citas_header")]] <- shiny::renderUI({
    full <- citas_reactive()
    dr <- date_range_reactive()
    origenes_period_header_ui(
      "CITAS",
      origenes_format_date_range_label(dr$start, dr$end),
      paste(nrow(full), "filas")
    )
  })

  output[[paste0(prefix, "_ventas_header")]] <- shiny::renderUI({
    full <- ventas_reactive()
    dr <- date_range_reactive()
    origenes_period_header_ui(
      "VENTAS",
      origenes_format_date_range_label(dr$start, dr$end),
      paste(nrow(full), "filas")
    )
  })

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

  # --- Resumen charts (últimos 6 meses) ------------------------------------
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
      c("^Registrados$", "Cita agendada", "Primera cita", "Conversión primera"),
      "Embajadores registrados"
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_resumen_activos")]], {
    open_resumen_metric_drill(
      c("^Activos", "Conversión activo"),
      "Embajadores activos"
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input[[paste0(prefix, "_kpi_resumen_citas_nuevas")]], {
    origenes_citas_open_drill(
      citas_reactive(),
      list(
        list(field = "Primera Cita", value = "si"),
        list(field = "Estatus", value = "Realizada")
      ),
      "Primera cita realizada",
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

  origenes_bind_date_range <- function(prefix) {
    shiny::reactive({
      defaults <- origenes_default_date_range()
      start <- input[[paste0(prefix, "_date_start")]]
      end <- input[[paste0(prefix, "_date_end")]]
      if (is.null(start)) start <- defaults$start
      if (is.null(end)) end <- defaults$end
      start <- as.Date(start)
      end <- as.Date(end)
      if (is.na(start)) start <- defaults$start
      if (is.na(end)) end <- defaults$end
      if (start > end) {
        tmp <- start
        start <- end
        end <- tmp
      }
      list(start = start, end = end)
    })
  }

  nxtgen_date_range <- origenes_bind_date_range("nxtgen")
  brokers_date_range <- origenes_bind_date_range("brokers")

  nxtgen_granularity <- shiny::reactive({
    input$nxtgen_granularity %||% "mensual"
  })
  brokers_granularity <- shiny::reactive({
    input$brokers_granularity %||% "mensual"
  })

  nxtgen_resumen <- shiny::reactive({
    dr <- nxtgen_date_range()
    origenes_resumen_bundle(
      joined(),
      "nxtgen",
      start = dr$start,
      end = dr$end,
      granularity = nxtgen_granularity()
    )
  })

  brokers_resumen <- shiny::reactive({
    dr <- brokers_date_range()
    origenes_resumen_bundle(
      joined(),
      "broker",
      start = dr$start,
      end = dr$end,
      granularity = brokers_granularity()
    )
  })

  # Gráficas Resumen: siempre últimos 6 meses (misma granularidad del filtro).
  nxtgen_resumen_charts <- shiny::reactive({
    dr <- origenes_chart_date_range(n_months = 6L)
    origenes_resumen_bundle(
      joined(),
      "nxtgen",
      start = dr$start,
      end = dr$end,
      granularity = nxtgen_granularity()
    )
  })

  brokers_resumen_charts <- shiny::reactive({
    dr <- origenes_chart_date_range(n_months = 6L)
    origenes_resumen_bundle(
      joined(),
      "broker",
      start = dr$start,
      end = dr$end,
      granularity = brokers_granularity()
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
      rscg = rscg_bundle()
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
      joined = joined()
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
