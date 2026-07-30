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

origenes_resumen_header_ui <- function(bundle) {
  if (is.null(bundle)) {
    return(origenes_empty_ui())
  }
  k <- bundle$kpis
  gran_label <- origenes_period_label_es(bundle$granularity %||% "mensual")
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
        accent = TRUE
      ),
      origenes_resumen_kpi(
        "Activos (30 días)",
        nxtgen_fmt_num(k$activos),
        paste0(nxtgen_fmt_pct(k$conv_activo_primera), " de quienes ya tuvieron primera cita")
      ),
      origenes_resumen_kpi(
        "Citas nuevas realizadas",
        nxtgen_fmt_num(k$citas_nuevas),
        paste0(nxtgen_fmt_num(k$citas_agendadas), " agendadas en el canal")
      ),
      origenes_resumen_kpi(
        "Cierres",
        nxtgen_fmt_num(k$cierres),
        paste0(nxtgen_fmt_pct(k$conv_cita_cierre), " conversión cita realizada → venta")
      ),
      origenes_resumen_kpi(
        "Unidades vendidas",
        nxtgen_fmt_num(k$unidades),
        "operaciones firmadas del canal"
      ),
      origenes_resumen_kpi(
        "Facturación",
        origenes_money(k$facturacion, compact = TRUE),
        "valor acumulado firmado"
      )
    )
  )
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

  output$nxtgen_resumen_header <- shiny::renderUI({
    origenes_resumen_header_ui(nxtgen_resumen())
  })
  output$brokers_resumen_header <- shiny::renderUI({
    origenes_resumen_header_ui(brokers_resumen())
  })

  output$nxtgen_citas_header <- shiny::renderUI({
    tbl <- nxtgen_citas()
    dr <- nxtgen_date_range()
    origenes_period_header_ui(
      "CITAS",
      origenes_format_date_range_label(dr$start, dr$end),
      paste(nrow(tbl), "filas")
    )
  })
  output$nxtgen_ventas_header <- shiny::renderUI({
    tbl <- nxtgen_ventas()
    dr <- nxtgen_date_range()
    origenes_period_header_ui(
      "VENTAS",
      origenes_format_date_range_label(dr$start, dr$end),
      paste(nrow(tbl), "filas")
    )
  })
  output$brokers_citas_header <- shiny::renderUI({
    tbl <- brokers_citas()
    dr <- brokers_date_range()
    origenes_period_header_ui(
      "CITAS",
      origenes_format_date_range_label(dr$start, dr$end),
      paste(nrow(tbl), "filas")
    )
  })
  output$brokers_ventas_header <- shiny::renderUI({
    tbl <- brokers_ventas()
    dr <- brokers_date_range()
    origenes_period_header_ui(
      "VENTAS",
      origenes_format_date_range_label(dr$start, dr$end),
      paste(nrow(tbl), "filas")
    )
  })

  # DTOutput estático en UI → renderDT (evita widgets rotos dentro de renderUI).
  output$nxtgen_tbl_embajadores <- DT::renderDT({
    bundle <- nxtgen_resumen()
    shiny::req(bundle)
    origenes_datatable(bundle$tabla_embajadores, style = "metric")
  }, server = FALSE)

  output$nxtgen_tbl_resultados <- DT::renderDT({
    bundle <- nxtgen_resumen()
    shiny::req(bundle)
    origenes_datatable(bundle$tabla_resultados, style = "metric")
  }, server = FALSE)

  output$nxtgen_tbl_conversion <- DT::renderDT({
    bundle <- nxtgen_resumen()
    shiny::req(bundle)
    origenes_datatable(bundle$tabla_conversion, style = "metric")
  }, server = FALSE)

  output$brokers_tbl_embajadores <- DT::renderDT({
    bundle <- brokers_resumen()
    shiny::req(bundle)
    origenes_datatable(bundle$tabla_embajadores, style = "metric")
  }, server = FALSE)

  output$brokers_tbl_resultados <- DT::renderDT({
    bundle <- brokers_resumen()
    shiny::req(bundle)
    origenes_datatable(bundle$tabla_resultados, style = "metric")
  }, server = FALSE)

  output$brokers_tbl_conversion <- DT::renderDT({
    bundle <- brokers_resumen()
    shiny::req(bundle)
    origenes_datatable(bundle$tabla_conversion, style = "metric")
  }, server = FALSE)

  output$nxtgen_tbl_ventas <- DT::renderDT({
    origenes_datatable(nxtgen_ventas(), style = "detail")
  }, server = FALSE)

  output$brokers_tbl_ventas <- DT::renderDT({
    origenes_datatable(brokers_ventas(), style = "detail")
  }, server = FALSE)

  output$nxtgen_tbl_citas <- DT::renderDT({
    origenes_datatable(nxtgen_citas(), style = "detail")
  }, server = FALSE)

  output$brokers_tbl_citas <- DT::renderDT({
    origenes_datatable(brokers_citas(), style = "detail")
  }, server = FALSE)

  # Renderizar aunque la pestaña esté oculta (bslib/nav).
  for (id in c(
    "nxtgen_tbl_embajadores", "nxtgen_tbl_resultados", "nxtgen_tbl_conversion",
    "nxtgen_tbl_citas", "nxtgen_tbl_ventas",
    "brokers_tbl_embajadores", "brokers_tbl_resultados", "brokers_tbl_conversion",
    "brokers_tbl_citas", "brokers_tbl_ventas"
  )) {
    shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}
