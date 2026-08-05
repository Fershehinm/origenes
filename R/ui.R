origenes_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#f4f1eb",
    fg = "#17231d",
    primary = "#1f6a4a",
    secondary = "#d9a441",
    base_font = bslib::font_google("DM Sans"),
    heading_font = bslib::font_google("Manrope"),
    "border-radius" = "1rem",
    "card-border-width" = "0"
  )
}

origenes_brand <- function() {
  shiny::tags$a(
    href = "#",
    class = "or-brand",
    onclick = "document.querySelector('[data-value=\"inicio\"]').click(); return false;",
    shiny::span(class = "or-brand__mark", "O"),
    shiny::span(
      shiny::span(class = "or-brand__name", "Origin Analytics"),
      shiny::span(class = "or-brand__eyebrow", "DIVISIÓN 2c")
    )
  )
}

origenes_empty_ui <- function() {
  shiny::div(
    class = "or-empty",
    shiny::div(class = "or-empty__icon", "↗"),
    shiny::h3("Conecta la fuente de datos"),
    shiny::p(
      "Ejecuta ",
      shiny::code("Rscript scripts/build_origenes_canonical.R"),
      " para generar ",
      shiny::code("data/canonical/origenes_joined.rds"),
      "."
    )
  )
}

origenes_home_ui <- function() {
  shiny::div(
    class = "or-page or-home",
    shiny::tags$section(
      class = "or-section or-section--home",
      shiny::div(
        class = "or-section__heading or-section__heading--solo",
        shiny::h2(class = "or-home-title", "Orígenes")
      ),
      shiny::div(
        class = "or-origin-grid",
        shiny::tags$button(
          id = "home_nxtgen_card",
          class = "action-button or-origin-card or-origin-card--active",
          type = "button",
          onclick = "document.querySelector('[data-value=\"nxtgen\"]').click();",
          shiny::span(class = "or-origin-card__number", "01"),
          shiny::span(class = "or-origin-card__title", "NxtGen"),
          shiny::span(class = "or-origin-card__arrow", "↗")
        ),
        shiny::tags$button(
          id = "home_brokers_card",
          class = "action-button or-origin-card or-origin-card--active",
          type = "button",
          onclick = "document.querySelector('[data-value=\"brokers\"]').click();",
          shiny::span(class = "or-origin-card__number", "02"),
          shiny::span(class = "or-origin-card__title", "Brokers"),
          shiny::span(class = "or-origin-card__arrow", "↗")
        )
      )
    )
  )
}

origenes_filter_col <- function(..., min_width = "160px", grow = FALSE) {
  shiny::div(
    class = paste("or-filter-col", if (grow) "or-filter-col--grow" else NULL),
    style = paste0("min-width:", min_width, ";"),
    ...
  )
}

origenes_filter_bar <- function(...) {
  shiny::div(
    class = "or-filter-bar",
    shiny::div(class = "or-filter-bar__row", ...)
  )
}

origenes_resumen_kpi <- function(label, value, note = NULL, accent = FALSE, id = NULL) {
  body <- list(
    shiny::span(class = "or-resumen-kpi__label", label),
    shiny::span(class = "or-resumen-kpi__value", value),
    if (!is.null(note)) shiny::span(class = "or-resumen-kpi__note", note)
  )
  if (is.null(id)) {
    return(shiny::div(
      class = paste("or-resumen-kpi", if (accent) "or-resumen-kpi--accent" else NULL),
      body
    ))
  }
  shiny::tags$button(
    id = id,
    class = paste(
      "action-button or-resumen-kpi or-resumen-kpi--clickable",
      if (accent) "or-resumen-kpi--accent" else NULL
    ),
    type = "button",
    title = "Clic para ver el detalle",
    body
  )
}

origenes_metric_table_ui <- function(title, subtitle, output_id) {
  bslib::card(
    class = "or-card or-metric-card",
    bslib::card_header(
      shiny::div(
        shiny::span(class = "or-card__eyebrow", "TABLA"),
        shiny::h3(title)
      ),
      shiny::span(class = "or-card__meta", subtitle)
    ),
    shiny::div(
      class = "or-metric-table-wrap",
      DT::DTOutput(output_id, width = "100%")
    )
  )
}

#' Calendario inicio/fin estilo IBR (shiny::dateInput, dd/mm/yyyy).
origenes_calendar_range_ui <- function(start_id,
                                       end_id,
                                       start_label = "Fecha inicio",
                                       end_label = "Fecha fin",
                                       start_value = NULL,
                                       end_value = NULL,
                                       min_date = NULL,
                                       max_date = NULL) {
  defaults <- origenes_default_date_range()
  if (is.null(start_value)) {
    start_value <- defaults$start
  }
  if (is.null(end_value)) {
    end_value <- defaults$end
  }
  if (is.null(max_date)) {
    max_date <- origenes_calendar_max_date()
  }
  shiny::div(
    class = "calendar-range-split",
    shiny::div(
      class = "calendar-range-field",
      shiny::tags$label(`for` = start_id, start_label),
      shiny::dateInput(
        start_id,
        label = NULL,
        value = start_value,
        format = "dd/mm/yyyy",
        language = "es",
        weekstart = 1,
        min = min_date,
        max = max_date,
        width = "100%"
      )
    ),
    shiny::div(
      class = "calendar-range-field",
      shiny::tags$label(`for` = end_id, end_label),
      shiny::dateInput(
        end_id,
        label = NULL,
        value = end_value,
        format = "dd/mm/yyyy",
        language = "es",
        weekstart = 1,
        min = min_date,
        max = max_date,
        width = "100%"
      )
    )
  )
}

origenes_periodo_filter_ui <- function(prefix) {
  origenes_filter_bar(
    origenes_filter_col(
      min_width = "180px",
      shiny::selectInput(
        paste0(prefix, "_granularity"),
        "Periodo",
        choices = origenes_granularity_choices(),
        selected = "mensual",
        width = "100%"
      )
    ),
    shiny::div(
      class = "or-filter-col or-filter-col--date-range",
      style = "min-width:420px;",
      origenes_calendar_range_ui(
        paste0(prefix, "_date_start"),
        paste0(prefix, "_date_end")
      )
    )
  )
}

origenes_origin_shell <- function(kicker, title, filter_prefix, subtab_id, panels) {
  shiny::div(
    class = "or-page or-dashboard",
    shiny::div(
      class = "or-dashboard__header",
      shiny::div(
        shiny::span(class = "or-kicker", kicker),
        shiny::h1(title)
      )
    ),
    origenes_periodo_filter_ui(filter_prefix),
    do.call(
      bslib::navset_underline,
      c(list(id = subtab_id), panels)
    )
  )
}

origenes_detail_panel_shell <- function(kind_label,
                                        title,
                                        meta,
                                        header_id,
                                        table_id,
                                        insights_ui = NULL,
                                        table_kind = c("dt", "ventas_cierre")) {
  table_kind <- match.arg(table_kind)
  table_wrap <- if (identical(table_kind, "ventas_cierre")) {
    shiny::tagList(
      shiny::tags$p(
        class = "or-drill-modal-hint or-ventas-table-hint",
        "Cada bloque con borde agrupa las unidades del mismo cierre (misma venta)."
      ),
      shiny::div(
        class = "or-metric-table-wrap or-ventas-table-wrap or-drill-reactable-wrap",
        reactable::reactableOutput(table_id, width = "100%")
      )
    )
  } else {
    shiny::div(
      class = "or-metric-table-wrap or-ventas-table-wrap",
      DT::DTOutput(table_id, width = "100%")
    )
  }
  shiny::div(
    class = paste0("or-", tolower(kind_label)),
    shiny::uiOutput(header_id),
    insights_ui,
    bslib::card(
      class = "or-card or-metric-card",
      bslib::card_header(
        shiny::div(
          shiny::span(class = "or-card__eyebrow", "DETALLE"),
          shiny::h3(title)
        ),
        shiny::span(class = "or-card__meta", meta)
      ),
      table_wrap
    )
  )
}

origenes_citas_insights_ui <- function(prefix, include_postgrad = FALSE) {
  shiny::tagList(
    shiny::uiOutput(paste0(prefix, "_citas_kpis")),
    shiny::div(
      class = "or-viz-grid or-viz-grid--2",
      origenes_chart_card(
        "Estatus por generación",
        "Últimos 6 meses · clic para ver detalle",
        plotly::plotlyOutput(paste0(prefix, "_citas_chart_estatus"), height = "290px")
      ),
      origenes_chart_card(
        "Top embajadores",
        "Por volumen de citas",
        plotly::plotlyOutput(paste0(prefix, "_citas_chart_emb"), height = "290px")
      )
    ),
    shiny::div(
      class = if (include_postgrad) "or-viz-grid or-viz-grid--2" else "or-viz-grid or-viz-grid--1",
      origenes_chart_card(
        "Primera cita",
        "% de citas que son la 1ª del prospecto · clic para ver detalle",
        shiny::uiOutput(paste0(prefix, "_citas_primera_progress"))
      ),
      if (include_postgrad) {
        origenes_chart_card(
          "Post graduación",
          "% de citas después de graduarse · clic para ver detalle",
          shiny::uiOutput(paste0(prefix, "_citas_postgrad_progress"))
        )
      }
    )
  )
}

origenes_ventas_insights_ui <- function(prefix) {
  shiny::tagList(
    shiny::uiOutput(paste0(prefix, "_ventas_kpis")),
    shiny::div(
      class = "or-viz-grid or-viz-grid--2",
      origenes_chart_card(
        "Facturación por proyecto",
        "Clic para ver detalle",
        plotly::plotlyOutput(paste0(prefix, "_ventas_chart_proyecto"), height = "290px")
      ),
      origenes_chart_card(
        "Gen Venta",
        "Últimos 6 meses · unidades y facturación",
        plotly::plotlyOutput(paste0(prefix, "_ventas_chart_gen"), height = "290px")
      )
    ),
    shiny::div(
      class = "or-viz-grid or-viz-grid--2",
      origenes_chart_card(
        "Ciclo de venta",
        "Días entre primera cita y firma",
        plotly::plotlyOutput(paste0(prefix, "_ventas_chart_ciclo"), height = "290px")
      ),
      origenes_chart_card(
        "Top vendedores",
        "Por facturación firmada",
        plotly::plotlyOutput(paste0(prefix, "_ventas_chart_vendedor"), height = "290px")
      )
    )
  )
}

origenes_resumen_insights_ui <- function(prefix) {
  shiny::tagList(
    shiny::div(
      class = "or-viz-grid or-viz-grid--2",
      origenes_chart_card(
        "Evolución del canal",
        NULL,
        shiny::div(
          class = "or-series-controls",
          shiny::checkboxGroupInput(
            paste0(prefix, "_resumen_series_metrics"),
            label = NULL,
            choices = origenes_resumen_series_choices(),
            selected = origenes_resumen_series_defaults(),
            inline = TRUE
          )
        ),
        plotly::plotlyOutput(paste0(prefix, "_resumen_chart_series"), height = "300px")
      ),
      origenes_chart_card(
        "Conversiones por periodo",
        NULL,
        plotly::plotlyOutput(paste0(prefix, "_resumen_chart_conv"), height = "300px")
      )
    )
  )
}

origenes_nxtgen_ui <- function() {
  origenes_origin_shell(
    kicker = "ORIGEN 01",
    title = "NxtGen",
    filter_prefix = "nxtgen",
    subtab_id = "nxtgen_subtab",
    panels = list(
      bslib::nav_panel(
        "Resumen",
        value = "resumen",
        shiny::uiOutput("nxtgen_resumen_header"),
        origenes_resumen_insights_ui("nxtgen"),
        origenes_metric_table_ui(
          "Embajadores",
          "Cohorte por periodo de registro",
          "nxtgen_tbl_embajadores"
        ),
        origenes_metric_table_ui(
          "Resultados",
          "Volumen por periodo de cita / venta",
          "nxtgen_tbl_resultados"
        ),
        origenes_metric_table_ui(
          "Conversión Citas",
          "Cita realizada → cierre firmado",
          "nxtgen_tbl_conversion"
        )
      ),
      bslib::nav_panel(
        "Citas",
        value = "citas",
        origenes_detail_panel_shell(
          "CITAS", "Citas", "Bubble RSCG · Origin_OS del canal",
          "nxtgen_citas_header", "nxtgen_tbl_citas",
          insights_ui = origenes_citas_insights_ui("nxtgen", include_postgrad = FALSE)
        )
      ),
      bslib::nav_panel(
        "Ventas",
        value = "ventas",
        origenes_detail_panel_shell(
          "VENTAS", "Ventas", "Bubble RSCG · una fila por unidad · agrupado por cierre",
          "nxtgen_ventas_header", "nxtgen_tbl_ventas",
          insights_ui = origenes_ventas_insights_ui("nxtgen"),
          table_kind = "ventas_cierre"
        )
      )
    )
  )
}

origenes_brokers_ui <- function() {
  origenes_origin_shell(
    kicker = "ORIGEN 02",
    title = "Brokers",
    filter_prefix = "brokers",
    subtab_id = "brokers_subtab",
    panels = list(
      bslib::nav_panel(
        "Resumen",
        value = "resumen",
        shiny::uiOutput("brokers_resumen_header"),
        origenes_resumen_insights_ui("brokers"),
        origenes_metric_table_ui(
          "Conversión Embajadores",
          "Cohorte por periodo de registro",
          "brokers_tbl_embajadores"
        ),
        origenes_metric_table_ui(
          "Actividad",
          "Volumen por periodo de cita / venta",
          "brokers_tbl_resultados"
        ),
        origenes_metric_table_ui(
          "Conversión Citas",
          "Cita realizada → cierre firmado",
          "brokers_tbl_conversion"
        )
      ),
      bslib::nav_panel(
        "Citas",
        value = "citas",
        origenes_detail_panel_shell(
          "CITAS", "Citas", "Bubble RSCG · Origin_OS del canal",
          "brokers_citas_header", "brokers_tbl_citas",
          insights_ui = origenes_citas_insights_ui("brokers", include_postgrad = TRUE)
        )
      ),
      bslib::nav_panel(
        "Ventas",
        value = "ventas",
        origenes_detail_panel_shell(
          "VENTAS", "Ventas", "Bubble RSCG · una fila por unidad · agrupado por cierre",
          "brokers_ventas_header", "brokers_tbl_ventas",
          insights_ui = origenes_ventas_insights_ui("brokers"),
          table_kind = "ventas_cierre"
        )
      )
    )
  )
}

origenes_ui <- function() {
  bslib::page_navbar(
    id = "main_nav",
    title = origenes_brand(),
    theme = origenes_theme(),
    fillable = FALSE,
    window_title = "Origin Analytics | División 2c",
    header = shiny::tags$head(
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=29"),
      shiny::tags$script(htmltools::HTML(paste(
        "Shiny.addCustomMessageHandler('origenes-htmlwidgets-static-render', function(message) {",
        "  setTimeout(function() {",
        "    if (window.HTMLWidgets && HTMLWidgets.staticRender) { HTMLWidgets.staticRender(); }",
        "    window.dispatchEvent(new Event('resize'));",
        "  }, 80);",
        "});",
        sep = "\n"
      )))
    ),
    bslib::nav_panel("Inicio", value = "inicio", origenes_home_ui()),
    bslib::nav_panel(
      shiny::span("NxtGen", class = "or-nav-hidden-label"),
      value = "nxtgen",
      origenes_nxtgen_ui()
    ),
    bslib::nav_panel(
      shiny::span("Brokers", class = "or-nav-hidden-label"),
      value = "brokers",
      origenes_brokers_ui()
    )
  )
}
