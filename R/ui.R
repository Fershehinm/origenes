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

origenes_resumen_section <- function(kicker, title, ..., subtitle = NULL) {
  shiny::div(
    class = "or-resumen-section",
    shiny::div(
      class = "or-resumen-section__head",
      shiny::span(class = "or-resumen-section__kicker", kicker),
      shiny::h2(class = "or-resumen-section__title", title),
      if (!is.null(subtitle) && is.character(subtitle) && nzchar(subtitle)) {
        shiny::p(class = "or-resumen-section__sub", subtitle)
      }
    ),
    shiny::div(class = "or-resumen-grid", ...)
  )
}

origenes_metric_table_ui <- function(title, subtitle = NULL, output_id) {
  bslib::card(
    class = "or-card or-metric-card",
    bslib::card_header(
      shiny::h3(title),
      if (!is.null(subtitle) && nzchar(subtitle)) {
        shiny::span(class = "or-card__meta", subtitle)
      }
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
                                       max_date = NULL,
                                       origin_key = NULL) {
  defaults <- origenes_default_date_range_for_granularity(
    "anual",
    origin_key = origin_key
  )
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
  origin_key <- if (identical(prefix, "brokers")) "broker" else NULL
  origenes_filter_bar(
    origenes_filter_col(
      min_width = "180px",
      shiny::selectInput(
        paste0(prefix, "_granularity"),
        "Periodo",
        choices = origenes_granularity_choices(),
        selected = "anual",
        width = "100%"
      )
    ),
    shiny::div(
      class = "or-filter-col or-filter-col--date-range",
      style = "min-width:420px;",
      origenes_calendar_range_ui(
        paste0(prefix, "_date_start"),
        paste0(prefix, "_date_end"),
        origin_key = origin_key
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
                                        meta = NULL,
                                        header_id,
                                        table_id,
                                        insights_ui = NULL,
                                        metric_ui = NULL,
                                        table_kind = c("dt", "ventas_cierre")) {
  table_kind <- match.arg(table_kind)
  table_wrap <- if (identical(table_kind, "ventas_cierre")) {
    shiny::tagList(
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
    # header_id se conserva por compatibilidad; ya no mostramos la franja de fechas.
    insights_ui,
    metric_ui,
    bslib::card(
      class = "or-card or-metric-card",
      bslib::card_header(
        shiny::div(
          shiny::span(class = "or-card__eyebrow", "DETALLE"),
          shiny::h3(title)
        ),
        if (!is.null(meta) && nzchar(meta)) shiny::span(class = "or-card__meta", meta)
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
        plotly::plotlyOutput(paste0(prefix, "_citas_chart_estatus"), height = "290px"),
        subtitle = "Últimos 6 meses"
      ),
      origenes_chart_card(
        "Top embajadores",
        plotly::plotlyOutput(paste0(prefix, "_citas_chart_emb"), height = "290px")
      )
    ),
    shiny::div(
      class = if (include_postgrad) "or-viz-grid or-viz-grid--2" else "or-viz-grid or-viz-grid--1",
      origenes_chart_card(
        "Primera cita",
        shiny::uiOutput(paste0(prefix, "_citas_primera_progress"))
      ),
      if (include_postgrad) {
        origenes_chart_card(
          "Post graduación",
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
        plotly::plotlyOutput(paste0(prefix, "_ventas_chart_proyecto"), height = "290px")
      ),
      origenes_chart_card(
        "Gen Venta",
        plotly::plotlyOutput(paste0(prefix, "_ventas_chart_gen"), height = "290px"),
        subtitle = "Últimos 6 meses"
      )
    ),
    shiny::div(
      class = "or-viz-grid or-viz-grid--2",
      origenes_chart_card(
        "Ciclo de venta",
        plotly::plotlyOutput(paste0(prefix, "_ventas_chart_ciclo"), height = "290px")
      ),
      origenes_chart_card(
        "Top vendedores",
        plotly::plotlyOutput(paste0(prefix, "_ventas_chart_vendedor"), height = "290px")
      )
    )
  )
}

origenes_resumen_insights_ui <- function(prefix) {
  shiny::conditionalPanel(
    condition = sprintf("input['%s_granularity'] === 'anual'", prefix),
    shiny::div(
      class = "or-viz-grid or-viz-grid--1 or-resumen-charts",
      origenes_chart_card(
        "Evolución del canal",
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
        "Analytics",
        value = "resumen",
        shiny::uiOutput("nxtgen_resumen_header"),
        origenes_resumen_insights_ui("nxtgen")
      ),
      bslib::nav_panel(
        "Embajadores",
        value = "embajadores",
        origenes_metric_table_ui("Embajadores", output_id = "nxtgen_tbl_embajadores")
      ),
      bslib::nav_panel(
        "Citas",
        value = "citas",
        origenes_detail_panel_shell(
          "CITAS", "Citas",
          header_id = "nxtgen_citas_header", table_id = "nxtgen_tbl_citas",
          insights_ui = origenes_citas_insights_ui("nxtgen", include_postgrad = FALSE),
          metric_ui = origenes_metric_table_ui(
            "Citas y actividad",
            output_id = "nxtgen_tbl_resultados"
          )
        )
      ),
      bslib::nav_panel(
        "Ventas",
        value = "ventas",
        origenes_detail_panel_shell(
          "VENTAS", "Ventas",
          header_id = "nxtgen_ventas_header", table_id = "nxtgen_tbl_ventas",
          insights_ui = origenes_ventas_insights_ui("nxtgen"),
          metric_ui = origenes_metric_table_ui(
            "Ventas y conversión",
            output_id = "nxtgen_tbl_conversion"
          ),
          table_kind = "ventas_cierre"
        )
      )
    )
  )
}

origenes_brokers_embajadores_ui <- function() {
  canal_opts <- origenes_brokers_canal_choices()
  col_choices <- c(
    "(elegir columna)" = "",
    "Fecha de registro" = ".fecha_reg",
    "Nombre" = "Nombre",
    "Pertenece a un equipo" = "Pertenece a un equipo",
    "Role" = "Role",
    "Conciliacion Nombre" = "Conciliacion Nombre",
    "Canal" = "Canal",
    "Fecha graduación" = "Fecha graduación",
    "Generación" = "Generación",
    "Fecha primera cita realizada" = "Fecha primera cita realizada",
    "Fecha de última cita realizada" = "Fecha de última cita realizada",
    "Gen Registro" = "Gen Registro",
    "Días desde la última cita realizadas" = "Días desde la última cita realizadas",
    "Fecha de última cita agendada/realizada" = "Fecha de última cita agendada/realizada",
    "Gen primera cita realizada" = "Gen primera cita realizada",
    "Ciclo Registro / Primera Cita" = "Ciclo Registro / Primera Cita"
  )
  shiny::div(
    class = "or-brokers-roster",
    # Datalists únicos (no repetir <option> en cada fila).
    htmltools::HTML(origenes_brokers_datalist_html("brokers_gen_choices", paste("Gen", seq_len(5L)))),
    htmltools::HTML(origenes_brokers_datalist_html("brokers_canal_choices", canal_opts)),
    shiny::uiOutput("brokers_crm_datalist"),
    shiny::div(
      class = "or-brokers-roster__toolbar",
      shiny::div(
        class = "or-brokers-roster__tools",
        shiny::selectInput(
          "brokers_roster_col",
          "Columna",
          choices = col_choices,
          selected = "",
          width = "220px"
        ),
        shiny::conditionalPanel(
          condition = "input.brokers_roster_col && input.brokers_roster_col.length > 0",
          shiny::div(
            class = "or-brokers-roster__tools-col",
            shiny::textInput(
              "brokers_roster_col_filter",
              "Filtrar",
              value = "",
              placeholder = "Contiene…",
              width = "200px"
            )
          )
        )
      )
    ),
    bslib::card(
      class = "or-card or-metric-card or-brokers-roster-card",
      bslib::card_header(shiny::h3("Embajadores")),
      shiny::div(
        class = "or-metric-table-wrap or-brokers-roster__table",
        reactable::reactableOutput("brokers_roster_table", width = "100%")
      )
    )
  )
}

origenes_brokers_persona_filter_ui <- function() {
  origenes_filter_bar(
    origenes_filter_col(
      min_width = "220px",
      shiny::selectizeInput(
        "brokers_filter_canal",
        "Canal",
        choices = c("Todos" = ""),
        selected = "",
        width = "100%",
        options = list(placeholder = "Todos")
      )
    ),
    origenes_filter_col(
      min_width = "280px",
      grow = TRUE,
      shiny::selectizeInput(
        "brokers_filter_embajador",
        "Embajador",
        choices = c("Todos" = ""),
        selected = "",
        width = "100%",
        options = list(placeholder = "Todos")
      )
    )
  )
}

origenes_brokers_ui <- function() {
  shiny::div(
    class = "or-page or-dashboard",
    shiny::div(
      class = "or-dashboard__header",
      shiny::div(
        shiny::span(class = "or-kicker", "ORIGEN 02"),
        shiny::h1("Brokers")
      )
    ),
    origenes_periodo_filter_ui("brokers"),
    # Canal / Embajador en Analytics, Citas y Ventas (no en roster Embajadores).
    shiny::conditionalPanel(
      condition = "input.brokers_subtab !== 'embajadores'",
      origenes_brokers_persona_filter_ui()
    ),
    do.call(
      bslib::navset_underline,
      list(
        id = "brokers_subtab",
        bslib::nav_panel(
          "Analytics",
          value = "resumen",
          origenes_metric_table_ui("Embajadores", output_id = "brokers_tbl_embajadores"),
          origenes_metric_table_ui("Actividad", output_id = "brokers_tbl_resultados"),
          origenes_metric_table_ui("Citas por generación", output_id = "brokers_tbl_conversion")
        ),
        bslib::nav_panel(
          "Embajadores",
          value = "embajadores",
          origenes_brokers_embajadores_ui()
        ),
        bslib::nav_panel(
          "Citas",
          value = "citas",
          origenes_detail_panel_shell(
            "CITAS", "Citas",
            header_id = "brokers_citas_header", table_id = "brokers_tbl_citas",
            insights_ui = origenes_citas_insights_ui("brokers", include_postgrad = TRUE)
          )
        ),
        bslib::nav_panel(
          "Ventas",
          value = "ventas",
          origenes_detail_panel_shell(
            "VENTAS", "Ventas",
            header_id = "brokers_ventas_header", table_id = "brokers_tbl_ventas",
            insights_ui = origenes_ventas_insights_ui("brokers"),
            table_kind = "ventas_cierre"
          )
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
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=53"),
      shiny::tags$script(htmltools::HTML(paste(
        "window.origenesRosterFocusInput = function(el, ev) {",
        "  if (ev && ev.stopPropagation) ev.stopPropagation();",
        "  if (!el) return;",
        "  el.focus();",
        "  try { el.select(); } catch (e) {}",
        "  if (el.list) {",
        "    var v = el.value;",
        "    el.value = '';",
        "    try { el.dispatchEvent(new Event('input', { bubbles: true })); } catch (e2) {}",
        "    el.value = v;",
        "  }",
        "};",
        "document.addEventListener('mousedown', function(ev) {",
        "  var t = ev.target;",
        "  if (!t || !t.closest) return;",
        "  var td = t.closest('.or-brokers-roster__table .rt-td');",
        "  if (!td) return;",
        "  var input = t.classList && t.classList.contains('or-roster-input')",
        "    ? t",
        "    : td.querySelector('.or-roster-input');",
        "  if (!input) return;",
        "  ev.stopPropagation();",
        "}, true);",
        "document.addEventListener('click', function(ev) {",
        "  var t = ev.target;",
        "  if (!t || !t.closest) return;",
        "  var td = t.closest('.or-brokers-roster__table .rt-td');",
        "  if (!td) return;",
        "  var input = t.classList && t.classList.contains('or-roster-input')",
        "    ? t",
        "    : td.querySelector('.or-roster-input');",
        "  if (!input) return;",
        "  if (t !== input) { window.origenesRosterFocusInput(input, ev); }",
        "}, true);",
        "Shiny.addCustomMessageHandler('origenes-htmlwidgets-static-render', function(message) {",
        "  setTimeout(function() {",
        "    if (window.HTMLWidgets && HTMLWidgets.staticRender) { HTMLWidgets.staticRender(); }",
        "    window.dispatchEvent(new Event('resize'));",
        "  }, 80);",
        "});",
        "document.addEventListener('toggle', function(ev) {",
        "  var t = ev.target;",
        "  if (!t || !t.classList || !t.classList.contains('or-disclose') || !t.open) return;",
        "  setTimeout(function() {",
        "    if (window.jQuery && jQuery.fn.dataTable) {",
        "      jQuery(t).find('table.dataTable').each(function() {",
        "        if (jQuery.fn.dataTable.isDataTable(this)) {",
        "          jQuery(this).DataTable().columns.adjust();",
        "        }",
        "      });",
        "    }",
        "    window.dispatchEvent(new Event('resize'));",
        "  }, 60);",
        "}, true);",
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
