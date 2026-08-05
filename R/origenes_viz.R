# Visuales Plotly + drills hacia tablas (Orígenes).

origenes_viz_colors <- function() {
  list(
    green = "#1f6a4a",
    green_soft = "#d7ebe0",
    gold = "#d9a441",
    ink = "#17231d",
    muted = "#6b726c",
    line = "#e4dfd5",
    surface = "#fffdf8",
    series = c("#1f6a4a", "#d9a441", "#2f6f8f", "#8c4b2f", "#5b6b4a", "#6b5b95")
  )
}

origenes_plotly_base <- function() {
  cols <- origenes_viz_colors()
  list(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "DM Sans, sans-serif", color = cols$ink, size = 12),
    legend = list(orientation = "h", y = 1.14, x = 0, font = list(size = 11)),
    hoverlabel = list(bgcolor = cols$surface, font = list(family = "DM Sans, sans-serif"))
  )
}

origenes_plotly_finish <- function(p, source = NULL, margin = NULL) {
  base <- origenes_plotly_base()
  if (!is.null(margin)) {
    base$margin <- margin
  }
  p <- do.call(plotly::layout, c(list(p), base))
  plotly::config(p, displayModeBar = FALSE, responsive = TRUE)
}

origenes_plotly_empty <- function(message = "Sin datos en el periodo", source = NULL) {
  cols <- origenes_viz_colors()
  p <- plotly::plot_ly(source = source) |>
    plotly::layout(
      annotations = list(list(
        text = message,
        xref = "paper",
        yref = "paper",
        x = 0.5,
        y = 0.5,
        showarrow = FALSE,
        font = list(color = cols$muted, size = 14)
      )),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
  origenes_plotly_finish(p, source = source)
}

origenes_chart_card <- function(title, subtitle = NULL, ...) {
  bslib::card(
    class = "or-card or-chart-card",
    bslib::card_header(
      shiny::div(
        shiny::span(class = "or-card__eyebrow", "VISUAL"),
        shiny::h3(title)
      ),
      if (!is.null(subtitle) && nzchar(subtitle)) {
        shiny::span(class = "or-card__meta", subtitle)
      }
    ),
    shiny::div(class = "or-chart-card__body", ...)
  )
}

origenes_insight_card <- function(title, body) {
  shiny::div(
    class = "or-insight-card",
    shiny::span(class = "or-insight-card__eyebrow", "INSIGHT"),
    shiny::h3(title),
    shiny::p(body)
  )
}

origenes_mini_kpi <- function(label, value, note = NULL, accent = FALSE, id = NULL) {
  body <- list(
    shiny::span(class = "or-mini-kpi__label", label),
    shiny::span(class = "or-mini-kpi__value", value),
    if (!is.null(note)) shiny::span(class = "or-mini-kpi__note", note)
  )
  if (is.null(id)) {
    return(shiny::div(
      class = paste("or-mini-kpi", if (accent) "or-mini-kpi--accent" else NULL),
      body
    ))
  }
  shiny::tags$button(
    id = id,
    class = paste(
      "action-button or-mini-kpi or-mini-kpi--clickable",
      if (accent) "or-mini-kpi--accent" else NULL
    ),
    type = "button",
    title = "Clic para ver detalle",
    body
  )
}

origenes_drill_chip_ui <- function(drill, clear_id) {
  if (is.null(drill) || !nzchar(drill$label %||% "")) {
    return(NULL)
  }
  shiny::div(
    class = "or-drill-chip",
    shiny::span(class = "or-drill-chip__label", "FILTRO ACTIVO"),
    shiny::span(class = "or-drill-chip__value", drill$label),
    shiny::actionButton(
      clear_id,
      label = "Limpiar",
      class = "or-drill-chip__clear btn btn-sm"
    )
  )
}

#' Crea o alterna un drill (mismo label → limpia).
origenes_drill_set <- function(current, rules, label) {
  rules <- Filter(function(r) {
    !is.null(r$field) && !is.null(r$value) && !is.na(r$value) && nzchar(as.character(r$value))
  }, rules)
  if (!length(rules)) {
    return(NULL)
  }
  new <- list(rules = rules, label = label)
  if (!is.null(current) && identical(current$label, label)) {
    return(NULL)
  }
  new
}

origenes_apply_drill <- function(df, drill) {
  if (is.null(df) || !is.data.frame(df) || is.null(drill) || !length(drill$rules)) {
    return(df)
  }
  out <- df
  for (r in drill$rules) {
    field <- r$field
    # Ciclo bucket virtual (Ventas)
    if (identical(field, ".ciclo_bucket")) {
      out <- origenes_ventas_apply_ciclo_bucket(out, r$value)
      next
    }
    if (!field %in% names(out)) {
      next
    }
    want <- as.character(r$value)
    # Map labels de pie charts a valores de tabla
    if (field %in% c("Primera Cita", "¿Fue Post Graduación?")) {
      want <- dplyr::case_when(
        origenes_normalize_key(want) %in% c("si", "sí", "yes") ~ "si",
        origenes_normalize_key(want) %in% c("no") ~ "no",
        TRUE ~ tolower(want)
      )
    }
    got <- as.character(out[[field]])
    if (field %in% c("Primera Cita", "¿Fue Post Graduación?")) {
      got <- origenes_normalize_key(got)
      want <- origenes_normalize_key(want)
      out <- out[!is.na(got) & got == want, , drop = FALSE]
      next
    }
    if (identical(field, "Estatus")) {
      got <- origenes_normalize_key(got)
      want <- origenes_normalize_key(want)
      out <- out[!is.na(got) & got == want, , drop = FALSE]
      next
    }
    # Placeholders de charts para valores vacíos
    if (want %in% c("(sin gen)", "(sin embajador)", "(sin proyecto)", "(sin vendedor)", "(sin estatus)", "(sin dato)")) {
      out <- out[is.na(got) | !nzchar(stringr::str_squish(got)), , drop = FALSE]
      next
    }
    out <- out[!is.na(got) & got == want, , drop = FALSE]
  }
  out
}

#' Recorta tablas Resumen wide por periodo y/o filas de métrica.
origenes_resumen_table_drill <- function(tbl, drill = NULL, period_label = NULL) {
  if (is.null(tbl) || !is.data.frame(tbl)) {
    return(tbl)
  }
  # Compat: period_label suelto
  if (is.null(drill) && !is.null(period_label) && nzchar(period_label)) {
    drill <- list(rules = list(list(field = "periodo", value = period_label)))
  }
  if (is.null(drill) || !length(drill$rules)) {
    return(tbl)
  }

  out <- tbl
  period <- NULL
  metric_patterns <- character()
  for (r in drill$rules) {
    if (identical(r$field, "periodo")) {
      period <- as.character(r$value)
    }
    if (identical(r$field, "metrica")) {
      metric_patterns <- c(metric_patterns, as.character(r$value))
    }
  }
  if (length(metric_patterns)) {
    keep_rows <- Reduce(
      `|`,
      lapply(metric_patterns, function(p) grepl(p, out$metrica, ignore.case = TRUE))
    )
    out <- out[keep_rows, , drop = FALSE]
  }
  if (!is.null(period) && nzchar(period)) {
    keep <- intersect(c("metrica", "Total", period), names(out))
    if (length(keep)) {
      out <- out[, keep, drop = FALSE]
    }
  }
  out
}

origenes_parse_click_key <- function(ed) {
  if (is.null(ed) || !nrow(ed)) {
    return(NULL)
  }
  row <- ed[1, , drop = FALSE]
  key <- row$key %||% row$customdata %||% NA
  if (is.list(key)) {
    key <- key[[1]]
  }
  list(
    x = if (!is.null(row$x)) as.character(row$x[[1]]) else NA_character_,
    y = if (!is.null(row$y)) as.character(row$y[[1]]) else NA_character_,
    key = if (length(key) && !is.na(key[1])) as.character(key[1]) else NA_character_
  )
}

#' Tras abrir un modal con htmlwidgets, fuerza el render JS.
origenes_htmlwidgets_static_render <- function(session) {
  if (is.null(session)) {
    return(invisible(NULL))
  }
  session$onFlushed(function() {
    session$sendCustomMessage("origenes-htmlwidgets-static-render", list())
  }, once = TRUE)
  invisible(NULL)
}

#' Reactable genérico para detalle (citas / pivots) dentro de modales.
origenes_detail_reactable <- function(tbl) {
  if (is.null(tbl) || !is.data.frame(tbl)) {
    tbl <- data.frame()
  }
  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE, check.names = FALSE)
  n <- nrow(tbl)
  reactable::reactable(
    tbl,
    searchable = n > 12L,
    pagination = n > 25L,
    defaultPageSize = 25L,
    showPageSizeOptions = n > 50L,
    pageSizeOptions = c(25L, 50L, 100L),
    highlight = TRUE,
    bordered = TRUE,
    compact = TRUE,
    wrap = FALSE,
    resizable = TRUE,
    fullWidth = TRUE,
    defaultColDef = reactable::colDef(minWidth = 120, na = "—", html = FALSE)
  )
}

#' Modal de detalle para citas (reactable + static render).
origenes_citas_drill_show_modal <- function(title, tbl, session = NULL) {
  title_txt <- sprintf("Detalle · %s", title)
  if (is.null(tbl) || !is.data.frame(tbl) || !nrow(tbl)) {
    shiny::showModal(shiny::modalDialog(
      title = title_txt,
      shiny::tags$p(class = "or-drill-empty", "Sin citas para este cruce."),
      easyClose = TRUE,
      size = "l",
      footer = shiny::modalButton("Cerrar")
    ))
    return(invisible(NULL))
  }
  shiny::showModal(shiny::modalDialog(
    title = title_txt,
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

#' Modal de detalle Resumen (tablas pivot filtradas por periodo/métrica).
origenes_resumen_drill_show_modal <- function(title, bundle, drill, session = NULL) {
  title_txt <- sprintf("Detalle · %s", title)
  if (is.null(bundle)) {
    shiny::showModal(shiny::modalDialog(
      title = title_txt,
      shiny::tags$p(class = "or-drill-empty", "Sin datos."),
      easyClose = TRUE,
      size = "l",
      footer = shiny::modalButton("Cerrar")
    ))
    return(invisible(NULL))
  }
  sections <- list(
    list(label = "Embajadores", tbl = origenes_resumen_table_drill(bundle$tabla_embajadores, drill)),
    list(label = "Resultados", tbl = origenes_resumen_table_drill(bundle$tabla_resultados, drill)),
    list(label = "Conversión", tbl = origenes_resumen_table_drill(bundle$tabla_conversion, drill))
  )
  body <- lapply(sections, function(sec) {
    shiny::tagList(
      shiny::h4(class = "or-drill-modal-section", sec$label),
      htmltools::div(
        class = "or-drill-reactable-wrap",
        style = "margin-bottom:1.25rem;",
        origenes_detail_reactable(sec$tbl)
      )
    )
  })
  shiny::showModal(shiny::modalDialog(
    title = title_txt,
    shiny::tags$p(
      class = "or-drill-modal-hint",
      if (!is.null(drill$label) && nzchar(drill$label)) drill$label else "Selección actual"
    ),
    htmltools::div(style = "max-height:70vh;overflow:auto;", body),
    easyClose = TRUE,
    size = "l",
    footer = shiny::modalButton("Cerrar")
  ))
  origenes_htmlwidgets_static_render(session)
  invisible(NULL)
}

#' Aplica reglas y abre modal de citas.
origenes_citas_open_drill <- function(base_tbl, rules, label, session = NULL) {
  rules <- Filter(function(r) {
    !is.null(r$field) && !is.null(r$value) && !is.na(r$value) && nzchar(as.character(r$value))
  }, rules)
  filtered <- if (length(rules)) {
    origenes_apply_drill(base_tbl, list(rules = rules, label = label))
  } else {
    base_tbl
  }
  origenes_citas_drill_show_modal(label, filtered, session = session)
}

#' Aplica reglas y abre modal de ventas.
origenes_ventas_open_drill <- function(base_tbl, rules, label, channel_title, session = NULL) {
  rules <- Filter(function(r) {
    !is.null(r$field) && !is.null(r$value) && !is.na(r$value) && nzchar(as.character(r$value))
  }, rules)
  filtered <- if (length(rules)) {
    origenes_apply_drill(base_tbl, list(rules = rules, label = label))
  } else {
    base_tbl
  }
  origenes_ventas_drill_show_modal(channel_title, filtered, metric_lbl = label, session = session)
}
