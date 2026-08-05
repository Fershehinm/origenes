# Insights / agregaciones para visuales de Citas, Ventas y Resumen.

origenes_pct_label <- function(num, den, digits = 0) {
  if (is.na(den) || den <= 0) {
    return("—")
  }
  paste0(round(100 * num / den, digits), "%")
}

# --- Citas -------------------------------------------------------------------

origenes_citas_kpis <- function(tbl) {
  n <- nrow(tbl)
  if (!n) {
    return(list(
      total = 0L,
      realizadas = 0L,
      pct_realizadas = NA_real_,
      primeras = 0L,
      pct_primeras = NA_real_,
      top_embajador = "—",
      top_n = 0L,
      post_grad = 0L,
      pct_post = NA_real_,
      has_post = "¿Fue Post Graduación?" %in% names(tbl)
    ))
  }
  est <- origenes_normalize_key(tbl$Estatus)
  realizadas <- sum(est == "realizada", na.rm = TRUE)
  primeras <- sum(origenes_normalize_key(tbl[["Primera Cita"]]) == "si", na.rm = TRUE)
  emb_col <- if ("Embajador Registrado" %in% names(tbl)) {
    "Embajador Registrado"
  } else {
    "Embajador"
  }
  emb <- stringr::str_squish(as.character(tbl[[emb_col]]))
  emb[is.na(emb) | !nzchar(emb)] <- "(sin embajador)"
  top <- sort(table(emb), decreasing = TRUE)
  has_post <- "¿Fue Post Graduación?" %in% names(tbl)
  post_n <- if (has_post) {
    sum(origenes_normalize_key(tbl[["¿Fue Post Graduación?"]]) == "si", na.rm = TRUE)
  } else {
    0L
  }
  list(
    total = n,
    realizadas = realizadas,
    pct_realizadas = if (n) realizadas / n else NA_real_,
    primeras = primeras,
    pct_primeras = if (n) primeras / n else NA_real_,
    top_embajador = names(top)[[1]],
    top_n = as.integer(top[[1]]),
    post_grad = post_n,
    pct_post = if (has_post && n) post_n / n else NA_real_,
    has_post = has_post,
    emb_col = emb_col
  )
}

origenes_citas_kpis_ui <- function(kpis, prefix) {
  emb_col <- kpis$emb_col %||% "Embajador Registrado"
  cards <- list(
    origenes_mini_kpi(
      "Citas", nxtgen_fmt_num(kpis$total), "en el periodo",
      accent = TRUE, id = paste0(prefix, "_kpi_citas_total")
    ),
    origenes_mini_kpi(
      "Realizadas",
      origenes_pct_label(kpis$realizadas, kpis$total),
      paste(nxtgen_fmt_num(kpis$realizadas), "citas"),
      id = paste0(prefix, "_kpi_citas_realizadas")
    ),
    origenes_mini_kpi(
      "Primera cita",
      origenes_pct_label(kpis$primeras, kpis$total),
      paste(nxtgen_fmt_num(kpis$primeras), "sí"),
      id = paste0(prefix, "_kpi_citas_primera")
    ),
    origenes_mini_kpi(
      "Top embajador",
      kpis$top_embajador,
      paste(nxtgen_fmt_num(kpis$top_n), "citas"),
      id = paste0(prefix, "_kpi_citas_top_emb")
    )
  )
  if (isTRUE(kpis$has_post)) {
    cards <- c(cards, list(
      origenes_mini_kpi(
        "Post graduación",
        origenes_pct_label(kpis$post_grad, kpis$total),
        paste(nxtgen_fmt_num(kpis$post_grad), "citas"),
        id = paste0(prefix, "_kpi_citas_postgrad")
      )
    ))
  }
  attr(cards, "emb_col") <- emb_col
  shiny::div(class = "or-mini-kpi-grid", cards)
}

origenes_ventas_kpis_ui <- function(kpis, prefix) {
  shiny::div(
    class = "or-mini-kpi-grid",
    origenes_mini_kpi(
      "Operaciones", nxtgen_fmt_num(kpis$firmados), "filas / unidades",
      accent = TRUE, id = paste0(prefix, "_kpi_ventas_ops")
    ),
    origenes_mini_kpi(
      "Facturación", origenes_money(kpis$facturacion, compact = TRUE), "precio firmado",
      id = paste0(prefix, "_kpi_ventas_fact")
    ),
    origenes_mini_kpi(
      "Ticket medio",
      if (is.finite(kpis$ticket)) origenes_money(kpis$ticket, compact = TRUE) else "—",
      "por unidad",
      id = paste0(prefix, "_kpi_ventas_ticket")
    ),
    origenes_mini_kpi(
      "Ciclo mediano",
      if (is.finite(kpis$ciclo_med)) paste0(round(kpis$ciclo_med), " d") else "—",
      "primera cita → firma",
      id = paste0(prefix, "_kpi_ventas_ciclo")
    )
  )
}

origenes_citas_chart_estatus <- function(tbl, source) {
  if (!nrow(tbl) || !"Gen Cita" %in% names(tbl)) {
    return(origenes_plotly_empty(source = source))
  }
  cols <- origenes_viz_colors()
  df <- tbl |>
    dplyr::mutate(
      gen = dplyr::coalesce(dplyr::na_if(stringr::str_squish(as.character(.data[["Gen Cita"]])), ""), "(sin gen)"),
      estatus = dplyr::coalesce(dplyr::na_if(stringr::str_squish(as.character(.data$Estatus)), ""), "(sin estatus)")
    ) |>
    dplyr::count(.data$gen, .data$estatus, name = "n")
  gens <- sort(unique(df$gen))
  statuses <- unique(df$estatus)
  p <- plotly::plot_ly(source = source)
  for (i in seq_along(statuses)) {
    st <- statuses[[i]]
    sub <- df[df$estatus == st, , drop = FALSE]
    y <- stats::setNames(rep(0, length(gens)), gens)
    y[as.character(sub$gen)] <- sub$n
    p <- plotly::add_trace(
      p,
      x = gens,
      y = as.numeric(y),
      name = st,
      type = "bar",
      orientation = "v",
      key = rep(st, length(gens)),
      customdata = gens,
      hovertemplate = paste0(st, "<br>%{x}: %{y}<extra></extra>"),
      marker = list(color = cols$series[[((i - 1L) %% length(cols$series)) + 1L]])
    )
  }
  p <- plotly::layout(
    p,
    barmode = "group",
    bargap = 0.2,
    bargroupgap = 0.08,
    yaxis = list(title = "Citas", separatethousands = TRUE),
    xaxis = list(title = "Gen Cita", type = "category", categoryorder = "category ascending"),
    legend = list(orientation = "h", y = 1.14, x = 0)
  )
  origenes_plotly_finish(p, source = source, margin = list(l = 48, r = 24, t = 48, b = 56))
}

origenes_citas_chart_embajadores <- function(tbl, source, top_n = 10L) {
  if (!nrow(tbl)) {
    return(origenes_plotly_empty(source = source))
  }
  cols <- origenes_viz_colors()
  emb_col <- if ("Embajador Registrado" %in% names(tbl)) "Embajador Registrado" else "Embajador"
  df <- tbl |>
    dplyr::mutate(
      emb = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data[[emb_col]])), ""),
        "(sin embajador)"
      )
    ) |>
    dplyr::count(.data$emb, name = "n") |>
    dplyr::arrange(dplyr::desc(.data$n)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::arrange(.data$n)
  p <- plotly::plot_ly(
    df,
    x = ~n,
    y = ~emb,
    type = "bar",
    orientation = "h",
    key = ~emb,
    customdata = ~emb,
    source = source,
    marker = list(color = cols$green),
    hovertemplate = "%{y}: %{x}<extra></extra>"
  ) |>
    plotly::layout(xaxis = list(title = "Citas"), yaxis = list(title = "", automargin = TRUE))
  origenes_plotly_finish(p, source = source)
}

origenes_citas_chart_primera <- function(tbl, source) {
  # Legacy plotly entry-point: prefer HTML progress UI.
  origenes_plotly_empty(source = source)
}

#' Barra de progreso: % de citas que son la primera del prospecto.
origenes_citas_primera_progress_ui <- function(tbl, prefix) {
  n <- nrow(tbl)
  if (!n || !"Primera Cita" %in% names(tbl)) {
    return(shiny::div(class = "or-progress-empty", "Sin datos de primera cita"))
  }
  key <- origenes_normalize_key(tbl[["Primera Cita"]])
  n_si <- sum(key == "si", na.rm = TRUE)
  n_no <- sum(key == "no", na.rm = TRUE)
  n_na <- n - n_si - n_no
  pct <- if (n > 0) round(100 * n_si / n) else 0
  pct_no <- if (n > 0) round(100 * n_no / n) else 0

  shiny::div(
    class = "or-progress-block",
    shiny::p(
      class = "or-progress-block__help",
      "De las citas del periodo, cuántas están marcadas como la primera del prospecto (campo Primera Cita)."
    ),
    shiny::div(
      class = "or-progress-track",
      title = NULL,
      shiny::tags$button(
        id = paste0(prefix, "_primera_si"),
        class = "action-button or-progress-fill or-progress-fill--si",
        type = "button",
        style = paste0("width:", pct, "%;"),
        if (pct >= 12) paste0(pct, "%") else NULL
      ),
      shiny::tags$button(
        id = paste0(prefix, "_primera_no"),
        class = "action-button or-progress-fill or-progress-fill--no",
        type = "button",
        style = paste0("width:", max(0, 100 - pct), "%;"),
        if (pct_no >= 12 && pct < 88) paste0(pct_no, "%") else NULL
      )
    ),
    shiny::div(
      class = "or-progress-meta",
      shiny::span(
        class = "or-progress-meta__main",
        paste0(nxtgen_fmt_num(n_si), " de ", nxtgen_fmt_num(n), " son primera cita (", pct, "%)")
      ),
      shiny::span(
        class = "or-progress-meta__sub",
        paste0(
          nxtgen_fmt_num(n_no), " no son primera",
          if (n_na > 0) paste0(" · ", nxtgen_fmt_num(n_na), " sin dato") else ""
        )
      )
    ),
    shiny::div(
      class = "or-progress-legend",
      shiny::span(class = "or-progress-legend__item or-progress-legend__item--si", "Sí · primera"),
      shiny::span(class = "or-progress-legend__item or-progress-legend__item--no", "No · follow-up")
    )
  )
}

origenes_citas_postgrad_progress_ui <- function(tbl, prefix) {
  col <- "¿Fue Post Graduación?"
  n <- nrow(tbl)
  if (!n || !col %in% names(tbl)) {
    return(shiny::div(class = "or-progress-empty", "Sin dato de graduación"))
  }
  key <- origenes_normalize_key(tbl[[col]])
  n_si <- sum(key == "si", na.rm = TRUE)
  n_no <- sum(key == "no", na.rm = TRUE)
  n_na <- n - n_si - n_no
  pct <- if (n > 0) round(100 * n_si / n) else 0
  pct_no <- if (n > 0) round(100 * n_no / n) else 0

  shiny::div(
    class = "or-progress-block",
    shiny::p(
      class = "or-progress-block__help",
      "Qué % de las citas ocurrieron después de la fecha de graduación del embajador."
    ),
    shiny::div(
      class = "or-progress-track",
      shiny::tags$button(
        id = paste0(prefix, "_postgrad_si"),
        class = "action-button or-progress-fill or-progress-fill--si",
        type = "button",
        style = paste0("width:", pct, "%;"),
        if (pct >= 12) paste0(pct, "%") else NULL
      ),
      shiny::tags$button(
        id = paste0(prefix, "_postgrad_no"),
        class = "action-button or-progress-fill or-progress-fill--no",
        type = "button",
        style = paste0("width:", max(0, 100 - pct), "%;"),
        if (pct_no >= 12 && pct < 88) paste0(pct_no, "%") else NULL
      )
    ),
    shiny::div(
      class = "or-progress-meta",
      shiny::span(
        class = "or-progress-meta__main",
        paste0(nxtgen_fmt_num(n_si), " de ", nxtgen_fmt_num(n), " son post graduación (", pct, "%)")
      ),
      shiny::span(
        class = "or-progress-meta__sub",
        paste0(
          nxtgen_fmt_num(n_no), " antes o sin graduar",
          if (n_na > 0) paste0(" · ", nxtgen_fmt_num(n_na), " sin dato") else ""
        )
      )
    )
  )
}

origenes_citas_chart_postgrad <- function(tbl, source) {
  origenes_plotly_empty("Sin dato de graduación", source = source)
}

# --- Ventas ------------------------------------------------------------------

origenes_ventas_kpis <- function(tbl) {
  n <- nrow(tbl)
  if (!n) {
    return(list(
      firmados = 0L,
      unidades = 0L,
      facturacion = 0,
      ticket = NA_real_,
      ciclo_med = NA_real_
    ))
  }
  precio <- suppressWarnings(as.numeric(tbl[["Precio de venta"]]))
  ciclo <- suppressWarnings(as.numeric(tbl[["Ciclo Venta"]]))
  list(
    firmados = n,
    unidades = n,
    facturacion = sum(precio, na.rm = TRUE),
    ticket = if (n) mean(precio, na.rm = TRUE) else NA_real_,
    ciclo_med = if (any(is.finite(ciclo))) stats::median(ciclo[is.finite(ciclo)], na.rm = TRUE) else NA_real_
  )
}

origenes_ventas_chart_proyecto <- function(tbl, source, top_n = 10L) {
  if (!nrow(tbl) || !"Proyecto" %in% names(tbl)) {
    return(origenes_plotly_empty(source = source))
  }
  cols <- origenes_viz_colors()
  df <- tbl |>
    dplyr::mutate(
      proyecto = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$Proyecto)), ""),
        "(sin proyecto)"
      ),
      precio = suppressWarnings(as.numeric(.data[["Precio de venta"]]))
    ) |>
    dplyr::group_by(.data$proyecto) |>
    dplyr::summarise(
      n = dplyr::n(),
      fact = sum(.data$precio, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$fact)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::arrange(.data$fact)
  p <- plotly::plot_ly(
    df,
    x = ~fact,
    y = ~proyecto,
    type = "bar",
    orientation = "h",
    key = ~proyecto,
    customdata = ~proyecto,
    source = source,
    marker = list(color = cols$green),
    hovertemplate = "%{y}<br>%{x:$,.0f} · %{customdata}<extra></extra>"
  ) |>
    plotly::layout(
      xaxis = list(title = "Facturación", tickprefix = "$", separatethousands = TRUE),
      yaxis = list(title = "", automargin = TRUE)
    )
  origenes_plotly_finish(p, source = source)
}

origenes_ventas_chart_gen <- function(tbl, source) {
  if (!nrow(tbl) || !"Gen Venta" %in% names(tbl)) {
    return(origenes_plotly_empty(source = source))
  }
  cols <- origenes_viz_colors()
  df <- tbl |>
    dplyr::mutate(
      gen = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data[["Gen Venta"]])), ""),
        "(sin gen)"
      ),
      precio = suppressWarnings(as.numeric(.data[["Precio de venta"]]))
    ) |>
    dplyr::group_by(.data$gen) |>
    dplyr::summarise(
      unidades = dplyr::n(),
      fact = sum(.data$precio, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$gen)
  p <- plotly::plot_ly(source = source) |>
    plotly::add_bars(
      data = df,
      x = ~gen,
      y = ~unidades,
      name = "Unidades",
      key = ~gen,
      customdata = ~gen,
      marker = list(color = cols$green),
      hovertemplate = "Unidades %{x}: %{y}<extra></extra>"
    ) |>
    plotly::add_trace(
      data = df,
      x = ~gen,
      y = ~fact,
      name = "Facturación",
      type = "scatter",
      mode = "lines+markers",
      yaxis = "y2",
      key = ~gen,
      customdata = ~gen,
      line = list(color = cols$gold, width = 2),
      marker = list(color = cols$gold, size = 7),
      hovertemplate = "Facturación %{x}: %{y:$,.0f}<extra></extra>"
    ) |>
    plotly::layout(
      yaxis = list(title = "Unidades"),
      yaxis2 = list(
        title = "Facturación",
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        tickprefix = "$"
      ),
      xaxis = list(title = "")
    )
  origenes_plotly_finish(p, source = source)
}

origenes_ventas_chart_ciclo <- function(tbl, source) {
  if (!nrow(tbl) || !"Ciclo Venta" %in% names(tbl)) {
    return(origenes_plotly_empty(source = source))
  }
  cols <- origenes_viz_colors()
  ciclo <- suppressWarnings(as.numeric(tbl[["Ciclo Venta"]]))
  ciclo <- ciclo[is.finite(ciclo) & ciclo >= 0]
  if (!length(ciclo)) {
    return(origenes_plotly_empty("Sin ciclo calculable", source = source))
  }
  # Buckets for drillable bars
  brks <- c(0, 7, 14, 30, 60, 90, Inf)
  labs <- c("0-7 d", "8-14 d", "15-30 d", "31-60 d", "61-90 d", "90+ d")
  bucket <- cut(ciclo, breaks = brks, labels = labs, include.lowest = TRUE, right = TRUE)
  df <- as.data.frame(table(bucket), stringsAsFactors = FALSE)
  names(df) <- c("bucket", "n")
  df$bucket <- factor(df$bucket, levels = labs)
  df <- df[order(df$bucket), , drop = FALSE]
  p <- plotly::plot_ly(
    df,
    x = ~bucket,
    y = ~n,
    type = "bar",
    key = ~as.character(bucket),
    customdata = ~as.character(bucket),
    source = source,
    marker = list(color = cols$green),
    hovertemplate = "%{x}: %{y}<extra></extra>"
  ) |>
    plotly::layout(xaxis = list(title = "Ciclo"), yaxis = list(title = "Unidades"))
  origenes_plotly_finish(p, source = source)
}

origenes_ventas_chart_vendedor <- function(tbl, source, top_n = 10L) {
  if (!nrow(tbl) || !"Vendedor" %in% names(tbl)) {
    return(origenes_plotly_empty(source = source))
  }
  cols <- origenes_viz_colors()
  df <- tbl |>
    dplyr::mutate(
      vend = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(.data$Vendedor)), ""),
        "(sin vendedor)"
      ),
      precio = suppressWarnings(as.numeric(.data[["Precio de venta"]]))
    ) |>
    dplyr::group_by(.data$vend) |>
    dplyr::summarise(
      n = dplyr::n(),
      fact = sum(.data$precio, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$fact)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::arrange(.data$fact)
  p <- plotly::plot_ly(
    df,
    x = ~fact,
    y = ~vend,
    type = "bar",
    orientation = "h",
    key = ~vend,
    customdata = ~vend,
    source = source,
    marker = list(color = cols$gold),
    hovertemplate = "%{y}: %{x:$,.0f}<extra></extra>"
  ) |>
    plotly::layout(
      xaxis = list(title = "Facturación", tickprefix = "$"),
      yaxis = list(title = "", automargin = TRUE)
    )
  origenes_plotly_finish(p, source = source)
}

origenes_ventas_apply_ciclo_bucket <- function(df, bucket_label) {
  if (is.null(bucket_label) || !nrow(df) || !"Ciclo Venta" %in% names(df)) {
    return(df)
  }
  ciclo <- suppressWarnings(as.numeric(df[["Ciclo Venta"]]))
  brks <- c(0, 7, 14, 30, 60, 90, Inf)
  labs <- c("0-7 d", "8-14 d", "15-30 d", "31-60 d", "61-90 d", "90+ d")
  bucket <- as.character(cut(ciclo, breaks = brks, labels = labs, include.lowest = TRUE, right = TRUE))
  df[!is.na(bucket) & bucket == bucket_label, , drop = FALSE]
}

# --- Resumen -----------------------------------------------------------------

origenes_resumen_series_from_tables <- function(bundle) {
  if (is.null(bundle)) {
    return(tibble::tibble())
  }
  res <- bundle$tabla_resultados
  emb <- bundle$tabla_embajadores
  conv <- bundle$tabla_conversion
  if (is.null(res) || !nrow(res)) {
    return(tibble::tibble())
  }

  period_cols <- setdiff(names(res), c("metrica", "Total"))
  if (!length(period_cols)) {
    return(tibble::tibble())
  }

  pick_num_row <- function(tbl, metric) {
    if (is.null(tbl) || !nrow(tbl)) {
      return(stats::setNames(rep(NA_real_, length(period_cols)), period_cols))
    }
    row <- tbl[tbl$metrica == metric, period_cols, drop = FALSE]
    if (!nrow(row)) {
      return(stats::setNames(rep(NA_real_, length(period_cols)), period_cols))
    }
    vals <- as.character(unlist(row[1, ], use.names = TRUE))
    vals <- gsub("[^0-9.-]", "", vals)
    stats::setNames(suppressWarnings(as.numeric(vals)), period_cols)
  }

  pick_pct_row <- function(tbl, metric) {
    if (is.null(tbl) || !nrow(tbl)) {
      return(stats::setNames(rep(NA_real_, length(period_cols)), period_cols))
    }
    row <- tbl[tbl$metrica == metric, period_cols, drop = FALSE]
    if (!nrow(row)) {
      return(stats::setNames(rep(NA_real_, length(period_cols)), period_cols))
    }
    vals <- as.character(unlist(row[1, ], use.names = TRUE))
    vals <- gsub("%", "", vals, fixed = TRUE)
    vals <- gsub(",", "", vals, fixed = TRUE)
    stats::setNames(suppressWarnings(as.numeric(vals)), period_cols)
  }

  pick_money_row <- function(tbl, metric) {
    if (is.null(tbl) || !nrow(tbl)) {
      return(stats::setNames(rep(NA_real_, length(period_cols)), period_cols))
    }
    row <- tbl[tbl$metrica == metric, period_cols, drop = FALSE]
    if (!nrow(row)) {
      return(stats::setNames(rep(NA_real_, length(period_cols)), period_cols))
    }
    vals <- as.character(unlist(row[1, ], use.names = TRUE))
    vals <- gsub("[^0-9.-]", "", vals)
    stats::setNames(suppressWarnings(as.numeric(vals)), period_cols)
  }

  tibble::tibble(
    periodo = period_cols,
    registrados = as.numeric(pick_num_row(emb, "Registrados")),
    primera_cita = as.numeric(pick_num_row(res, "Primera cita")),
    cierres = as.numeric(pick_num_row(conv, "Cierres")),
    facturacion = as.numeric(pick_money_row(res, "Facturación")),
    conv_primera = as.numeric(pick_pct_row(emb, "Conversión primera cita / registrados")),
    conv_activo = {
      # row name includes dynamic days
      nm <- emb$metrica[grepl("^Conversión activo", emb$metrica)]
      if (length(nm)) as.numeric(pick_pct_row(emb, nm[[1]])) else rep(NA_real_, length(period_cols))
    },
    conv_cierre = as.numeric(pick_pct_row(conv, "Conversión cita realizada / venta"))
  )
}

origenes_resumen_insight <- function(series) {
  if (is.null(series) || !nrow(series)) {
    return(list(
      title = "Sin datos",
      body = "No hay periodos con métricas en el rango seleccionado."
    ))
  }
  best_fact <- series[which.max(dplyr::coalesce(series$facturacion, -Inf))[1], , drop = FALSE]
  best_conv <- series[which.max(dplyr::coalesce(series$conv_cierre, -Inf))[1], , drop = FALSE]
  title <- paste0("Mejor facturación: ", best_fact$periodo[[1]])
  body <- paste0(
    origenes_money(best_fact$facturacion[[1]], compact = TRUE),
    " firmados · Mejor conv. cita→venta: ",
    best_conv$periodo[[1]],
    " (",
    if (is.finite(best_conv$conv_cierre[[1]])) paste0(round(best_conv$conv_cierre[[1]], 1), "%") else "—",
    ")"
  )
  list(title = title, body = body)
}

origenes_resumen_series_choices <- function() {
  c(
    "Registrados" = "registrados",
    "Primera cita" = "primera_cita",
    "Cierres" = "cierres",
    "Facturación" = "facturacion"
  )
}

origenes_resumen_series_defaults <- function() {
  c("registrados", "primera_cita", "cierres")
}

origenes_resumen_chart_series <- function(series,
                                         source,
                                         metrics = origenes_resumen_series_defaults()) {
  if (is.null(series) || !nrow(series)) {
    return(origenes_plotly_empty(source = source))
  }
  choices <- origenes_resumen_series_choices()
  metrics <- intersect(as.character(metrics %||% character()), unname(choices))
  if (!length(metrics)) {
    return(origenes_plotly_empty("Selecciona al menos una métrica", source = source))
  }

  cols <- origenes_viz_colors()
  meta <- list(
    registrados = list(label = "Registrados", color = cols$series[[1]], money = FALSE),
    primera_cita = list(label = "Primera cita", color = cols$series[[2]], money = FALSE),
    cierres = list(label = "Cierres", color = cols$series[[3]], money = FALSE),
    facturacion = list(label = "Facturación", color = cols$series[[4]], money = TRUE)
  )

  has_volume <- any(metrics %in% c("registrados", "primera_cita", "cierres"))
  has_money <- "facturacion" %in% metrics
  # Eje derecho solo si hay volumen + facturación a la vez.
  use_y2 <- has_volume && has_money

  p <- plotly::plot_ly(source = source)
  for (m in metrics) {
    info <- meta[[m]]
    y <- series[[m]]
    on_y2 <- use_y2 && isTRUE(info$money)
    trace_args <- list(
      p = p,
      x = series$periodo,
      y = y,
      name = info$label,
      type = "scatter",
      mode = "lines+markers",
      key = series$periodo,
      customdata = series$periodo,
      line = list(
        color = info$color,
        width = 2.2,
        dash = if (isTRUE(info$money)) "dot" else "solid"
      ),
      marker = list(color = info$color, size = 7)
    )
    if (on_y2) {
      trace_args$yaxis <- "y2"
      trace_args$hovertemplate <- "%{x}: %{y:$,.0f}<extra>%{fullData.name}</extra>"
    } else if (isTRUE(info$money)) {
      trace_args$hovertemplate <- "%{x}: %{y:$,.0f}<extra>%{fullData.name}</extra>"
    } else {
      trace_args$hovertemplate <- "%{x}: %{y:,.0f}<extra>%{fullData.name}</extra>"
    }
    p <- do.call(plotly::add_trace, trace_args)
  }

  layout_args <- list(
    p = p,
    xaxis = list(
      title = "",
      gridcolor = "rgba(23,35,29,0.06)",
      zeroline = FALSE,
      tickfont = list(size = 11)
    ),
    legend = list(orientation = "h", y = 1.16, x = 0, font = list(size = 11))
  )

  if (use_y2) {
    layout_args$yaxis <- list(
      title = list(text = "Volumen", standoff = 8),
      separatethousands = TRUE,
      automargin = TRUE,
      gridcolor = "rgba(23,35,29,0.06)",
      zeroline = FALSE
    )
    layout_args$yaxis2 <- list(
      title = list(text = "Facturación", standoff = 18),
      overlaying = "y",
      side = "right",
      showgrid = FALSE,
      tickprefix = "$",
      separatethousands = TRUE,
      automargin = TRUE,
      tickfont = list(size = 11),
      titlefont = list(size = 11),
      zeroline = FALSE
    )
  } else if (has_money && !has_volume) {
    layout_args$yaxis <- list(
      title = list(text = "Facturación", standoff = 8),
      tickprefix = "$",
      separatethousands = TRUE,
      automargin = TRUE,
      gridcolor = "rgba(23,35,29,0.06)",
      zeroline = FALSE
    )
  } else {
    layout_args$yaxis <- list(
      title = list(text = "Volumen", standoff = 8),
      separatethousands = TRUE,
      automargin = TRUE,
      gridcolor = "rgba(23,35,29,0.06)",
      zeroline = FALSE
    )
  }

  p <- do.call(plotly::layout, layout_args)
  origenes_plotly_finish(
    p,
    source = source,
    margin = list(l = 56, r = if (use_y2) 72 else 28, t = 40, b = 48)
  )
}

origenes_resumen_chart_conversiones <- function(series, source) {
  if (is.null(series) || !nrow(series)) {
    return(origenes_plotly_empty(source = source))
  }
  cols <- origenes_viz_colors()
  p <- plotly::plot_ly(source = source) |>
    plotly::add_bars(
      x = series$periodo, y = series$conv_primera, name = "1ª cita / registrados",
      key = series$periodo, customdata = series$periodo,
      marker = list(color = cols$series[[1]]),
      hovertemplate = "%{x}: %{y:.1f}%<extra>1ª cita / registrados</extra>"
    ) |>
    plotly::add_bars(
      x = series$periodo, y = series$conv_activo, name = "Activos / 1ª cita",
      key = series$periodo, customdata = series$periodo,
      marker = list(color = cols$series[[2]]),
      hovertemplate = "%{x}: %{y:.1f}%<extra>Activos / 1ª cita</extra>"
    ) |>
    plotly::add_bars(
      x = series$periodo, y = series$conv_cierre, name = "Cita → venta",
      key = series$periodo, customdata = series$periodo,
      marker = list(color = cols$series[[3]]),
      hovertemplate = "%{x}: %{y:.1f}%<extra>Cita → venta</extra>"
    ) |>
    plotly::layout(
      barmode = "group",
      yaxis = list(title = "%", ticksuffix = "%"),
      xaxis = list(title = "")
    )
  origenes_plotly_finish(p, source = source)
}
