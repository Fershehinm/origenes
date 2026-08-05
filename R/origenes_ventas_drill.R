# Detalle de ventas agrupado por cierre (misma venta / sale_id).
# Patrón visual alineado al modal de vendedores_dashboard.

origenes_ventas_money_compact <- function(x) {
  origenes_money(x, compact = TRUE)
}

origenes_ventas_chr_display <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x[is.na(x) | !nzchar(x)] <- "—"
  x
}

origenes_fmt_cierre_seq <- function(idx, n_total) {
  idx <- as.integer(idx)
  w <- if (is.na(n_total) || n_total < 1L) {
    2L
  } else {
    max(2L, nchar(as.character(as.integer(n_total))))
  }
  sprintf(paste0("%0", w, "d"), idx)
}

#' Prepara filas de ventas con etiqueta Cierre N y flags first/last del grupo.
origenes_ventas_prepare_cierre_display <- function(tbl) {
  if (is.null(tbl) || !nrow(tbl)) {
    return(tibble::tibble())
  }
  df <- tbl
  if (!".sale_id" %in% names(df)) {
    df$.sale_id <- paste0(
      "row-",
      seq_len(nrow(df))
    )
  }
  key <- as.character(df$.sale_id)
  key[is.na(key) | !nzchar(key)] <- paste0(
    "fallback-",
    origenes_normalize_key(df[["Nombre de cliente"]]),
    "-",
    as.character(df[["Fecha de firma"]])
  )
  df$cierre_key <- key

  fecha_ord <- suppressWarnings(
    lubridate::parse_date_time(
      as.character(df[["Fecha de firma"]]),
      orders = c("dmy", "Ymd", "dbY"),
      quiet = TRUE
    )
  )
  df$fecha_ord <- as.Date(fecha_ord)
  df$precio_num <- suppressWarnings(as.numeric(df[["Precio de venta"]]))

  keys <- df |>
    dplyr::group_by(.data$cierre_key) |>
    dplyr::summarise(
      fecha_g = suppressWarnings(min(.data$fecha_ord, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$fecha_g), .data$cierre_key) |>
    dplyr::pull(.data$cierre_key)

  n_cierres <- length(keys)
  df |>
    dplyr::mutate(
      cierre_idx = match(.data$cierre_key, keys),
      cierre_lbl = paste0(
        "Cierre ",
        origenes_fmt_cierre_seq(.data$cierre_idx, n_cierres)
      )
    ) |>
    dplyr::arrange(
      .data$cierre_idx,
      dplyr::desc(.data$precio_num),
      .data[["ID de propiedad"]]
    ) |>
    dplyr::group_by(.data$cierre_key) |>
    dplyr::mutate(
      cierre_first = dplyr::row_number() == 1L,
      cierre_last = dplyr::row_number() == dplyr::n()
    ) |>
    dplyr::ungroup()
}

#' Reactable con bloques bordeados por cierre (una venta = N unidades).
origenes_ventas_cierre_reactable <- function(tbl) {
  empty <- tibble::tibble(
    cierre = character(),
    estatus = character(),
    fecha = character(),
    cliente = character(),
    proyecto = character(),
    unidad_id = character(),
    m2 = numeric(),
    precio = numeric(),
    vendedor = character()
  )
  if (is.null(tbl) || !nrow(tbl)) {
    return(reactable::reactable(empty, pagination = FALSE, language = list(noData = "Sin ventas")))
  }

  prep <- origenes_ventas_prepare_cierre_display(tbl)
  display <- prep |>
    dplyr::transmute(
      cierre_ord = as.integer(.data$cierre_idx),
      cierre = .data$cierre_lbl,
      estatus = origenes_ventas_chr_display(.data$Status),
      fecha = origenes_ventas_chr_display(.data[["Fecha de firma"]]),
      cliente = origenes_ventas_chr_display(.data[["Nombre de cliente"]]),
      proyecto = origenes_ventas_chr_display(.data$Proyecto),
      unidad_id = origenes_ventas_chr_display(.data[["ID de propiedad"]]),
      m2 = suppressWarnings(as.numeric(.data$M2)),
      precio = .data$precio_num,
      vendedor = origenes_ventas_chr_display(.data$Vendedor),
      .cierre_first = .data$cierre_first,
      .cierre_last = .data$cierre_last
    )

  reactable::reactable(
    display,
    compact = TRUE,
    highlight = TRUE,
    bordered = FALSE,
    pagination = nrow(display) > 80L,
    defaultPageSize = 80L,
    defaultSorted = list(cierre_ord = "asc"),
    showSortable = TRUE,
    showSortIcon = TRUE,
    rowStyle = function(index) {
      first <- isTRUE(display$.cierre_first[[index]])
      last <- isTRUE(display$.cierre_last[[index]])
      list(
        backgroundColor = "#fff",
        borderLeft = "2px solid #94a3b8",
        borderRight = "2px solid #94a3b8",
        borderTop = if (first) "2px solid #94a3b8" else "1px solid #e2e8f0",
        borderBottom = if (last) "2px solid #94a3b8" else "1px solid #e2e8f0",
        marginBottom = if (last) "10px" else "0"
      )
    },
    theme = reactable::reactableTheme(
      headerStyle = list(
        fontWeight = 600,
        fontSize = "13px",
        color = "#64748b",
        background = "#f8fafc",
        borderColor = "#e2e8f0"
      ),
      cellStyle = list(fontSize = "14px", borderColor = "#e2e8f0")
    ),
    columns = list(
      .cierre_first = reactable::colDef(show = FALSE),
      .cierre_last = reactable::colDef(show = FALSE),
      cierre_ord = reactable::colDef(show = FALSE),
      cierre = reactable::colDef(
        name = "Cierre",
        minWidth = 110,
        sortable = FALSE,
        cell = function(value) {
          htmltools::tags$span(class = "or-drill-cierre-lbl", value)
        }
      ),
      estatus = reactable::colDef(name = "Estatus", minWidth = 110),
      fecha = reactable::colDef(name = "Fecha", minWidth = 96, align = "center"),
      cliente = reactable::colDef(name = "Nombre de cliente", minWidth = 150),
      proyecto = reactable::colDef(name = "Proyecto", minWidth = 120),
      unidad_id = reactable::colDef(name = "Id", minWidth = 88),
      m2 = reactable::colDef(
        name = "M2",
        align = "right",
        minWidth = 72,
        na = "—",
        format = reactable::colFormat(digits = 2)
      ),
      precio = reactable::colDef(
        name = "Precio",
        align = "right",
        minWidth = 100,
        cell = function(value) {
          if (!is.finite(value) || value <= 0) {
            return("—")
          }
          htmltools::tags$span(
            class = "or-drill-money-pill",
            origenes_ventas_money_compact(value)
          )
        }
      ),
      vendedor = reactable::colDef(name = "Vendedor", minWidth = 130)
    )
  )
}

#' Modal de detalle de ventas agrupado por cierre.
origenes_ventas_drill_show_modal <- function(title, tbl, metric_lbl = "Cierres", session = NULL) {
  title_txt <- if (!is.null(metric_lbl) && nzchar(metric_lbl)) {
    sprintf("Detalle · %s · %s", title, metric_lbl)
  } else {
    sprintf("Detalle · %s", title)
  }
  if (is.null(tbl) || !nrow(tbl)) {
    shiny::showModal(shiny::modalDialog(
      title = title_txt,
      shiny::tags$p(class = "or-drill-empty", "Sin unidades para este cruce."),
      easyClose = TRUE,
      size = "l",
      footer = shiny::modalButton("Cerrar")
    ))
    return(invisible(NULL))
  }

  prep <- origenes_ventas_prepare_cierre_display(tbl)
  n_uni <- nrow(prep)
  n_cierres <- length(unique(prep$cierre_key))
  total <- sum(prep$precio_num, na.rm = TRUE)

  shiny::showModal(shiny::modalDialog(
    title = title_txt,
    shiny::tags$p(
      class = "or-drill-modal-meta",
      sprintf(
        "%s cierre(s) · %s unidad(es) · %s total",
        format(as.integer(n_cierres), big.mark = ",", scientific = FALSE),
        format(as.integer(n_uni), big.mark = ",", scientific = FALSE),
        origenes_ventas_money_compact(total)
      )
    ),
    shiny::tags$p(
      class = "or-drill-modal-hint",
      "Cada bloque con borde agrupa las unidades del mismo cierre (misma venta)."
    ),
    htmltools::div(
      class = "or-drill-reactable-wrap",
      style = "max-height:70vh;overflow:auto;",
      origenes_ventas_cierre_reactable(tbl)
    ),
    easyClose = TRUE,
    size = "l",
    footer = shiny::modalButton("Cerrar")
  ))
  origenes_htmlwidgets_static_render(session)
  invisible(NULL)
}
