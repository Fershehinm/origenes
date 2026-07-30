# Tablas DT ordenables (todas las columnas), estilo Orígenes.

origenes_dt_language <- function() {
  list(
    search = "Buscar:",
    searchPlaceholder = "Filtrar…",
    info = "_START_–_END_ de _TOTAL_",
    infoEmpty = "Sin filas",
    infoFiltered = "(filtrado de _MAX_)",
    zeroRecords = "Sin coincidencias",
    emptyTable = "Sin datos",
    paginate = list(previous = "Ant.", `next` = "Sig."),
    aria = list(
      sortAscending = ": ordenar ascendente",
      sortDescending = ": ordenar descendente"
    )
  )
}

origenes_dt_sort_callback <- htmlwidgets::JS(
  "function(settings) {",
  "  if ($.fn.dataTable.ext.type.order['or-smart-pre']) return;",
  "  $.fn.dataTable.ext.type.order['or-smart-pre'] = function(d) {",
  "    if (d === null || d === undefined) return Number.NEGATIVE_INFINITY;",
  "    var s = String(d).trim();",
  "    if (!s || s === '—' || s === '-') return Number.NEGATIVE_INFINITY;",
  "    var m = s.match(/^(\\d{1,2})\\/(\\d{1,2})\\/(\\d{2,4})(?:\\s+(\\d{1,2}):(\\d{2})(?::(\\d{2}))?)?/);",
  "    if (m) {",
  "      var y = m[3].length === 2 ? ('20' + m[3]) : m[3];",
  "      var pad = function(x) { return ('0' + x).slice(-2); };",
  "      return y + pad(m[2]) + pad(m[1]) + pad(m[4] || '0') + pad(m[5] || '0') + pad(m[6] || '0');",
  "    }",
  "    if (/%\\s*$/.test(s)) {",
  "      var p = parseFloat(s.replace(/[^0-9.,-]/g, '').replace(/,/g, ''));",
  "      return isNaN(p) ? s.toLowerCase() : p;",
  "    }",
  "    if (/^\\$?-?[0-9]/.test(s.replace(/\\s/g, ''))) {",
  "      var n = parseFloat(s.replace(/[^0-9.-]/g, ''));",
  "      if (!isNaN(n)) return n;",
  "    }",
  "    var gen = s.match(/^(\\d{4})\\s*-\\s*(\\d{1,2})$/);",
  "    if (gen) return gen[1] + ('0' + gen[2]).slice(-2);",
  "    return s.toLowerCase();",
  "  };",
  "}"
)

#' Datatable ordenable para métricas (Resumen) o detalle (Citas / Ventas).
origenes_datatable <- function(data, style = c("metric", "detail")) {
  style <- match.arg(style)
  if (is.null(data) || !is.data.frame(data)) {
    data <- data.frame()
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  n <- nrow(data)
  n_cols <- ncol(data)
  is_detail <- identical(style, "detail")

  opts <- list(
    ordering = TRUE,
    orderClasses = TRUE,
    autoWidth = FALSE,
    processing = FALSE,
    scrollX = TRUE,
    paging = is_detail && n > 100L,
    pageLength = if (is_detail) 100L else max(n, 1L),
    lengthChange = FALSE,
    searching = is_detail && n > 20L,
    info = is_detail && n > 100L,
    dom = if (is_detail && n > 20L) {
      if (n > 100L) "ftip" else "ft"
    } else {
      "t"
    },
    language = origenes_dt_language(),
    columnDefs = list(list(
      targets = if (n_cols) seq_len(n_cols) - 1L else list(),
      orderable = TRUE,
      type = "or-smart"
    )),
    initComplete = origenes_dt_sort_callback
  )

  DT::datatable(
    data,
    rownames = FALSE,
    escape = TRUE,
    selection = "none",
    width = "100%",
    class = paste(
      "display nowrap compact or-dt",
      if (is_detail) "or-dt--detail" else "or-dt--metric"
    ),
    options = opts
  )
}
