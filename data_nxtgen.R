# Datos de NxtGen para Orígenes.
#
# El canal se mide por generación de embajadores (no por formulario).
# Resumen usa un snapshot de métricas mensuales alineado a las tablas
# Embajadores / Resultados / Conversión Citas hasta conectar el CRM.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

origenes_empty_sales <- function() {
  tibble::tibble(
    fecha = as.Date(character()),
    origen = character(),
    proyecto = character(),
    vendedor = character(),
    cliente = character(),
    estatus = character(),
    precio = numeric()
  )
}

origenes_pick_column <- function(data, aliases) {
  if (is.null(data) || !nrow(data)) {
    return(character())
  }
  keys <- gsub("[^a-z0-9]", "", tolower(names(data)))
  wanted <- gsub("[^a-z0-9]", "", tolower(aliases))
  hit <- match(wanted, keys, nomatch = 0L)
  hit <- hit[hit > 0L]
  if (!length(hit)) {
    return(rep(NA_character_, nrow(data)))
  }
  as.character(data[[hit[[1L]]]])
}

origenes_parse_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  parsed <- suppressWarnings(lubridate::parse_date_time(
    as.character(x),
    orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY", "mdY"),
    quiet = TRUE,
    tz = "UTC"
  ))
  as.Date(parsed)
}

origenes_standardize_sales <- function(data) {
  if (is.null(data) || !is.data.frame(data) || !nrow(data)) {
    return(origenes_empty_sales())
  }

  fecha <- origenes_pick_column(
    data,
    c("fecha", "date.of.sale", "closed.date", "created.date", "sale_date")
  )
  origen <- origenes_pick_column(
    data,
    c("origen", "origin_os", "origen_os", "sale_origin")
  )
  proyecto <- origenes_pick_column(
    data,
    c("proyecto", "project_os", "project_osl", "project", "development")
  )
  vendedor <- origenes_pick_column(
    data,
    c("vendedor", "seller.full.name", "seller", "asesor")
  )
  cliente <- origenes_pick_column(
    data,
    c("cliente", "client.full.name", "client_full_name", "client")
  )
  estatus <- origenes_pick_column(
    data,
    c("estatus", "estado_venta", "status_os", "status", "estado")
  )
  precio <- origenes_pick_column(
    data,
    c("precio", "purchase.price", "sale.price", "monto_facturacion", "amount")
  )

  tibble::tibble(
    fecha = origenes_parse_date(fecha),
    origen = stringr::str_squish(trimws(origen)),
    proyecto = stringr::str_squish(trimws(proyecto)),
    vendedor = stringr::str_squish(trimws(vendedor)),
    cliente = stringr::str_squish(trimws(cliente)),
    estatus = stringr::str_squish(trimws(estatus)),
    precio = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", precio)))
  ) |>
    dplyr::mutate(
      dplyr::across(
        c("origen", "proyecto", "vendedor", "cliente", "estatus"),
        ~ dplyr::na_if(.x, "")
      )
    )
}

origenes_extract_sales <- function(object) {
  if (is.data.frame(object)) {
    return(origenes_standardize_sales(object))
  }
  if (!is.list(object)) {
    return(origenes_empty_sales())
  }
  candidates <- c("units", "units_enriched", "sales", "sales_clean", "sales_raw")
  for (candidate in candidates) {
    value <- object[[candidate]]
    if (is.data.frame(value) && nrow(value)) {
      return(origenes_standardize_sales(value))
    }
  }
  origenes_empty_sales()
}

origenes_data_candidates <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  configured <- Sys.getenv("ORIGENES_DATA_PATH", unset = "")
  paths <- c(
    configured,
    file.path(root, "data", "cache", "latest.rds"),
    file.path(root, "data", "origenes.rds"),
    file.path(root, "data", "origenes.csv")
  )
  unique(paths[nzchar(paths)])
}

origenes_load_sales <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  candidates <- origenes_data_candidates(root)
  path <- candidates[file.exists(candidates)][1L]
  if (!length(path) || is.na(path)) {
    data <- origenes_empty_sales()
    attr(data, "source_path") <- NA_character_
    return(data)
  }

  object <- tryCatch(
    if (grepl("\\.csv$", path, ignore.case = TRUE)) {
      utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    } else {
      readRDS(path)
    },
    error = function(error) {
      warning("No se pudieron cargar los datos de Orígenes: ", conditionMessage(error))
      NULL
    }
  )
  data <- origenes_extract_sales(object)
  attr(data, "source_path") <- normalizePath(path, winslash = "/", mustWork = FALSE)
  data
}

origenes_normalize_key <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  gsub("[^a-z0-9]", "", tolower(x))
}

nxtgen_sales <- function(data) {
  if (!nrow(data)) {
    return(data)
  }
  data |>
    dplyr::filter(origenes_normalize_key(.data$origen) == "nxtgen")
}

# Snapshot provisional de las tablas NxtGen (hasta CRM).
# Totales = columna Total; meses = columnas visibles del corte.
nxtgen_snapshot_monthly <- function() {
  tibble::tibble(
    mes = as.Date(c(
      "2025-01-01", "2025-09-01", "2025-10-01",
      "2025-11-01", "2025-12-01", "2026-01-01"
    )),
    registrados = c(1, 8, 18, 21, 17, 27),
    cita_agendada_emb = c(1, 6, 12, 14, 11, 16),
    primera_cita = c(1, 5, 10, 13, 10, 16),
    citas_agendadas = c(0, 0, 0, 0, 0, 64),
    citas_nuevas = c(0, 0, 0, 0, 0, 32),
    cierres = c(0, 0, 0, 0, 0, 3),
    unidades = c(0, 0, 0, 0, 0, 4),
    facturacion = c(0, 0, 0, 0, 0, 31200000),
    conv_cita_cierre = c(NA, NA, NA, NA, NA, 0.0938),
    ciclo_promedio = c(NA, NA, NA, NA, NA, 55.2)
  )
}

nxtgen_snapshot_totals <- function() {
  list(
    registrados = 159L,
    cita_agendada_emb = 81L,
    primera_cita = 72L,
    conv_primera_reg = 0.4528,
    activos = 12L,
    conv_activo_primera = 0.1667,
    citas_agendadas = 320L,
    citas_nuevas = 204L,
    unidades = 11L,
    facturacion = 89137463,
    cierres = 8L,
    conv_cita_cierre = 0.0392,
    unidades_conversion = 10L,
    facturacion_conversion = 84921962,
    source_label = "Snapshot tablas NxtGen"
  )
}

nxtgen_load_dashboard <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  path <- file.path(root, "data", "nxtgen", "resumen.rds")
  if (file.exists(path)) {
    object <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.list(object) && is.data.frame(object$monthly)) {
      attr(object, "source_path") <- normalizePath(path, winslash = "/", mustWork = FALSE)
      attr(object, "source_label") <- "data/nxtgen/resumen.rds"
      return(object)
    }
  }

  object <- list(
    monthly = nxtgen_snapshot_monthly(),
    totals = nxtgen_snapshot_totals()
  )
  attr(object, "source_path") <- NA_character_
  attr(object, "source_label") <- object$totals$source_label
  object
}

nxtgen_fmt_pct <- function(x, digits = 1) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !is.finite(x)) {
    return("—")
  }
  paste0(format(round(100 * x, digits), nsmall = digits, trim = TRUE), "%")
}

nxtgen_fmt_num <- function(x, digits = 0) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !is.finite(x)) {
    return("—")
  }
  format(round(x, digits), big.mark = ",", nsmall = digits, trim = TRUE)
}

nxtgen_funnel_table <- function(totals) {
  tibble::tibble(
    etapa = factor(
      c("Registrados", "Cita agendada", "Primera cita", "Cierres"),
      levels = rev(c("Registrados", "Cita agendada", "Primera cita", "Cierres"))
    ),
    valor = c(
      totals$registrados %||% 0,
      totals$cita_agendada_emb %||% 0,
      totals$primera_cita %||% 0,
      totals$cierres %||% 0
    )
  )
}

nxtgen_activity_long <- function(monthly) {
  monthly |>
    dplyr::select(.data$mes, .data$citas_agendadas, .data$citas_nuevas) |>
    tidyr::pivot_longer(
      cols = c("citas_agendadas", "citas_nuevas"),
      names_to = "serie",
      values_to = "valor"
    ) |>
    dplyr::mutate(
      serie = dplyr::recode(
        .data$serie,
        citas_agendadas = "Agendadas",
        citas_nuevas = "Nuevas realizadas"
      )
    )
}

nxtgen_embudo_long <- function(monthly) {
  monthly |>
    dplyr::select(.data$mes, .data$registrados, .data$primera_cita) |>
    tidyr::pivot_longer(
      cols = c("registrados", "primera_cita"),
      names_to = "serie",
      values_to = "valor"
    ) |>
    dplyr::mutate(
      serie = dplyr::recode(
        .data$serie,
        registrados = "Registrados",
        primera_cita = "Primera cita"
      )
    )
}
