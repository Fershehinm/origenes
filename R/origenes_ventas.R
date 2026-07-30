# Tabla de detalle Ventas (NxtGen / Brokers) alineada al Sheets.
# Fuente: Bubble sales + soldproperty; teléfono/email desde hub contact si hay join;
# Primera Cita / Gen Cita / Ciclo desde lead_first_meeting_index.

origenes_ym_label <- function(d) {
  d <- as.Date(d)
  ifelse(
    is.na(d),
    NA_character_,
    paste(format(d, "%Y"), format(d, "%m"), sep = " - ")
  )
}

origenes_fmt_recompra <- function(x) {
  key <- origenes_normalize_key(x)
  dplyr::case_when(
    is.na(x) | !nzchar(as.character(x)) ~ NA_character_,
    key %in% c("true", "1", "yes", "si", "sí") ~ "si",
    key %in% c("false", "0", "no") ~ "no",
    TRUE ~ tolower(stringr::str_squish(as.character(x)))
  )
}

origenes_fmt_date_sheets <- function(d) {
  d <- as.Date(d)
  ifelse(is.na(d), NA_character_, format(d, "%d/%m/%Y"))
}

origenes_hub_contact_phone_email <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  amb <- tryCatch(origenes_load_ambassadors_raw(root), error = function(e) NULL)
  if (is.null(amb) || is.null(amb$contact) || !nrow(amb$contact)) {
    return(tibble::tibble(
      client_contact_id = character(),
      telefono = character(),
      email = character()
    ))
  }
  ctc <- amb$contact
  tibble::tibble(
    client_contact_id = origenes_id_norm(ctc$RSCG.Lead.ID),
    telefono = stringr::str_squish(as.character(ctc$Phone %||% NA)),
    email = stringr::str_squish(as.character(ctc$Email %||% NA))
  ) |>
    dplyr::filter(!is.na(.data$client_contact_id), nzchar(.data$client_contact_id)) |>
    dplyr::mutate(
      telefono = dplyr::na_if(.data$telefono, ""),
      email = dplyr::na_if(.data$email, "")
    ) |>
    dplyr::group_by(.data$client_contact_id) |>
    dplyr::summarise(
      telefono = dplyr::coalesce(dplyr::first(stats::na.omit(.data$telefono)), NA_character_),
      email = dplyr::coalesce(dplyr::first(stats::na.omit(.data$email)), NA_character_),
      .groups = "drop"
    )
}

#' Tabla Ventas con columnas del Sheets, filtrada por origen y rango de fechas.
origenes_ventas_table <- function(origin_key,
                                  start = NULL,
                                  end = NULL,
                                  root = Sys.getenv("ORIGENES_APP_ROOT", "."),
                                  rscg = NULL) {
  origin_key <- origenes_origin_key(origin_key)
  defaults <- origenes_default_date_range()
  start <- as.Date(start %||% defaults$start)
  end <- as.Date(end %||% defaults$end)

  if (is.null(rscg)) {
    rscg <- tryCatch(origenes_load_rscg_bundle(root), error = function(e) NULL)
  }
  if (is.null(rscg)) {
    return(origenes_ventas_empty())
  }

  sales_raw <- rscg$sales_raw %||% tibble::tibble()
  props_raw <- rscg$soldproperty_raw %||% tibble::tibble()
  first_idx <- rscg$lead_first_meeting_index %||% tibble::tibble()

  if (!nrow(sales_raw)) {
    return(origenes_ventas_empty())
  }

  sales <- sales_raw |>
    dplyr::mutate(
      sale_id = origenes_id_norm(.data$X_id),
      fecha_firma = origenes_parse_date(.data$Date.of.sale),
      origen = stringr::str_squish(as.character(.data$Origin_OS)),
      origen_key = origenes_origin_key(.data$Origin_OS),
      cliente = stringr::str_squish(as.character(.data$Client.full.name)),
      client_contact_id = origenes_id_norm(.data$Client_contact_R),
      vendedor = stringr::str_squish(as.character(.data$Seller.full.name)),
      status = stringr::str_squish(as.character(.data$Status_OS)),
      recompra = origenes_fmt_recompra(.data$Repurchase),
      fecha_rescision_sale = origenes_parse_date(.data$Date.of.rescission),
      proyecto_sale = {
        p <- .data$Project_OSL
        if (is.list(p)) {
          vapply(p, function(x) {
            x <- unlist(x)
            if (!length(x)) NA_character_ else paste(x, collapse = ", ")
          }, character(1))
        } else {
          stringr::str_squish(as.character(p))
        }
      }
    ) |>
    dplyr::filter(.data$origen_key == origin_key) |>
    dplyr::filter(
      !is.na(.data$fecha_firma),
      .data$fecha_firma >= start,
      .data$fecha_firma <= end
    )

  if (!nrow(sales)) {
    return(origenes_ventas_empty())
  }

  props <- if (nrow(props_raw)) {
    props_raw |>
      dplyr::mutate(
        sale_id = origenes_id_norm(.data$Sale_R),
        id_propiedad = stringr::str_squish(as.character(.data$Property.ID)),
        m2 = suppressWarnings(as.numeric(.data$m2)),
        precio = suppressWarnings(as.numeric(.data$Purchase.price)),
        proyecto_prop = stringr::str_squish(as.character(.data$Project_OS)),
        intencion = stringr::str_squish(as.character(.data$Intention_OS)),
        fecha_rescision_prop = origenes_parse_date(.data$Rescission.Date)
      ) |>
      dplyr::select(
        "sale_id", "id_propiedad", "m2", "precio", "proyecto_prop",
        "intencion", "fecha_rescision_prop"
      )
  } else {
    tibble::tibble(
      sale_id = character(),
      id_propiedad = character(),
      m2 = numeric(),
      precio = numeric(),
      proyecto_prop = character(),
      intencion = character(),
      fecha_rescision_prop = as.Date(character())
    )
  }

  # Una fila por unidad/propiedad (como el Sheets); si no hay prop, una fila por sale.
  joined <- sales |>
    dplyr::left_join(props, by = "sale_id", relationship = "many-to-many")

  has_prop <- !is.na(joined$id_propiedad) | !is.na(joined$precio)
  with_prop <- joined[has_prop, , drop = FALSE]
  sales_covered <- unique(joined$sale_id[has_prop])
  only_sale <- sales |>
    dplyr::filter(!(.data$sale_id %in% sales_covered)) |>
    dplyr::mutate(
      id_propiedad = NA_character_,
      m2 = NA_real_,
      precio = NA_real_,
      proyecto_prop = NA_character_,
      intencion = NA_character_,
      fecha_rescision_prop = as.Date(NA)
    )
  rows <- dplyr::bind_rows(with_prop, only_sale)

  first_meet <- if (nrow(first_idx) && "lead_contact_id" %in% names(first_idx)) {
    first_idx |>
      dplyr::transmute(
        client_contact_id = origenes_id_norm(.data$lead_contact_id),
        primera_cita = as.Date(.data$cohort_fecha)
      ) |>
      dplyr::filter(!is.na(.data$client_contact_id), !is.na(.data$primera_cita)) |>
      dplyr::group_by(.data$client_contact_id) |>
      dplyr::summarise(primera_cita = min(.data$primera_cita), .groups = "drop")
  } else {
    tibble::tibble(client_contact_id = character(), primera_cita = as.Date(character()))
  }

  phones <- origenes_hub_contact_phone_email(root)

  out <- rows |>
    dplyr::left_join(first_meet, by = "client_contact_id") |>
    dplyr::left_join(phones, by = "client_contact_id") |>
    dplyr::mutate(
      proyecto = dplyr::coalesce(
        dplyr::na_if(.data$proyecto_prop, ""),
        dplyr::na_if(.data$proyecto_sale, "")
      ),
      fecha_rescision = dplyr::coalesce(.data$fecha_rescision_prop, .data$fecha_rescision_sale),
      precio_venta = .data$precio,
      gen_venta = origenes_ym_label(.data$fecha_firma),
      gen_cita = origenes_ym_label(.data$primera_cita),
      ciclo_venta = as.integer(.data$fecha_firma - .data$primera_cita),
      conciliacion_lead = NA_character_
    ) |>
    dplyr::arrange(dplyr::desc(.data$fecha_firma), .data$cliente, .data$id_propiedad) |>
    dplyr::transmute(
      `Fecha de firma` = origenes_fmt_date_sheets(.data$fecha_firma),
      `Nombre de cliente` = .data$cliente,
      `Telefono de cliente` = .data$telefono,
      `Email del cliente` = .data$email,
      M2 = .data$m2,
      Origen = .data$origen,
      Proyecto = .data$proyecto,
      `ID de propiedad` = .data$id_propiedad,
      Recompra = .data$recompra,
      `Fecha de rescision` = origenes_fmt_date_sheets(.data$fecha_rescision),
      Vendedor = .data$vendedor,
      Status = .data$status,
      `Precio de venta` = .data$precio_venta,
      Intencion = .data$intencion,
      `Conciliacion Lead` = .data$conciliacion_lead,
      `Gen Venta` = .data$gen_venta,
      `Primera Cita` = origenes_fmt_date_sheets(.data$primera_cita),
      `Gen Cita` = .data$gen_cita,
      `Ciclo Venta` = .data$ciclo_venta
    )

  out
}

origenes_ventas_empty <- function() {
  tibble::tibble(
    `Fecha de firma` = character(),
    `Nombre de cliente` = character(),
    `Telefono de cliente` = character(),
    `Email del cliente` = character(),
    M2 = numeric(),
    Origen = character(),
    Proyecto = character(),
    `ID de propiedad` = character(),
    Recompra = character(),
    `Fecha de rescision` = character(),
    Vendedor = character(),
    Status = character(),
    `Precio de venta` = numeric(),
    Intencion = character(),
    `Conciliacion Lead` = character(),
    `Gen Venta` = character(),
    `Primera Cita` = character(),
    `Gen Cita` = character(),
    `Ciclo Venta` = integer()
  )
}
