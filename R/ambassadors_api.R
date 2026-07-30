# Cliente Data API Embajadores (Bubble en ambassadors.mx).
# Misma autenticación que RSCG / vendedores: Bearer + /obj/{tipo}.

ambassadors_credentials <- function() {
  list(
    base = trimws(Sys.getenv("AMBASSADORS_BASE", unset = "")),
    token = trimws(Sys.getenv("AMBASSADORS_TOKEN", unset = ""))
  )
}

ambassadors_configured <- function() {
  creds <- ambassadors_credentials()
  nzchar(creds$base) && nzchar(creds$token)
}

ambassadors_api_root <- function(api_base_url = NULL) {
  if (is.null(api_base_url) || !nzchar(api_base_url)) {
    api_base_url <- ambassadors_credentials()$base
  }
  u <- sub("/+$", "", api_base_url)
  if (grepl("/obj$", u, ignore.case = TRUE)) {
    sub("/obj$", "", u, ignore.case = TRUE)
  } else {
    u
  }
}

ambassadors_get <- function(endpoint,
                            limit = 100L,
                            cursor = 0L,
                            api_base = NULL,
                            token = NULL) {
  creds <- ambassadors_credentials()
  api_base <- api_base %||% creds$base
  token <- token %||% creds$token
  if (!nzchar(api_base) || !nzchar(token)) {
    stop("Define AMBASSADORS_BASE y AMBASSADORS_TOKEN en .Renviron", call. = FALSE)
  }

  root <- ambassadors_api_root(api_base)
  url <- paste0(root, "/obj/", endpoint)
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_url_query(limit = as.integer(limit), cursor = as.integer(cursor)) |>
      httr2::req_headers(
        Authorization = paste("Bearer", token),
        Accept = "application/json"
      ) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_timeout(seconds = 120) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) {
    return(list(status = NA_integer_, body = list(), url = url))
  }
  body <- tryCatch(
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE),
    error = function(e) list()
  )
  list(status = httr2::resp_status(resp), body = body, url = url)
}

ambassadors_meta_get <- function(api_base = NULL, token = NULL) {
  creds <- ambassadors_credentials()
  api_base <- api_base %||% creds$base
  token <- token %||% creds$token
  root <- ambassadors_api_root(api_base)
  url <- paste0(root, "/meta")
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_headers(
        Authorization = paste("Bearer", token),
        Accept = "application/json"
      ) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_timeout(seconds = 60) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) {
    return(list(status = NA_integer_, body = list(), url = url))
  }
  body <- tryCatch(
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE),
    error = function(e) list()
  )
  list(status = httr2::resp_status(resp), body = body, url = url)
}

ambassadors_fetch_endpoint <- function(endpoint,
                                       page_size = 100L,
                                       max_pages = 200L,
                                       api_base = NULL,
                                       token = NULL) {
  all <- list()
  cursor <- 0L
  pages <- 0L
  first_status <- NA_integer_
  repeat {
    if (pages >= max_pages) {
      warning("max_pages alcanzado: ", endpoint, call. = FALSE)
      break
    }
    res <- ambassadors_get(
      endpoint,
      limit = page_size,
      cursor = cursor,
      api_base = api_base,
      token = token
    )
    if (is.na(first_status)) {
      first_status <- suppressWarnings(as.integer(res$status %||% NA_integer_))
    }
    if (!is.na(first_status) && first_status >= 400L && pages == 0L) {
      break
    }
    recs <- origenes_bubble_extract_records(res$body)
    rem <- origenes_bubble_extract_remaining(res$body)
    if (!length(recs)) {
      break
    }
    all <- c(all, recs)
    pages <- pages + 1L
    n <- length(recs)
    if (!is.na(rem) && rem <= 0L) {
      break
    }
    if (n < page_size) {
      break
    }
    cursor <- cursor + n
  }
  df <- origenes_bubble_records_to_df(all)
  attr(df, "http_status") <- first_status
  attr(df, "endpoint") <- endpoint
  df
}

ambassadors_candidate_types <- function() {
  c(
    # genéricos Bubble / CRM
    "user", "User",
    "ambassador", "Ambassador", "embajador", "Embajador",
    "lead", "Lead",
    "meeting", "Meeting", "cita", "Cita",
    "sale", "Sale", "venta", "Venta",
    "seller", "Seller",
    "contact", "Contact",
    "project", "Project",
    "property", "Property", "soldproperty", "Soldproperty",
    "registration", "Registration", "registro", "Registro",
    "generation", "Generation", "generacion", "Generacion",
    "origin", "Origin", "origen", "Origen",
    "company", "Company",
    "broker", "Broker",
    "appointment", "Appointment",
    "deal", "Deal",
    "activity", "Activity",
    "note", "Note",
    "task", "Task",
    "tag", "Tag",
    "form", "Form",
    "submission", "Submission",
    # NxtGen / academy
    "nxtgen", "NxtGen", "Nxtgen",
    "academy", "Academy",
    "cohort", "Cohort",
    "graduate", "Graduate",
    "module", "Module",
    "session", "Session",
    "attendance", "Attendance"
  )
}

#' Prueba tipos candidatos y meta; no imprime secretos.
ambassadors_discover_types <- function(candidates = ambassadors_candidate_types(),
                                       sample_limit = 5L) {
  if (!ambassadors_configured()) {
    stop("Faltan AMBASSADORS_BASE / AMBASSADORS_TOKEN", call. = FALSE)
  }

  meta <- ambassadors_meta_get()
  meta_types <- character()
  if (!is.na(meta$status) && meta$status < 400L) {
    types <- meta$body$types %||% meta$body$response$types %||% list()
    if (is.list(types) && length(types)) {
      meta_types <- names(types)
      if (!length(meta_types) || all(!nzchar(meta_types))) {
        meta_types <- vapply(types, function(x) {
          as.character(x$name %||% x$type %||% NA_character_)
        }, character(1L))
        meta_types <- meta_types[!is.na(meta_types) & nzchar(meta_types)]
      }
    }
  }

  probe <- unique(c(meta_types, candidates))
  rows <- lapply(probe, function(endpoint) {
    res <- ambassadors_get(endpoint, limit = sample_limit, cursor = 0L)
    status <- suppressWarnings(as.integer(res$status %||% NA_integer_))
    recs <- origenes_bubble_extract_records(res$body)
    rem <- origenes_bubble_extract_remaining(res$body)
    cols <- if (length(recs) && is.list(recs[[1L]])) {
      paste(names(recs[[1L]]), collapse = ", ")
    } else {
      NA_character_
    }
    tibble::tibble(
      endpoint = endpoint,
      status = status,
      ok = !is.na(status) && status < 400L && length(recs) > 0L,
      n_sample = length(recs),
      remaining = rem,
      columns_sample = cols,
      from_meta = endpoint %in% meta_types
    )
  })

  list(
    meta_status = meta$status,
    meta_types = meta_types,
    catalog = dplyr::bind_rows(rows) |>
      dplyr::arrange(dplyr::desc(.data$ok), .data$endpoint)
  )
}
