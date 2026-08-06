# Unión Embajadores (ambassadors.mx) ↔ Bubble RSCG (misma app que vendedores).
#
# Reglas canónicas:
#   1) Cita:   amb.meeting.RSCG.Meeting.ID  ==  bubble.meeting.X_id
#   2) Lead:   amb.contact.RSCG.Lead.ID     ==  bubble.meeting.Lead_contact_R
#              (también vs sale.Client_contact_R cuando exista)
#   3) Local:  amb.meeting.Client_R         ==  amb.contact.X_id
#
# Sistema de verdad por tipo de métrica:
#   - Volumen comercial (citas/ventas/facturación): Bubble RSCG
#   - Registro / tipo de embajador / equipo hub: Embajadores
#   - Enriquecimiento de cita hub: join (1)

.origenes_data_cache <- new.env(parent = emptyenv())

#' Cache en proceso para RDS/APIs pesadas (un load por worker de shinyapps).
#' `producer` debe ser una función sin argumentos que devuelve el valor.
origenes_data_cached <- function(key, producer) {
  key <- as.character(key)
  if (exists(key, envir = .origenes_data_cache, inherits = FALSE)) {
    return(get(key, envir = .origenes_data_cache, inherits = FALSE))
  }
  val <- producer()
  assign(key, val, envir = .origenes_data_cache)
  val
}

origenes_id_norm <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

origenes_origin_key <- function(x) {
  origenes_normalize_key(x)
}

origenes_load_rscg_bundle <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  origenes_data_cached(paste0("rscg::", normalizePath(root, winslash = "/", mustWork = FALSE)), function() {
    path <- Sys.getenv("ORIGENES_VENDEDORES_BUNDLE", unset = "")
    if (nzchar(path) && file.exists(path)) {
      b <- readRDS(path)
      attr(b, "source_path") <- normalizePath(path, winslash = "/", mustWork = FALSE)
      attr(b, "source_label") <- "vendedores_bundle"
      return(b)
    }
    cache <- file.path(root, "data", "cache", "latest.rds")
    if (file.exists(cache)) {
      b <- readRDS(cache)
      attr(b, "source_path") <- normalizePath(cache, winslash = "/", mustWork = FALSE)
      attr(b, "source_label") <- "origenes_cache"
      return(b)
    }
    if (origenes_bubble_configured()) {
      b <- origenes_bubble_fetch_bundle()
      attr(b, "source_label") <- "bubble_live"
      return(b)
    }
    stop(
      "No hay bundle Bubble. Define ORIGENES_VENDEDORES_BUNDLE o BUBBLE_* / data/cache/latest.rds",
      call. = FALSE
    )
  })
}

#' Resuelve .rds en explore/ sin depender de mayúsculas (macOS vs Linux shinyapps).
origenes_explore_rds_path <- function(explore_dir, candidates) {
  if (!dir.exists(explore_dir)) {
    return(NULL)
  }
  files <- list.files(explore_dir, pattern = "\\.rds$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) {
    return(NULL)
  }
  basenames <- tolower(basename(files))
  for (cand in candidates) {
    hit <- files[basenames == tolower(cand)]
    if (length(hit)) {
      return(hit[[1]])
    }
  }
  NULL
}

origenes_load_ambassadors_raw <- function(root = Sys.getenv("ORIGENES_APP_ROOT", "."),
                                          allow_live = TRUE) {
  origenes_data_cached(
    paste0("amb::", normalizePath(root, winslash = "/", mustWork = FALSE), "::", allow_live),
    function() {
      explore <- file.path(root, "data", "ambassadors", "explore")
      contact_path <- origenes_explore_rds_path(explore, c("contact.rds", "Contact.rds"))
      meeting_path <- origenes_explore_rds_path(explore, c("meeting.rds", "Meeting.rds"))
      team_path <- origenes_explore_rds_path(explore, c("team.rds", "Team.rds"))

      if (!is.null(contact_path) && !is.null(meeting_path)) {
        out <- list(
          contact = readRDS(contact_path),
          meeting = readRDS(meeting_path),
          team = if (!is.null(team_path)) readRDS(team_path) else tibble::tibble(),
          source_label = "ambassadors_explore_cache"
        )
        attr(out, "source_path") <- normalizePath(explore, winslash = "/", mustWork = FALSE)
        return(out)
      }

      if (!isTRUE(allow_live) || !ambassadors_configured()) {
        stop("Falta data/ambassadors/explore (Contact.rds / Meeting.rds)", call. = FALSE)
      }
      list(
        contact = ambassadors_fetch_endpoint("contact"),
        meeting = ambassadors_fetch_endpoint("meeting"),
        team = ambassadors_fetch_endpoint("team"),
        source_label = "ambassadors_live"
      )
    }
  )
}

origenes_ambassador_type_flags <- function(type_os) {
  key <- origenes_normalize_key(type_os)
  list(
    is_ambassador = grepl("^ambassador", key),
    is_nxtgen_ambassador = key %in% c("ambassadornxtgen"),
    is_broker_ambassador = key %in% c("ambassadorbroker"),
    is_other_ambassador = key %in% c("ambassadorother", "ambassadorotro"),
    is_prospecto = key == "prospecto",
    is_seller = key %in% c("rscgseller", "seller")
  )
}

origenes_standardize_amb_contacts <- function(contact) {
  if (is.null(contact) || !nrow(contact)) {
    return(tibble::tibble())
  }
  flags <- origenes_ambassador_type_flags(contact$Type_OS)
  tibble::tibble(
    amb_contact_id = origenes_id_norm(contact$X_id),
    rscg_lead_id = origenes_id_norm(contact$RSCG.Lead.ID),
    nombre = stringr::str_squish(dplyr::coalesce(
      origenes_id_norm(contact$Full.Name),
      paste(
        dplyr::coalesce(origenes_id_norm(contact$First.Name), ""),
        dplyr::coalesce(origenes_id_norm(contact$Last.Name), "")
      )
    )),
    email = origenes_id_norm(contact$Email),
    type_os = origenes_id_norm(contact$Type_OS),
    status_os = origenes_id_norm(contact$Status_OS),
    origin_os = origenes_id_norm(contact$Origin_OS),
    origin_key = origenes_origin_key(contact$Origin_OS),
    first_meeting = tolower(origenes_id_norm(contact$First.Meeting)) %in% c("true", "1", "yes"),
    prospecting_method = origenes_id_norm(contact$Prospecting.Method_OS),
    last_meeting_date = origenes_parse_date(contact$Last.Meeting.Date),
    fecha_registro = origenes_parse_date(contact$Created.Date),
    origin_team_id = origenes_id_norm(contact$Origin.Team_R),
    origin_challenge_id = origenes_id_norm(contact$Origin.Challenge_R),
    is_ambassador = flags$is_ambassador,
    is_nxtgen_ambassador = flags$is_nxtgen_ambassador,
    is_broker_ambassador = flags$is_broker_ambassador,
    is_other_ambassador = flags$is_other_ambassador,
    is_prospecto = flags$is_prospecto,
    matched_rscg_lead = !is.na(origenes_id_norm(contact$RSCG.Lead.ID))
  ) |>
    dplyr::mutate(
      nombre = dplyr::na_if(stringr::str_squish(.data$nombre), ""),
      nombre_key = brokers_norm_name(.data$nombre)
    )
}

origenes_standardize_amb_meetings <- function(meeting) {
  if (is.null(meeting) || !nrow(meeting)) {
    return(tibble::tibble())
  }
  tibble::tibble(
    amb_meeting_id = origenes_id_norm(meeting$X_id),
    rscg_meeting_id = origenes_id_norm(meeting$RSCG.Meeting.ID),
    amb_contact_id = origenes_id_norm(meeting$Client_R),
    fecha_cita = origenes_parse_date(meeting$Start.Date),
    status_os = origenes_id_norm(meeting$Status_OS),
    origin_os = origenes_id_norm(meeting$Origin_OS),
    origin_key = origenes_origin_key(meeting$Origin_OS),
    first_meeting = tolower(origenes_id_norm(meeting$First.Meeting)) %in% c("true", "1", "yes"),
    prospecting_method = origenes_id_norm(meeting$Prospecting.Method_OS),
    vendedor = origenes_id_norm(meeting$Seller.Full.Name),
    lead_nombre = origenes_id_norm(meeting$Lead.Full.Name),
    project_os = origenes_id_norm(meeting$Project_OS),
    intention_os = origenes_id_norm(meeting$Intention_OS),
    product_type_os = origenes_id_norm(meeting$Product.Type_OS),
    origin_team_id = origenes_id_norm(meeting$Origin.Team_R),
    matched_rscg_meeting = !is.na(origenes_id_norm(meeting$RSCG.Meeting.ID))
  )
}

origenes_join_diagnostics <- function(amb, rscg) {
  amb_m <- origenes_standardize_amb_meetings(amb$meeting)
  amb_c <- origenes_standardize_amb_contacts(amb$contact)

  rscg_meetings <- rscg$meetings_raw %||% tibble::tibble()
  rscg_sales <- rscg$sales_raw %||% tibble::tibble()
  rscg_mids <- unique(na.omit(origenes_id_norm(rscg_meetings$X_id)))
  rscg_leads <- unique(na.omit(origenes_id_norm(rscg_meetings$Lead_contact_R)))
  rscg_clients <- unique(na.omit(origenes_id_norm(rscg_sales$Client_contact_R)))
  rscg_people <- unique(c(rscg_leads, rscg_clients))

  amb_mids <- unique(na.omit(amb_m$rscg_meeting_id))
  amb_lids <- unique(na.omit(amb_c$rscg_lead_id))

  list(
    fetched_at = Sys.time(),
    ambassadors_source = amb$source_label %||% NA_character_,
    rscg_source = attr(rscg, "source_label", exact = TRUE) %||% NA_character_,
    meetings = list(
      amb_n = nrow(amb_m),
      amb_with_rscg_id = length(amb_mids),
      matched = sum(amb_mids %in% rscg_mids),
      match_rate = if (length(amb_mids)) mean(amb_mids %in% rscg_mids) else NA_real_,
      unmatched = length(setdiff(amb_mids, rscg_mids))
    ),
    leads = list(
      amb_n = nrow(amb_c),
      amb_with_rscg_id = length(amb_lids),
      matched_meeting_lead = sum(amb_lids %in% rscg_leads),
      matched_sale_client = sum(amb_lids %in% rscg_clients),
      matched_either = sum(amb_lids %in% rscg_people),
      match_rate = if (length(amb_lids)) mean(amb_lids %in% rscg_people) else NA_real_
    ),
    local_meeting_contact = list(
      amb_meetings = nrow(amb_m),
      linked_to_amb_contact = sum(amb_m$amb_contact_id %in% amb_c$amb_contact_id, na.rm = TRUE)
    )
  )
}

#' Dataset canónico por origen (NxtGen / Broker / …).
origenes_build_origin_canonical <- function(origin_label,
                                            amb,
                                            rscg,
                                            as_of = Sys.Date(),
                                            root = Sys.getenv("ORIGENES_APP_ROOT", "."),
                                            name_map = NULL) {
  target_origin_key <- origenes_origin_key(origin_label)
  amb_c <- origenes_standardize_amb_contacts(amb$contact)
  amb_m <- origenes_standardize_amb_meetings(amb$meeting)
  amb_team <- amb$team %||% tibble::tibble()

  meetings_clean <- rscg$meetings_clean %||% tibble::tibble()
  sales_clean <- rscg$sales_clean %||% tibble::tibble()
  meetings_raw <- rscg$meetings_raw %||% tibble::tibble()

  # Embajadores del hub (registro): Type_OS manda; Origin_OS suele venir vacío.
  embajadores <- amb_c |> dplyr::filter(.data$is_ambassador)
  if (identical(target_origin_key, "nxtgen")) {
    embajadores <- embajadores |> dplyr::filter(.data$is_nxtgen_ambassador)
  } else if (identical(target_origin_key, "broker")) {
    embajadores <- embajadores |> dplyr::filter(.data$is_broker_ambassador)
  } else if (target_origin_key %in% c("embajadorotro", "embajador_otro")) {
    embajadores <- embajadores |> dplyr::filter(.data$is_other_ambassador)
  } else {
    embajadores <- embajadores |> dplyr::filter(.data$origin_key == target_origin_key)
  }

  prospectos_hub <- amb_c |>
    dplyr::filter(.data$is_prospecto, .data$origin_key == target_origin_key)

  meetings_hub <- amb_m |>
    dplyr::filter(.data$origin_key == target_origin_key)

  # Verdad comercial: Bubble
  meetings_rscg <- meetings_clean |>
    dplyr::filter(origenes_origin_key(.data$origen) == target_origin_key) |>
    dplyr::mutate(
      rscg_meeting_id = origenes_id_norm(.data$id),
      lead_contact_id = origenes_id_norm(.data$lead_contact_id)
    )

  # Enriquecer con hub meeting + Ambassador crudo de Bubble
  amb_by_meeting <- meetings_hub |>
    dplyr::select(
      rscg_meeting_id,
      amb_meeting_id,
      amb_contact_id,
      hub_prospecting_method = prospecting_method,
      hub_project_os = project_os,
      hub_intention_os = intention_os,
      hub_origin_team_id = origin_team_id
    ) |>
    dplyr::distinct(.data$rscg_meeting_id, .keep_all = TRUE)

  ambassador_raw <- if (nrow(meetings_raw) && "Ambassador" %in% names(meetings_raw)) {
    tibble::tibble(
      rscg_meeting_id = origenes_id_norm(meetings_raw$X_id),
      embajador_bubble = origenes_id_norm(meetings_raw$Ambassador)
    ) |>
      dplyr::distinct(.data$rscg_meeting_id, .keep_all = TRUE)
  } else {
    tibble::tibble(rscg_meeting_id = character(), embajador_bubble = character())
  }

  meetings <- meetings_rscg |>
    dplyr::left_join(amb_by_meeting, by = "rscg_meeting_id") |>
    dplyr::left_join(ambassador_raw, by = "rscg_meeting_id") |>
    dplyr::left_join(
      amb_c |>
        dplyr::select(
          amb_contact_id,
          embajador_hub_nombre = nombre,
          embajador_hub_type = type_os,
          rscg_lead_id_hub = rscg_lead_id
        ),
      by = "amb_contact_id"
    ) |>
    dplyr::mutate(
      join_meeting_hub = !is.na(.data$amb_meeting_id),
      embajador_raw = dplyr::coalesce(.data$embajador_bubble, .data$embajador_hub_nombre),
      embajador = .data$embajador_raw,
      embajador_key = brokers_norm_name(.data$embajador)
    )

  # Conciliación de nombres (lista oficial + name_map Brokers).
  observed <- c(
    meetings$embajador_raw,
    embajadores$nombre,
    if ("embajador_bubble" %in% names(meetings)) meetings$embajador_bubble else NULL,
    if ("embajador_hub_nombre" %in% names(meetings)) meetings$embajador_hub_nombre else NULL
  )
  if (is.null(name_map)) {
    name_map <- origenes_build_name_map(
      target_origin_key,
      observed_names = observed,
      root = root
    )
  }
  meetings <- origenes_apply_name_map(
    meetings, "embajador", key_col = "embajador_key", name_map = name_map
  )
  if (nrow(embajadores) && "nombre" %in% names(embajadores)) {
    embajadores <- embajadores |>
      dplyr::mutate(
        nombre_raw = .data$nombre,
        nombre = origenes_resolve_names(.data$nombre, name_map),
        nombre_key = brokers_norm_name(.data$nombre)
      )
  }

  sales <- sales_clean |>
    dplyr::filter(origenes_origin_key(.data$origen) == target_origin_key) |>
    dplyr::mutate(
      rscg_sale_id = origenes_id_norm(.data$id),
      client_contact_id = origenes_id_norm(.data$client_contact_id)
    ) |>
    dplyr::left_join(
      amb_c |>
        dplyr::filter(!is.na(.data$rscg_lead_id)) |>
        dplyr::select(
          client_contact_id = rscg_lead_id,
          amb_contact_id,
          amb_contact_type = type_os,
          amb_contact_nombre = nombre
        ) |>
        dplyr::distinct(.data$client_contact_id, .keep_all = TRUE),
      by = "client_contact_id"
    ) |>
    dplyr::mutate(join_contact_hub = !is.na(.data$amb_contact_id))

  teams <- if (nrow(amb_team) && "Predefined.Origin_OS" %in% names(amb_team)) {
    amb_team |>
      dplyr::filter(origenes_origin_key(.data$Predefined.Origin_OS) == target_origin_key)
  } else {
    amb_team[0, , drop = FALSE]
  }

  list(
    origin = origin_label,
    origin_key = target_origin_key,
    as_of = as_of,
    embajadores = embajadores,
    prospectos_hub = prospectos_hub,
    meetings_hub = meetings_hub,
    meetings = meetings,
    sales = sales,
    teams = teams,
    name_map = name_map,
    meta = list(
      n_embajadores = nrow(embajadores),
      n_prospectos_hub = nrow(prospectos_hub),
      n_meetings_rscg = nrow(meetings),
      n_meetings_hub = nrow(meetings_hub),
      n_meetings_joined_hub = sum(meetings$join_meeting_hub, na.rm = TRUE),
      n_sales = nrow(sales),
      n_sales_joined_hub = sum(sales$join_contact_hub, na.rm = TRUE),
      n_teams = nrow(teams),
      n_name_map = nrow(name_map),
      n_embajadores_reconciled = if ("embajador_raw" %in% names(meetings)) {
        sum(
          !is.na(meetings$embajador_raw) &
            !is.na(meetings$embajador) &
            meetings$embajador_raw != meetings$embajador,
          na.rm = TRUE
        )
      } else {
        0L
      }
    )
  )
}

origenes_build_all_canonical <- function(root = Sys.getenv("ORIGENES_APP_ROOT", "."),
                                         origins = c("NxtGen", "Broker"),
                                         as_of = Sys.Date()) {
  amb <- origenes_load_ambassadors_raw(root)
  rscg <- origenes_load_rscg_bundle(root)
  diagnostics <- origenes_join_diagnostics(amb, rscg)

  by_origin <- lapply(origins, function(origin) {
    origenes_build_origin_canonical(
      origin,
      amb = amb,
      rscg = rscg,
      as_of = as_of,
      root = root
    )
  })
  names(by_origin) <- vapply(origins, origenes_origin_key, character(1))

  list(
    built_at = Sys.time(),
    as_of = as_of,
    ambassadors_source = amb$source_label,
    rscg_source = attr(rscg, "source_label", exact = TRUE),
    diagnostics = diagnostics,
    origins = by_origin
  )
}

origenes_save_canonical <- function(canonical,
                                    root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  out_dir <- file.path(root, "data", "canonical")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(out_dir, "origenes_joined.rds")
  saveRDS(canonical, path, compress = "gzip")

  # Diagnóstico legible
  diag <- canonical$diagnostics
  summary_df <- tibble::tibble(
    metric = c(
      "meetings_amb",
      "meetings_amb_with_rscg_id",
      "meetings_matched",
      "meetings_match_rate",
      "leads_amb_with_rscg_id",
      "leads_matched_either",
      "leads_match_rate"
    ),
    value = c(
      diag$meetings$amb_n,
      diag$meetings$amb_with_rscg_id,
      diag$meetings$matched,
      round(100 * diag$meetings$match_rate, 1),
      diag$leads$amb_with_rscg_id,
      diag$leads$matched_either,
      round(100 * diag$leads$match_rate, 1)
    )
  )
  utils::write.csv(
    summary_df,
    file.path(out_dir, "join_diagnostics.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  for (key in names(canonical$origins)) {
    origin_dir <- file.path(out_dir, key)
    dir.create(origin_dir, recursive = TRUE, showWarnings = FALSE)
    obj <- canonical$origins[[key]]
    saveRDS(obj$meetings, file.path(origin_dir, "meetings.rds"))
    saveRDS(obj$sales, file.path(origin_dir, "sales.rds"))
    saveRDS(obj$embajadores, file.path(origin_dir, "embajadores.rds"))
    saveRDS(obj$prospectos_hub, file.path(origin_dir, "prospectos_hub.rds"))
    saveRDS(obj$meta, file.path(origin_dir, "meta.rds"))
  }

  invisible(path)
}
