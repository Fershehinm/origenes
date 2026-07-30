# Conciliación de nombres de embajadores (NxtGen / Brokers).
# Las listas oficiales evitan duplicados por spelling distinto en Bubble/hub.

origenes_name_paths <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  list(
    nxtgen_lista = file.path(
      root, "data", "nxtgen", "NxtGen Analytics - Lista de Embajadores.csv"
    ),
    brokers_lista = file.path(
      root, "data", "brokers", "raw",
      "Brokers Academy Analytics - Lista de Brokers.csv"
    ),
    brokers_name_map = file.path(
      root, "data", "brokers", "canonical", "name_map.csv"
    ),
    brokers_registros = file.path(
      root, "data", "brokers", "raw",
      "Brokers Academy Analytics - Registros de embajadores.csv"
    )
  )
}

#' Lee export tipo Sheets: fila 1 vacía, fila 2 encabezados, col "Embajador Registrado".
origenes_read_embajador_lista <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(
      embajador = character(),
      n_citas = numeric(),
      dias_desde_ultima = numeric()
    ))
  }
  raw <- utils::read.csv(
    path,
    skip = 1L,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
  if (!ncol(raw)) {
    return(tibble::tibble(
      embajador = character(),
      n_citas = numeric(),
      dias_desde_ultima = numeric()
    ))
  }

  keys <- gsub("[^a-z0-9]", "", tolower(names(raw)))
  name_col <- which(keys %in% c("embajadorregistrado", "embajador", "nombre"))[1]
  if (is.na(name_col)) {
    # Primera columna con texto no vacío (a menudo la 2ª tras una vacía)
    for (j in seq_len(ncol(raw))) {
      vals <- stringr::str_squish(as.character(raw[[j]]))
      if (any(nzchar(vals) & !is.na(vals))) {
        name_col <- j
        break
      }
    }
  }
  if (is.na(name_col)) {
    return(tibble::tibble(
      embajador = character(),
      n_citas = numeric(),
      dias_desde_ultima = numeric()
    ))
  }

  citas_col <- which(grepl("citas", keys))[1]
  dias_col <- which(grepl("dias|d[ií]as", keys) | grepl("ultima|ltima", keys))[1]

  out <- tibble::tibble(
    embajador = stringr::str_squish(as.character(raw[[name_col]])),
    n_citas = if (!is.na(citas_col)) {
      suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(raw[[citas_col]]))))
    } else {
      NA_real_
    },
    dias_desde_ultima = if (!is.na(dias_col)) {
      suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(raw[[dias_col]]))))
    } else {
      NA_real_
    }
  ) |>
    dplyr::filter(
      !is.na(.data$embajador),
      nzchar(.data$embajador),
      !grepl("^suma\\s*total$", tolower(.data$embajador))
    ) |>
    dplyr::mutate(embajador_key = brokers_norm_name(.data$embajador)) |>
    dplyr::distinct(.data$embajador_key, .keep_all = TRUE)

  out
}

origenes_load_brokers_name_map_csv <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(
      nombre_key = character(),
      embajador = character(),
      fuente_mapa = character(),
      embajador_key = character()
    ))
  }
  nm <- utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  tibble::tibble(
    nombre_key = brokers_norm_name(nm$nombre_key %||% nm$nombre_raw),
    embajador = stringr::str_squish(as.character(nm$embajador)),
    fuente_mapa = as.character(nm$fuente_mapa %||% "name_map"),
    embajador_key = brokers_norm_name(nm$embajador)
  ) |>
    dplyr::filter(!is.na(.data$nombre_key), nzchar(.data$nombre_key), nzchar(.data$embajador)) |>
    dplyr::distinct(.data$nombre_key, .keep_all = TRUE)
}

#' Empareja un nombre normalizado a la lista canónica (exacto → contención única → 1º+último).
origenes_match_key_to_lista <- function(key, lista_keys, lista_names) {
  if (is.na(key) || !nzchar(key) || !length(lista_keys)) {
    return(NA_character_)
  }
  exact <- match(key, lista_keys)
  if (!is.na(exact)) {
    return(lista_names[[exact]])
  }

  toks <- strsplit(key, " ", fixed = TRUE)[[1]]
  toks <- toks[nzchar(toks)]
  if (!length(toks)) {
    return(NA_character_)
  }

  lista_toks <- strsplit(lista_keys, " ", fixed = TRUE)
  contain_hits <- which(vapply(lista_toks, function(lt) {
    all(toks %in% lt) || all(lt %in% toks)
  }, logical(1)))
  if (length(contain_hits) == 1L) {
    return(lista_names[[contain_hits]])
  }

  if (length(toks) >= 2L) {
    first <- toks[[1]]
    last <- toks[[length(toks)]]
    fl_hits <- which(vapply(lista_toks, function(lt) {
      length(lt) >= 2L &&
        identical(lt[[1]], first) &&
        identical(lt[[length(lt)]], last)
    }, logical(1)))
    if (length(fl_hits) == 1L) {
      return(lista_names[[fl_hits]])
    }
  }

  NA_character_
}

origenes_map_names_to_lista <- function(names_raw, lista, fuente = "lista") {
  names_raw <- stringr::str_squish(as.character(names_raw))
  keep <- !is.na(names_raw) & nzchar(names_raw)
  if (!any(keep) || !nrow(lista)) {
    return(tibble::tibble(
      nombre_key = character(),
      embajador = character(),
      fuente_mapa = character(),
      embajador_key = character()
    ))
  }

  uniq <- unique(names_raw[keep])
  keys <- brokers_norm_name(uniq)
  resolved <- vapply(
    keys,
    origenes_match_key_to_lista,
    character(1),
    lista_keys = lista$embajador_key,
    lista_names = lista$embajador
  )

  tibble::tibble(
    nombre_raw = uniq,
    nombre_key = keys,
    embajador = resolved,
    fuente_mapa = dplyr::if_else(is.na(resolved), NA_character_, fuente)
  ) |>
    dplyr::filter(!is.na(.data$embajador)) |>
    dplyr::mutate(embajador_key = brokers_norm_name(.data$embajador)) |>
    dplyr::select("nombre_key", "embajador", "fuente_mapa", "embajador_key") |>
    dplyr::distinct(.data$nombre_key, .keep_all = TRUE)
}

#' Construye mapa nombre_raw_key → embajador canónico para un origen.
origenes_build_name_map <- function(origin_key,
                                    observed_names = character(),
                                    root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  origin_key <- origenes_origin_key(origin_key)
  paths <- origenes_name_paths(root)
  parts <- list()

  if (identical(origin_key, "broker")) {
    parts[[length(parts) + 1L]] <- origenes_load_brokers_name_map_csv(paths$brokers_name_map)
    lista <- origenes_read_embajador_lista(paths$brokers_lista)
  } else {
    lista <- origenes_read_embajador_lista(paths$nxtgen_lista)
  }

  # Identidad: cada nombre de lista se mapea a sí mismo
  if (nrow(lista)) {
    parts[[length(parts) + 1L]] <- tibble::tibble(
      nombre_key = lista$embajador_key,
      embajador = lista$embajador,
      fuente_mapa = "lista",
      embajador_key = lista$embajador_key
    )
  }

  # Observados (Bubble / hub) → lista
  if (length(observed_names) && nrow(lista)) {
    parts[[length(parts) + 1L]] <- origenes_map_names_to_lista(
      observed_names,
      lista,
      fuente = "lista_match"
    )
  }

  if (!length(parts)) {
    return(tibble::tibble(
      nombre_key = character(),
      embajador = character(),
      fuente_mapa = character(),
      embajador_key = character()
    ))
  }

  dplyr::bind_rows(parts) |>
    dplyr::filter(!is.na(.data$nombre_key), nzchar(.data$nombre_key), nzchar(.data$embajador)) |>
    dplyr::group_by(.data$nombre_key) |>
    dplyr::summarise(
      embajador = dplyr::coalesce(
        dplyr::first(.data$embajador[.data$fuente_mapa == "alias_manual"]),
        dplyr::first(.data$embajador[.data$fuente_mapa == "conciliacion"]),
        dplyr::first(.data$embajador[.data$fuente_mapa == "lista"]),
        dplyr::first(.data$embajador[.data$fuente_mapa == "lista_match"]),
        dplyr::first(.data$embajador)
      ),
      fuente_mapa = dplyr::coalesce(
        dplyr::first(.data$fuente_mapa[.data$fuente_mapa == "alias_manual"]),
        dplyr::first(.data$fuente_mapa[.data$fuente_mapa == "conciliacion"]),
        dplyr::first(.data$fuente_mapa[.data$fuente_mapa == "lista"]),
        dplyr::first(.data$fuente_mapa[.data$fuente_mapa == "lista_match"]),
        dplyr::first(.data$fuente_mapa)
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(embajador_key = brokers_norm_name(.data$embajador))
}

origenes_resolve_names <- function(x, name_map) {
  x <- stringr::str_squish(as.character(x))
  if (!nrow(name_map)) {
    return(x)
  }
  key <- brokers_norm_name(x)
  idx <- match(key, name_map$nombre_key)
  resolved <- name_map$embajador[idx]
  dplyr::coalesce(resolved, x)
}

origenes_apply_name_map <- function(df, name_col, key_col = NULL, name_map) {
  if (!nrow(df) || !name_col %in% names(df) || !nrow(name_map)) {
    return(df)
  }
  resolved <- origenes_resolve_names(df[[name_col]], name_map)
  df[[name_col]] <- resolved
  if (!is.null(key_col)) {
    df[[key_col]] <- brokers_norm_name(resolved)
  }
  df
}
