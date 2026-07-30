# Datos de Brokers para Orígenes.
#
# Paso 2: tablas canónicas desde data/brokers/raw/
# Conversiones alineadas:
#   1) registro → 1ª cita
#   2) cita agendada → realizada  (denominador = todas las citas programadas)
#   3) realizada → venta          (solo Firmado; "en proceso" aparte)
# Matching cita↔venta: resuelto para Firmadas sin Gen Cita en CSV 2026
# (citas en app de 2025 / recompra; fuera del periodo Academy).

brokers_raw_dir <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  file.path(root, "data", "brokers", "raw")
}

brokers_canonical_dir <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  file.path(root, "data", "brokers", "canonical")
}

brokers_raw_paths <- function(root = Sys.getenv("ORIGENES_APP_ROOT", ".")) {
  raw <- brokers_raw_dir(root)
  list(
    citas = file.path(raw, "Brokers Academy Analytics - Citas.csv"),
    citas_embajadores = file.path(raw, "Brokers Academy Analytics - citas de embajadores.csv"),
    ventas = file.path(raw, "Brokers Academy Analytics - Ventas.csv"),
    registros = file.path(raw, "Brokers Academy Analytics - Registros de embajadores.csv")
  )
}

brokers_read_csv <- function(path) {
  if (!file.exists(path)) {
    stop("No se encuentra el archivo: ", path, call. = FALSE)
  }
  utils::read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM",
    na.strings = c("", "NA", "#DIV/0!", "#N/A")
  )
}

brokers_norm_name <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  stringr::str_squish(x)
}

brokers_parse_datetime <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  suppressWarnings(lubridate::parse_date_time(
    as.character(x),
    orders = c(
      "dmy HMS", "dmy HM", "dmy",
      "mdy HMS", "mdy HM", "mdy",
      "Ymd HMS", "Ymd HM", "Ymd"
    ),
    quiet = TRUE,
    tz = "UTC"
  ))
}

brokers_parse_date <- function(x) {
  as.Date(brokers_parse_datetime(x))
}

brokers_parse_money <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
}

brokers_parse_gen <- function(fecha) {
  dplyr::if_else(
    is.na(fecha),
    NA_character_,
    paste(format(fecha, "%Y"), format(fecha, "%m"), sep = " - ")
  )
}

brokers_empty_citas <- function() {
  tibble::tibble(
    cita_id = character(),
    prospecto = character(),
    prospecto_key = character(),
    fecha_inicio = as.POSIXct(character(), tz = "UTC"),
    fecha_cita = as.Date(character()),
    fecha_creacion = as.Date(character()),
    gen_cita = character(),
    mes_incompleto = logical(),
    dia_semana = character(),
    es_fin_semana = logical(),
    estatus = character(),
    realizada = logical(),
    cancelada = logical(),
    agendada_pendiente = logical(),
    primera_cita = logical(),
    vendedor = character(),
    vendedor_rol = character(),
    embajador_raw = character(),
    embajador = character(),
    embajador_key = character(),
    embajador_registrado_raw = character(),
    fuente = character()
  )
}

brokers_empty_embajadores <- function() {
  tibble::tibble(
    embajador_id = character(),
    nombre_raw = character(),
    embajador = character(),
    embajador_key = character(),
    fecha_registro = as.Date(character()),
    gen_registro = character(),
    generacion = character(),
    fecha_graduacion = as.Date(character()),
    graduado = logical(),
    fecha_primera_cita = as.Date(character()),
    fecha_ultima_cita_realizada = as.Date(character()),
    fecha_ultima_cita_cualquier = as.Date(character()),
    dias_desde_ultima_realizada = numeric(),
    ciclo_registro_primera_cita = numeric(),
    tiene_primera_cita = logical(),
    activo_30d = logical()
  )
}

brokers_empty_ventas <- function() {
  tibble::tibble(
    venta_id = character(),
    fecha_firma = as.Date(character()),
    gen_venta = character(),
    mes_incompleto = logical(),
    cliente = character(),
    cliente_key = character(),
    proyecto = character(),
    id_propiedad = character(),
    vendedor = character(),
    vendedor_rol = character(),
    estatus_raw = character(),
    es_firmado = logical(),
    es_en_proceso = logical(),
    cuenta_conversion = logical(),
    cuenta_proyeccion = logical(),
    precio = numeric(),
    m2 = numeric(),
    intencion = character(),
    recompra = logical(),
    conciliacion_lead = character(),
    fecha_primera_cita = as.Date(character()),
    gen_cita = character(),
    ciclo_venta = numeric(),
    match_cita_id = character(),
    match_confianza = character()
  )
}

# Mauricio = Origin Leader + vendedor que sí toma citas.
brokers_vendedor_rol <- function(vendedor) {
  dplyr::case_when(
    brokers_norm_name(vendedor) == "mauricio sanchez" ~ "origin_leader",
    is.na(vendedor) | !nzchar(vendedor) ~ NA_character_,
    TRUE ~ "vendedor"
  )
}

brokers_estatus_flags <- function(estatus) {
  key <- origenes_normalize_key(estatus)
  list(
    realizada = key == "realizada",
    cancelada = key == "cancelada",
    agendada_pendiente = key == "agendada"
  )
}

# Mapa provisional de nombres de embajador.
# Preferimos Conciliacion Nombre cuando existe; si no, el spelling más frecuente.
# Pendiente de validación humana (punto 5).
brokers_build_name_map <- function(registros, citas_embajador_raw, citas_raw) {
  conciliados <- registros |>
    dplyr::filter(!is.na(.data$conciliacion) & nzchar(.data$conciliacion)) |>
    dplyr::transmute(
      nombre_raw = .data$nombre,
      embajador = stringr::str_squish(.data$conciliacion),
      fuente_mapa = "conciliacion"
    )

  observed <- tibble::tibble(
    nombre_raw = c(
      citas_embajador_raw$Embajador,
      citas_raw[["Embajador"]],
      citas_raw[["Embajador Registrado"]],
      registros$nombre
    )
  ) |>
    dplyr::filter(!is.na(.data$nombre_raw) & nzchar(.data$nombre_raw)) |>
    dplyr::mutate(nombre_raw = stringr::str_squish(.data$nombre_raw)) |>
    dplyr::count(.data$nombre_raw, name = "n") |>
    dplyr::arrange(dplyr::desc(.data$n), .data$nombre_raw)

  # Alias heurísticos obvios vistos en el export
  manual_aliases <- tibble::tibble(
    nombre_raw = c(
      "Daniel Pascal",
      "Daniel Cobo Pascal",
      "Paulina Barragán",
      "Paulina Barragan",
      "Gicela Cruz",
      "Gicela Cruz Castañeda",
      "Gisela Castañeda",
      "Gicela Castañeda Cruz",
      "Rodrigo Espriu",
      "Rodrigo Daniel Espriu Aguilar",
      "Angela Betancour",
      "Angela Betancur",
      "Ulises franco",
      "Ulises Franco",
      "Maria Del Carmen Buenfil Suarez",
      "María Del Carmen Buenfil Suarez",
      "Cesar Rendon",
      "Cesar Eduardo Rendon Zapata",
      "Lorena Sanchez Aldana",
      "Lorena Sánchez Aldana González"
    ),
    embajador = c(
      "Daniel Cobo Pascal",
      "Daniel Cobo Pascal",
      "Paulina Barragan",
      "Paulina Barragan",
      "Gicela Castañeda Cruz",
      "Gicela Castañeda Cruz",
      "Gicela Castañeda Cruz",
      "Gicela Castañeda Cruz",
      "Rodrigo Daniel Espriu Aguilar",
      "Rodrigo Daniel Espriu Aguilar",
      "Angela Betancur",
      "Angela Betancur",
      "Ulises Franco",
      "Ulises Franco",
      "María Del Carmen Buenfil Suarez",
      "María Del Carmen Buenfil Suarez",
      "Cesar Eduardo Rendon Zapata",
      "Cesar Eduardo Rendon Zapata",
      "Lorena Sánchez Aldana González",
      "Lorena Sánchez Aldana González"
    ),
    fuente_mapa = "alias_manual"
  )

  from_conciliacion <- conciliados |>
    dplyr::mutate(
      key = brokers_norm_name(.data$nombre_raw),
      canon_key = brokers_norm_name(.data$embajador)
    ) |>
    dplyr::distinct(.data$key, .keep_all = TRUE)

  from_manual <- manual_aliases |>
    dplyr::mutate(
      key = brokers_norm_name(.data$nombre_raw),
      canon_key = brokers_norm_name(.data$embajador)
    ) |>
    dplyr::distinct(.data$key, .keep_all = TRUE)

  # Para nombres sin mapa: canónico = spelling más frecuente del mismo key
  by_key <- observed |>
    dplyr::mutate(key = brokers_norm_name(.data$nombre_raw)) |>
    dplyr::group_by(.data$key) |>
    dplyr::arrange(dplyr::desc(.data$n), .data$nombre_raw, .by_group = TRUE) |>
    dplyr::summarise(
      embajador = dplyr::first(.data$nombre_raw),
      n = sum(.data$n),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      nombre_raw = .data$embajador,
      fuente_mapa = "frecuencia",
      canon_key = .data$key
    )

  dplyr::bind_rows(
    from_manual |> dplyr::select("nombre_raw", "embajador", "fuente_mapa", "key", "canon_key"),
    from_conciliacion |> dplyr::select("nombre_raw", "embajador", "fuente_mapa", "key", "canon_key"),
    by_key |> dplyr::select("nombre_raw", "embajador", "fuente_mapa", "key", "canon_key")
  ) |>
    dplyr::group_by(.data$key) |>
    dplyr::summarise(
      embajador = dplyr::coalesce(
        dplyr::first(.data$embajador[.data$fuente_mapa == "alias_manual"]),
        dplyr::first(.data$embajador[.data$fuente_mapa == "conciliacion"]),
        dplyr::first(.data$embajador)
      ),
      fuente_mapa = dplyr::coalesce(
        dplyr::first(.data$fuente_mapa[.data$fuente_mapa == "alias_manual"]),
        dplyr::first(.data$fuente_mapa[.data$fuente_mapa == "conciliacion"]),
        dplyr::first(.data$fuente_mapa)
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      embajador = stringr::str_squish(.data$embajador),
      embajador_key = brokers_norm_name(.data$embajador)
    ) |>
    dplyr::rename(nombre_key = "key")
}

brokers_resolve_name <- function(x, name_map) {
  key <- brokers_norm_name(x)
  idx <- match(key, name_map$nombre_key)
  resolved <- name_map$embajador[idx]
  dplyr::coalesce(resolved, stringr::str_squish(as.character(x)))
}

brokers_month_incomplete_flag <- function(fecha, as_of = Sys.Date()) {
  dplyr::if_else(
    is.na(fecha),
    FALSE,
    format(fecha, "%Y-%m") == format(as_of, "%Y-%m") &
      as_of < (lubridate::ceiling_date(as_of, "month") - 1)
  )
}

brokers_load_registros_raw <- function(path) {
  raw <- brokers_read_csv(path)
  tibble::tibble(
    nombre = stringr::str_squish(raw[["Nombre"]]),
    conciliacion = stringr::str_squish(raw[["Conciliacion Nombre"]]),
    fecha_registro = brokers_parse_date(raw[["Fecha de registro"]]),
    gen_registro = stringr::str_squish(raw[["Gen Registro"]]),
    generacion = stringr::str_squish(gsub("[()]", "", raw[["Generación"]])),
    fecha_graduacion = brokers_parse_date(raw[["Fecha graduación"]]),
    fecha_primera_cita = brokers_parse_date(raw[["Fecha primera cita realizada"]]),
    fecha_ultima_cita_realizada = brokers_parse_date(raw[["Fecha de última cita realizada"]]),
    fecha_ultima_cita_cualquier = brokers_parse_date(
      raw[["Fecha de última cita agendada/realizada"]]
    ),
    dias_desde_ultima_realizada = suppressWarnings(
      as.numeric(raw[["Días desde la última cita realizadas"]])
    ),
    ciclo_registro_primera_cita = suppressWarnings(
      as.numeric(raw[["Ciclo Registro / Primera Cita"]])
    )
  )
}

brokers_build_citas <- function(paths, name_map, as_of = Sys.Date()) {
  emb <- brokers_read_csv(paths$citas_embajadores)
  cit <- brokers_read_csv(paths$citas)

  base <- tibble::tibble(
    prospecto = stringr::str_squish(emb[["Nombre del Prospecto"]]),
    fecha_inicio = brokers_parse_datetime(emb[["Fecha de Inicio"]]),
    estatus = stringr::str_squish(emb[["Estatus"]]),
    vendedor = stringr::str_squish(emb[["Vendedor"]]),
    embajador_raw = stringr::str_squish(emb[["Embajador"]]),
    primera_cita_raw = tolower(stringr::str_squish(emb[["Primera Cita"]])),
    fecha_creacion = brokers_parse_date(emb[["Fecha de creación"]]),
    fuente = "citas_embajadores"
  ) |>
    dplyr::mutate(
      prospecto_key = brokers_norm_name(.data$prospecto),
      join_key = paste(.data$prospecto_key, as.character(.data$fecha_inicio), sep = "|")
    )

  cit_aux <- tibble::tibble(
    prospecto = stringr::str_squish(cit[["Nombre del Prospecto"]]),
    fecha_inicio = brokers_parse_datetime(cit[["Fecha de Inicio"]]),
    embajador_registrado_raw = stringr::str_squish(cit[["Embajador Registrado"]]),
    gen_cita_src = stringr::str_squish(cit[["Gen Cita"]])
  ) |>
    dplyr::mutate(
      prospecto_key = brokers_norm_name(.data$prospecto),
      join_key = paste(.data$prospecto_key, as.character(.data$fecha_inicio), sep = "|")
    ) |>
    dplyr::distinct(.data$join_key, .keep_all = TRUE) |>
    dplyr::select("join_key", "embajador_registrado_raw", "gen_cita_src")

  flags <- brokers_estatus_flags(base$estatus)

  out <- base |>
    dplyr::left_join(cit_aux, by = "join_key") |>
    dplyr::mutate(
      fecha_cita = as.Date(.data$fecha_inicio),
      gen_cita = dplyr::coalesce(.data$gen_cita_src, brokers_parse_gen(.data$fecha_cita)),
      mes_incompleto = brokers_month_incomplete_flag(.data$fecha_cita, as_of = as_of),
      dia_semana = dplyr::case_when(
        is.na(.data$fecha_cita) ~ NA_character_,
        lubridate::wday(.data$fecha_cita, week_start = 1) == 1 ~ "lunes",
        lubridate::wday(.data$fecha_cita, week_start = 1) == 2 ~ "martes",
        lubridate::wday(.data$fecha_cita, week_start = 1) == 3 ~ "miercoles",
        lubridate::wday(.data$fecha_cita, week_start = 1) == 4 ~ "jueves",
        lubridate::wday(.data$fecha_cita, week_start = 1) == 5 ~ "viernes",
        lubridate::wday(.data$fecha_cita, week_start = 1) == 6 ~ "sabado",
        TRUE ~ "domingo"
      ),
      es_fin_semana = .data$dia_semana %in% c("sabado", "domingo"),
      realizada = flags$realizada,
      cancelada = flags$cancelada,
      agendada_pendiente = flags$agendada_pendiente,
      primera_cita = .data$primera_cita_raw %in% c("si", "sí", "yes", "true", "1"),
      vendedor_rol = brokers_vendedor_rol(.data$vendedor),
      embajador = brokers_resolve_name(.data$embajador_raw, name_map),
      embajador_key = brokers_norm_name(.data$embajador),
      cita_id = paste0(
        "cita_",
        dplyr::row_number(),
        "_",
        substr(brokers_norm_name(paste(.data$prospecto, .data$fecha_inicio)), 1, 24)
      )
    ) |>
    dplyr::select(
      "cita_id", "prospecto", "prospecto_key", "fecha_inicio", "fecha_cita",
      "fecha_creacion", "gen_cita", "mes_incompleto", "dia_semana", "es_fin_semana",
      "estatus", "realizada", "cancelada", "agendada_pendiente", "primera_cita",
      "vendedor", "vendedor_rol", "embajador_raw", "embajador", "embajador_key",
      "embajador_registrado_raw", "fuente"
    )

  # Filas solo en Citas.csv (sin vendedor) — por si el overlap no es 100%
  only_cit_keys <- setdiff(
    paste(
      brokers_norm_name(cit[["Nombre del Prospecto"]]),
      as.character(brokers_parse_datetime(cit[["Fecha de Inicio"]])),
      sep = "|"
    ),
    base$join_key
  )
  if (length(only_cit_keys)) {
    warning(
      length(only_cit_keys),
      " citas están solo en Citas.csv y no se incorporaron (sin vendedor). ",
      "Revisar overlap de fuentes."
    )
  }

  out
}

brokers_build_embajadores <- function(registros, name_map, as_of = Sys.Date()) {
  registros |>
    dplyr::mutate(
      embajador = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(.data$conciliacion), ""),
        brokers_resolve_name(.data$nombre, name_map)
      ),
      embajador_key = brokers_norm_name(.data$embajador),
      gen_registro = dplyr::coalesce(
        dplyr::na_if(.data$gen_registro, ""),
        brokers_parse_gen(.data$fecha_registro)
      ),
      graduado = !is.na(.data$fecha_graduacion),
      tiene_primera_cita = !is.na(.data$fecha_primera_cita),
      activo_30d = !is.na(.data$fecha_ultima_cita_cualquier) &
        (.data$fecha_ultima_cita_cualquier >= (as_of - 30)),
      embajador_id = paste0("emb_", dplyr::row_number(), "_", substr(.data$embajador_key, 1, 20))
    ) |>
    dplyr::transmute(
      embajador_id = .data$embajador_id,
      nombre_raw = .data$nombre,
      embajador = .data$embajador,
      embajador_key = .data$embajador_key,
      fecha_registro = .data$fecha_registro,
      gen_registro = .data$gen_registro,
      generacion = dplyr::na_if(stringr::str_squish(.data$generacion), ""),
      fecha_graduacion = .data$fecha_graduacion,
      graduado = .data$graduado,
      fecha_primera_cita = .data$fecha_primera_cita,
      fecha_ultima_cita_realizada = .data$fecha_ultima_cita_realizada,
      fecha_ultima_cita_cualquier = .data$fecha_ultima_cita_cualquier,
      dias_desde_ultima_realizada = .data$dias_desde_ultima_realizada,
      ciclo_registro_primera_cita = .data$ciclo_registro_primera_cita,
      tiene_primera_cita = .data$tiene_primera_cita,
      activo_30d = .data$activo_30d
    )
}

brokers_build_ventas <- function(path, as_of = Sys.Date()) {
  raw <- brokers_read_csv(path)
  estatus <- stringr::str_squish(raw[["Status"]])
  estatus_key <- origenes_normalize_key(estatus)

  tibble::tibble(
    fecha_firma = brokers_parse_date(raw[["Fecha de firma"]]),
    cliente = stringr::str_squish(raw[["Nombre de cliente"]]),
    proyecto = stringr::str_squish(raw[["Proyecto"]]),
    id_propiedad = stringr::str_squish(as.character(raw[["ID de propiedad"]])),
    vendedor = stringr::str_squish(raw[["Vendedor"]]),
    estatus_raw = estatus,
    precio = brokers_parse_money(raw[["Precio de venta"]]),
    m2 = suppressWarnings(as.numeric(raw[["M2"]])),
    intencion = stringr::str_squish(raw[["Intencion"]]),
    recompra_raw = tolower(stringr::str_squish(raw[["Recompra"]])),
    conciliacion_lead = stringr::str_squish(raw[["Conciliacion Lead"]]),
    fecha_primera_cita = brokers_parse_date(raw[["Primera Cita"]]),
    gen_cita = stringr::str_squish(raw[["Gen Cita"]]),
    ciclo_venta = suppressWarnings(as.numeric(raw[["Ciclo Venta"]])),
    gen_venta_src = stringr::str_squish(raw[["Gen Venta"]])
  ) |>
    dplyr::mutate(
      cliente_key = brokers_norm_name(.data$cliente),
      gen_venta = dplyr::coalesce(
        dplyr::na_if(.data$gen_venta_src, ""),
        brokers_parse_gen(.data$fecha_firma)
      ),
      mes_incompleto = brokers_month_incomplete_flag(.data$fecha_firma, as_of = as_of),
      vendedor_rol = brokers_vendedor_rol(.data$vendedor),
      es_firmado = grepl("firmado", estatus_key),
      es_en_proceso = grepl("apartado|carta|documentos|intencion|proceso", estatus_key) &
        !.data$es_firmado,
      cuenta_conversion = .data$es_firmado,
      cuenta_proyeccion = .data$es_firmado | .data$es_en_proceso,
      recompra = .data$recompra_raw %in% c("si", "sí", "yes", "true", "1"),
      match_cita_id = NA_character_,
      match_confianza = "pendiente",
      venta_id = paste0(
        "venta_",
        dplyr::row_number(),
        "_",
        substr(paste0(.data$cliente_key, "_", .data$id_propiedad), 1, 28)
      )
    ) |>
    dplyr::select(
      "venta_id", "fecha_firma", "gen_venta", "mes_incompleto", "cliente", "cliente_key",
      "proyecto", "id_propiedad", "vendedor", "vendedor_rol", "estatus_raw",
      "es_firmado", "es_en_proceso", "cuenta_conversion", "cuenta_proyeccion",
      "precio", "m2", "intencion", "recompra", "conciliacion_lead",
      "fecha_primera_cita", "gen_cita", "ciclo_venta", "match_cita_id", "match_confianza"
    )
}

brokers_quality_report <- function(citas, embajadores, ventas, name_map) {
  list(
    citas_n = nrow(citas),
    embajadores_n = nrow(embajadores),
    ventas_n = nrow(ventas),
    ventas_firmadas = sum(ventas$cuenta_conversion, na.rm = TRUE),
    ventas_en_proceso = sum(ventas$es_en_proceso, na.rm = TRUE),
    citas_sin_fecha = sum(is.na(citas$fecha_cita)),
    citas_sin_vendedor = sum(is.na(citas$vendedor) | !nzchar(citas$vendedor)),
    citas_sin_embajador = sum(is.na(citas$embajador) | !nzchar(citas$embajador)),
    tasa_realizada = mean(citas$realizada, na.rm = TRUE),
    share_fin_semana = mean(citas$es_fin_semana, na.rm = TRUE),
    share_origin_leader = mean(citas$vendedor_rol == "origin_leader", na.rm = TRUE),
    embajadores_con_primera_cita = sum(embajadores$tiene_primera_cita, na.rm = TRUE),
    name_map_n = nrow(name_map),
    name_map_fuentes = if (nrow(name_map) && "fuente_mapa" %in% names(name_map)) {
      table(name_map$fuente_mapa)
    } else {
      table(character())
    },
    match_venta_status = if (nrow(ventas) && "match_confianza" %in% names(ventas)) {
      table(ventas$match_confianza, useNA = "ifany")
    } else {
      table(character())
    }
  )
}

brokers_build_canonical <- function(root = Sys.getenv("ORIGENES_APP_ROOT", "."),
                                    as_of = Sys.Date(),
                                    write = TRUE) {
  paths <- brokers_raw_paths(root)
  missing <- paths[!file.exists(unlist(paths))]
  if (length(missing)) {
    stop(
      "Faltan CSV en data/brokers/raw/: ",
      paste(basename(unlist(missing)), collapse = ", "),
      call. = FALSE
    )
  }

  registros <- brokers_load_registros_raw(paths$registros)
  citas_emb_raw <- brokers_read_csv(paths$citas_embajadores)
  citas_raw <- brokers_read_csv(paths$citas)

  name_map <- brokers_build_name_map(registros, citas_emb_raw, citas_raw)
  citas <- brokers_build_citas(paths, name_map, as_of = as_of)
  embajadores <- brokers_build_embajadores(registros, name_map, as_of = as_of)
  ventas <- brokers_build_ventas(paths$ventas, as_of = as_of)
  quality <- brokers_quality_report(citas, embajadores, ventas, name_map)

  canonical <- list(
    citas = citas,
    embajadores = embajadores,
    ventas = ventas,
    name_map = name_map,
    quality = quality,
    meta = list(
      built_at = Sys.time(),
      as_of = as_of,
      definitions = list(
        agendada_a_realizada = "denominador = todas las citas programadas (A)",
        realizada_a_venta = "solo Firmado; en proceso = proyeccion",
        registro_a_primera_cita = "embajadores.tiene_primera_cita / registros del periodo",
        matching_cita_venta = "resuelto: Firmadas sin Gen Cita en CSV 2026 tenian citas en app de 2025 (recompra/ciclo largo); fuera del periodo Academy",
        orlando_corona_lara = "2 unidades distintas (no duplicado)"
      ),
      sources = paths
    )
  )

  if (isTRUE(write)) {
    out_dir <- brokers_canonical_dir(root)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(canonical, file.path(out_dir, "brokers_canonical.rds"))
    utils::write.csv(
      citas,
      file.path(out_dir, "citas.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
    utils::write.csv(
      embajadores,
      file.path(out_dir, "embajadores.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
    # Ventas canónicas sin teléfono/email (el raw sí los tiene)
    utils::write.csv(
      ventas,
      file.path(out_dir, "ventas.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
    utils::write.csv(
      name_map,
      file.path(out_dir, "name_map.csv"),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }

  canonical
}

# --- Dashboard Brokers: carga + tablas (mensual / semanal) -----------------

brokers_active_window_days <- function() {
  days <- suppressWarnings(as.integer(Sys.getenv("ORIGENES_BROKERS_ACTIVE_DAYS", "30")))
  if (!is.finite(days) || days <= 0L) 30L else days
}

brokers_default_start <- function() {
  as.Date("2025-12-01")
}

brokers_default_end <- function(as_of = Sys.Date()) {
  lubridate::ceiling_date(as_of, "month") - 1
}

# Carga datos Brokers solo desde CRM (Bubble).
# Prioridad (ORIGENES_BROKERS_DATA_SOURCE=auto|crm):
#   1) Bundle vendedores (misma extracción Bubble)
#   2) Bubble live (BUBBLE_BASE + BUBBLE_TOKEN)
# No usa CSV / Sheets de Academy.
brokers_load_dashboard <- function(root = Sys.getenv("ORIGENES_APP_ROOT", "."),
                                   as_of = Sys.Date()) {
  mode <- tolower(Sys.getenv("ORIGENES_BROKERS_DATA_SOURCE", unset = "crm"))
  if (!nzchar(mode) || identical(mode, "auto")) {
    mode <- "crm"
  }

  try_vendedores_bundle <- function() {
    path <- Sys.getenv("ORIGENES_VENDEDORES_BUNDLE", unset = "")
    if (!nzchar(path)) {
      path <- file.path(
        dirname(root),
        "data_division",
        "vendedores_dashboard",
        "data",
        "cache",
        "latest.rds"
      )
    }
    if (!file.exists(path)) {
      return(NULL)
    }
    bundle <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.null(bundle)) {
      return(NULL)
    }
    bundle$source <- paste0("vendedores_cache:", basename(dirname(path)))
    object <- brokers_canonical_from_bubble(bundle, as_of = as_of)
    attr(object, "source_path") <- normalizePath(path, winslash = "/", mustWork = FALSE)
    attr(object, "source_label") <- "CRM Bubble (cache vendedores · origen Broker)"
    object
  }

  try_bubble_live <- function() {
    if (!origenes_bubble_configured()) {
      return(NULL)
    }
    bundle <- tryCatch(
      origenes_bubble_fetch_bundle(),
      error = function(e) {
        warning("Bubble live falló: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (is.null(bundle)) {
      return(NULL)
    }
    object <- brokers_canonical_from_bubble(bundle, as_of = as_of)
    attr(object, "source_path") <- NA_character_
    attr(object, "source_label") <- "CRM Bubble live (origen Broker)"
    object
  }

  object <- NULL
  if (mode %in% c("crm", "vendedores", "cache", "bubble", "live")) {
    if (mode %in% c("crm", "vendedores", "cache", "bubble", "live")) {
      object <- try_vendedores_bundle()
    }
    if (is.null(object) && mode %in% c("crm", "bubble", "live")) {
      object <- try_bubble_live()
    }
    if (is.null(object) && identical(mode, "vendedores")) {
      object <- try_bubble_live()
    }
  }

  if (!is.null(object)) {
    return(object)
  }

  empty <- list(
    citas = brokers_empty_citas(),
    embajadores = brokers_empty_embajadores(),
    ventas = brokers_empty_ventas(),
    name_map = tibble::tibble(),
    quality = list(),
    meta = list()
  )
  attr(empty, "source_path") <- NA_character_
  attr(empty, "source_label") <- "sin datos CRM"
  empty
}

brokers_period_starts <- function(start, end, granularity = c("mensual", "semanal")) {
  granularity <- match.arg(granularity)
  start <- as.Date(start)
  end <- as.Date(end)
  if (is.na(start) || is.na(end) || end < start) {
    return(as.Date(character()))
  }
  if (identical(granularity, "mensual")) {
    seq(lubridate::floor_date(start, "month"), lubridate::floor_date(end, "month"), by = "month")
  } else {
    seq(lubridate::floor_date(start, "week", week_start = 1),
        lubridate::floor_date(end, "week", week_start = 1),
        by = "week")
  }
}

brokers_period_label <- function(period_start, granularity = c("mensual", "semanal")) {
  granularity <- match.arg(granularity)
  period_start <- as.Date(period_start)
  if (identical(granularity, "mensual")) {
    paste(format(period_start, "%Y"), format(period_start, "%m"), sep = " - ")
  } else {
    paste0(format(period_start, "%d %b"), " · ", format(period_start + 6, "%d %b"))
  }
}

brokers_period_end <- function(period_start, granularity = c("mensual", "semanal")) {
  granularity <- match.arg(granularity)
  period_start <- as.Date(period_start)
  if (identical(granularity, "mensual")) {
    lubridate::ceiling_date(period_start, "month") - 1
  } else {
    period_start + 6
  }
}

brokers_in_period <- function(fecha, period_start, granularity) {
  pe <- brokers_period_end(period_start, granularity)
  !is.na(fecha) & fecha >= period_start & fecha <= pe
}

brokers_fmt_pct <- function(num, den) {
  dplyr::if_else(
    is.na(den) | den <= 0,
    "—",
    paste0(format(round(100 * num / den, 2), nsmall = 2), "%")
  )
}

brokers_fmt_money <- function(x) {
  dplyr::if_else(
    is.na(x) | !is.finite(x),
    "$0",
    scales::label_dollar(prefix = "$", big.mark = ",", accuracy = 1)(x)
  )
}

# --- Resumen: periodo único (anual / mensual / semanal / Q) ---------------

brokers_resumen_years <- function(canonical = NULL, as_of = Sys.Date()) {
  years <- integer()
  if (!is.null(canonical) && is.data.frame(canonical$citas) && nrow(canonical$citas)) {
    years <- unique(lubridate::year(stats::na.omit(canonical$citas$fecha_cita)))
    years <- years[is.finite(years)]
  }
  y_now <- as.integer(lubridate::year(as_of))
  years <- sort(unique(c(as.integer(years), y_now, y_now - 1L)))
  years[years >= 2020L]
}

brokers_resumen_bounds <- function(granularity = c("anual", "mensual", "semanal", "trimestral"),
                                   year = NULL,
                                   month = NULL,
                                   quarter = NULL,
                                   week_date = NULL,
                                   as_of = Sys.Date()) {
  granularity <- match.arg(granularity)
  year <- suppressWarnings(as.integer(if (is.null(year)) lubridate::year(as_of) else year))
  if (!is.finite(year)) year <- as.integer(lubridate::year(as_of))

  if (identical(granularity, "anual")) {
    start <- as.Date(sprintf("%d-01-01", year))
    end <- as.Date(sprintf("%d-12-31", year))
    label <- as.character(year)
  } else if (identical(granularity, "mensual")) {
    month <- suppressWarnings(as.integer(if (is.null(month)) lubridate::month(as_of) else month))
    if (!is.finite(month) || month < 1L || month > 12L) {
      month <- as.integer(lubridate::month(as_of))
    }
    start <- as.Date(sprintf("%d-%02d-01", year, month))
    end <- lubridate::ceiling_date(start, "month") - 1
    label <- paste(format(start, "%b"), year)
  } else if (identical(granularity, "trimestral")) {
    quarter <- suppressWarnings(as.integer(if (is.null(quarter)) lubridate::quarter(as_of) else quarter))
    if (!is.finite(quarter) || quarter < 1L || quarter > 4L) {
      quarter <- as.integer(lubridate::quarter(as_of))
    }
    start_month <- (quarter - 1L) * 3L + 1L
    start <- as.Date(sprintf("%d-%02d-01", year, start_month))
    end <- lubridate::ceiling_date(start, "quarter") - 1
    label <- paste0("Q", quarter, " ", year)
  } else {
    wd <- as.Date(if (is.null(week_date)) as_of else week_date)
    if (is.na(wd)) wd <- as.Date(as_of)
    start <- lubridate::floor_date(wd, "week", week_start = 1)
    end <- start + 6
    label <- paste0(format(start, "%d %b"), " – ", format(end, "%d %b %Y"))
  }

  list(start = start, end = end, label = label, granularity = granularity)
}

# KPIs de la subpestaña Resumen (un solo periodo).
brokers_metric_resumen <- function(canonical,
                                   start,
                                   end,
                                   as_of = Sys.Date()) {
  start <- as.Date(start)
  end <- as.Date(end)
  as_of <- as.Date(as_of)
  as_of_eff <- min(as_of, end)

  citas <- if (is.data.frame(canonical$citas)) canonical$citas else brokers_empty_citas()
  emb <- if (is.data.frame(canonical$embajadores)) {
    canonical$embajadores
  } else {
    brokers_empty_embajadores()
  }

  active_days <- brokers_active_window_days()
  c_period <- citas |>
    dplyr::filter(
      !is.na(.data$fecha_cita),
      .data$fecha_cita >= start,
      .data$fecha_cita <= end
    )

  n_totales <- nrow(emb)
  activos <- emb |>
    dplyr::filter(
      !is.na(.data$fecha_ultima_cita_cualquier),
      .data$fecha_ultima_cita_cualquier >= (as_of_eff - active_days)
    )
  n_activos <- nrow(activos)
  n_citas <- nrow(c_period)
  n_realizadas <- sum(c_period$realizada, na.rm = TRUE)

  citas_por_activo <- if (n_activos > 0) n_citas / n_activos else NA_real_
  pct_activos <- if (n_totales > 0) n_activos / n_totales else NA_real_

  by_emb <- c_period |>
    dplyr::filter(!is.na(.data$embajador_key), nzchar(.data$embajador_key)) |>
    dplyr::group_by(.data$embajador_key, .data$embajador) |>
    dplyr::summarise(
      n_citas = dplyr::n(),
      n_realizadas = sum(.data$realizada, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      conversion = dplyr::if_else(.data$n_citas > 0, .data$n_realizadas / .data$n_citas, NA_real_)
    ) |>
    dplyr::arrange(dplyr::desc(.data$n_citas), .data$embajador)

  conversion_por_broker <- if (nrow(by_emb)) {
    mean(by_emb$conversion, na.rm = TRUE)
  } else {
    NA_real_
  }

  top15 <- utils::head(by_emb, 15L)
  top15_citas <- sum(top15$n_citas)
  top15_share <- if (n_citas > 0) top15_citas / n_citas else NA_real_
  if (nrow(top15)) {
    top15$max_citas <- max(top15$n_citas, na.rm = TRUE)
    top15$share_bar <- dplyr::if_else(
      top15$max_citas > 0,
      top15$n_citas / top15$max_citas,
      0
    )
  }

  list(
    start = start,
    end = end,
    as_of = as_of_eff,
    active_days = active_days,
    n_citas = n_citas,
    n_realizadas = n_realizadas,
    n_activos = n_activos,
    n_totales = n_totales,
    n_brokers_con_cita = nrow(by_emb),
    citas_por_activo = citas_por_activo,
    conversion_por_broker = conversion_por_broker,
    pct_activos = pct_activos,
    top15_citas = top15_citas,
    top15_share = top15_share,
    top15 = top15
  )
}

# Tabla 1: cohorte por periodo de registro del embajador.
brokers_metric_conversion_embajadores <- function(canonical,
                                                  start,
                                                  end,
                                                  granularity = c("mensual", "semanal"),
                                                  as_of = Sys.Date()) {
  granularity <- match.arg(granularity)
  periods <- brokers_period_starts(start, end, granularity)
  emb <- canonical$embajadores
  active_days <- brokers_active_window_days()
  active_from <- as_of - active_days

  cols <- lapply(periods, function(ps) {
    pe <- brokers_period_end(ps, granularity)
    cohort <- emb |>
      dplyr::filter(
        !is.na(.data$fecha_registro),
        .data$fecha_registro >= ps,
        .data$fecha_registro <= pe
      )
    registrados <- nrow(cohort)
    # Proxy Excel: tuvo alguna cita agendada/realizada (fecha_ultima_cita_cualquier)
    cita_agendada <- sum(!is.na(cohort$fecha_ultima_cita_cualquier), na.rm = TRUE)
    primera_cita <- sum(cohort$tiene_primera_cita, na.rm = TRUE)
    activos <- sum(
      !is.na(cohort$fecha_ultima_cita_cualquier) &
        cohort$fecha_ultima_cita_cualquier >= active_from &
        cohort$fecha_ultima_cita_cualquier <= as_of,
      na.rm = TRUE
    )
    list(
      registrados = registrados,
      cita_agendada = cita_agendada,
      primera_cita = primera_cita,
      activos = activos
    )
  })

  total_reg <- sum(vapply(cols, `[[`, integer(1), "registrados"))
  total_ag <- sum(vapply(cols, `[[`, integer(1), "cita_agendada"))
  total_1ra <- sum(vapply(cols, `[[`, integer(1), "primera_cita"))
  # Activos total = activos actuales en todo el padrón (no suma de cohortes)
  total_activos <- sum(
    !is.na(emb$fecha_ultima_cita_cualquier) &
      emb$fecha_ultima_cita_cualquier >= active_from &
      emb$fecha_ultima_cita_cualquier <= as_of,
    na.rm = TRUE
  )

  period_labels <- vapply(periods, brokers_period_label, character(1), granularity = granularity)

  tibble::tibble(
    metrica = c(
      "Registrados",
      "Cita agendada",
      "Primera cita",
      "Conversión primera cita / registrados",
      paste0("Activos (Cita últimos ", active_days, " días)"),
      "Conversión activo / primera cita"
    ),
    Total = c(
      as.character(total_reg),
      as.character(total_ag),
      as.character(total_1ra),
      brokers_fmt_pct(total_1ra, total_reg),
      as.character(total_activos),
      brokers_fmt_pct(total_activos, total_1ra)
    )
  ) |>
    dplyr::bind_cols(
      as.data.frame(
        stats::setNames(
          lapply(seq_along(cols), function(i) {
            c(
              as.character(cols[[i]]$registrados),
              as.character(cols[[i]]$cita_agendada),
              as.character(cols[[i]]$primera_cita),
              brokers_fmt_pct(cols[[i]]$primera_cita, cols[[i]]$registrados),
              as.character(cols[[i]]$activos),
              brokers_fmt_pct(cols[[i]]$activos, cols[[i]]$primera_cita)
            )
          }),
          period_labels
        ),
        check.names = FALSE
      )
    )
}

# Tabla 2: actividad por periodo de la cita / venta.
brokers_metric_actividad <- function(canonical,
                                     start,
                                     end,
                                     granularity = c("mensual", "semanal")) {
  granularity <- match.arg(granularity)
  periods <- brokers_period_starts(start, end, granularity)
  citas <- canonical$citas
  ventas <- canonical$ventas

  cols <- lapply(periods, function(ps) {
    pe <- brokers_period_end(ps, granularity)
    c_period <- citas |>
      dplyr::filter(!is.na(.data$fecha_cita), .data$fecha_cita >= ps, .data$fecha_cita <= pe)
    v_period <- ventas |>
      dplyr::filter(
        .data$cuenta_conversion,
        !is.na(.data$fecha_firma),
        .data$fecha_firma >= ps,
        .data$fecha_firma <= pe
      )
    list(
      primera_cita = sum(c_period$primera_cita & c_period$realizada, na.rm = TRUE),
      citas_agendadas = nrow(c_period),
      citas_nuevas_realizadas = sum(c_period$primera_cita & c_period$realizada, na.rm = TRUE),
      total_realizadas = sum(c_period$realizada, na.rm = TRUE),
      unidades = nrow(v_period),
      facturacion = sum(v_period$precio, na.rm = TRUE)
    )
  })

  period_labels <- vapply(periods, brokers_period_label, character(1), granularity = granularity)
  totals <- list(
    primera_cita = sum(vapply(cols, `[[`, numeric(1), "primera_cita")),
    citas_agendadas = sum(vapply(cols, `[[`, numeric(1), "citas_agendadas")),
    citas_nuevas_realizadas = sum(vapply(cols, `[[`, numeric(1), "citas_nuevas_realizadas")),
    total_realizadas = sum(vapply(cols, `[[`, numeric(1), "total_realizadas")),
    unidades = sum(vapply(cols, `[[`, numeric(1), "unidades")),
    facturacion = sum(vapply(cols, `[[`, numeric(1), "facturacion"))
  )

  tibble::tibble(
    metrica = c(
      "Primera cita",
      "Citas Agendadas",
      "Citas nuevas Realizadas",
      "Total de citas realizadas",
      "Unidades Vendidas",
      "Facturación"
    ),
    Total = c(
      as.character(totals$primera_cita),
      as.character(totals$citas_agendadas),
      as.character(totals$citas_nuevas_realizadas),
      as.character(totals$total_realizadas),
      as.character(totals$unidades),
      brokers_fmt_money(totals$facturacion)
    )
  ) |>
    dplyr::bind_cols(
      as.data.frame(
        stats::setNames(
          lapply(seq_along(cols), function(i) {
            c(
              as.character(cols[[i]]$primera_cita),
              as.character(cols[[i]]$citas_agendadas),
              as.character(cols[[i]]$citas_nuevas_realizadas),
              as.character(cols[[i]]$total_realizadas),
              as.character(cols[[i]]$unidades),
              brokers_fmt_money(cols[[i]]$facturacion)
            )
          }),
          period_labels
        ),
        check.names = FALSE
      )
    )
}

# Tabla 3: conversión cita → venta (solo Firmado con atribución de periodo;
# excluye firmas cuya única cita conocida es pre-periodo / 2025).
brokers_metric_conversion_citas <- function(canonical,
                                            start,
                                            end,
                                            granularity = c("mensual", "semanal")) {
  granularity <- match.arg(granularity)
  periods <- brokers_period_starts(start, end, granularity)
  citas <- canonical$citas
  ventas <- canonical$ventas
  # Firmadas atribuibles al funnel del periodo: tienen gen_cita / fecha_primera_cita
  ventas_attr <- ventas |>
    dplyr::filter(
      .data$cuenta_conversion,
      !is.na(.data$fecha_primera_cita) | (nzchar(.data$gen_cita) & !is.na(.data$gen_cita))
    )
  ventas_proceso <- ventas |>
    dplyr::filter(.data$es_en_proceso)

  cols <- lapply(periods, function(ps) {
    pe <- brokers_period_end(ps, granularity)
    c_period <- citas |>
      dplyr::filter(!is.na(.data$fecha_cita), .data$fecha_cita >= ps, .data$fecha_cita <= pe)
    realizadas_nuevas <- sum(c_period$primera_cita & c_period$realizada, na.rm = TRUE)
    v_close <- ventas_attr |>
      dplyr::filter(!is.na(.data$fecha_firma), .data$fecha_firma >= ps, .data$fecha_firma <= pe)
    v_proc <- ventas_proceso |>
      dplyr::filter(!is.na(.data$fecha_firma), .data$fecha_firma >= ps, .data$fecha_firma <= pe)
    ciclos <- v_close$ciclo_venta[is.finite(v_close$ciclo_venta)]
    list(
      cita_realizada = realizadas_nuevas,
      procesos = nrow(v_proc),
      cierres = nrow(v_close),
      unidades = nrow(v_close),
      facturacion = sum(v_close$precio, na.rm = TRUE),
      ciclo = if (length(ciclos)) mean(ciclos) else NA_real_
    )
  })

  period_labels <- vapply(periods, brokers_period_label, character(1), granularity = granularity)
  tot_real <- sum(vapply(cols, `[[`, numeric(1), "cita_realizada"))
  tot_proc <- sum(vapply(cols, `[[`, numeric(1), "procesos"))
  tot_close <- sum(vapply(cols, `[[`, numeric(1), "cierres"))
  tot_fact <- sum(vapply(cols, `[[`, numeric(1), "facturacion"))
  all_ciclos <- unlist(lapply(seq_along(cols), function(i) {
    ps <- periods[[i]]
    pe <- brokers_period_end(ps, granularity)
    ventas_attr$ciclo_venta[
      !is.na(ventas_attr$fecha_firma) &
        ventas_attr$fecha_firma >= ps &
        ventas_attr$fecha_firma <= pe &
        is.finite(ventas_attr$ciclo_venta)
    ]
  }))
  tot_ciclo <- if (length(all_ciclos)) mean(all_ciclos) else NA_real_

  tibble::tibble(
    metrica = c(
      "Cita Realizada",
      "Procesos de venta",
      "Cierres",
      "Conversión cita realizada / venta",
      "Unidades Vendidas",
      "Facturación",
      "Ciclo promedio"
    ),
    Total = c(
      as.character(tot_real),
      as.character(tot_proc),
      as.character(tot_close),
      brokers_fmt_pct(tot_close, tot_real),
      as.character(tot_close),
      brokers_fmt_money(tot_fact),
      if (is.na(tot_ciclo)) "—" else as.character(round(tot_ciclo))
    )
  ) |>
    dplyr::bind_cols(
      as.data.frame(
        stats::setNames(
          lapply(seq_along(cols), function(i) {
            c(
              as.character(cols[[i]]$cita_realizada),
              as.character(cols[[i]]$procesos),
              as.character(cols[[i]]$cierres),
              brokers_fmt_pct(cols[[i]]$cierres, cols[[i]]$cita_realizada),
              as.character(cols[[i]]$unidades),
              brokers_fmt_money(cols[[i]]$facturacion),
              if (is.na(cols[[i]]$ciclo)) "—" else as.character(round(cols[[i]]$ciclo))
            )
          }),
          period_labels
        ),
        check.names = FALSE
      )
    )
}

# Filtro legado para ventas del origen broker dentro del dataset general Orígenes.
brokers_sales <- function(data) {
  if (is.null(data) || !nrow(data)) {
    return(origenes_empty_sales())
  }
  data |>
    dplyr::filter(origenes_normalize_key(.data$origen) == "broker")
}
