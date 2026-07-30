# Registrados: decisión de fuente

**Decisión (2026-07-28):** el dash usa `ambassadors.contact.Created.Date` como
proxy de fecha de registro. No se liga a la Fecha / Gen Registro del Sheets.

## Por qué

- En el Sheets, **Fecha de registro es manual**.
- Histórico: lo anterior a 2026 se cargó en bloque con fecha de **diciembre**
  (u otra fecha operativa), no con la fecha real de alta en la app.
- Por eso Sheets y dash no empatan mes a mes (p. ej. Brokers dic Sheets 25 vs
  dash 8; ene Sheets 43 vs dash 54 por altas `Created.Date` del 20-ene).
- Más adelante se podrá corregir la fecha de registro **en la app** (campo real
  o edición manual). Hasta entonces, la verdad del dash es la data del hub.

## Cómo lo calcula Orígenes hoy

| Paso | Regla |
|---|---|
| Universo | `contact` con `Type_OS` = `Ambassador NxtGen` o `Ambassador Broker` |
| Fecha | `Created.Date` → `fecha_registro` |
| Mes (Gen Registro proxy) | `YYYY - MM` de `fecha_registro` |
| Métrica Registrados | `#` embajadores con `fecha_registro` en el periodo de la columna |

Código: `R/origenes_join.R` (`origenes_standardize_amb_contacts`) +
`R/origenes_resumen.R` (`origenes_resumen_tabla_embajadores`).

## Delta documentado vs Sheets (referencia)

No es un bug del dash: es Sheets manual ≠ `Created.Date`.

### Brokers (ventana dic-2025 → jul-2026)

| Mes | Sheets (Gen Registro) | Dash (`Created.Date`) | Δ |
|---|---:|---:|---:|
| 2025 - 12 | 25 | 8 | −17 |
| 2026 - 01 | 43 | 54 | +11 |
| 2026 - 02 | 35 | 34 | −1 |
| 2026 - 03 | 28 | 30 | +2 |
| 2026 - 04 | 54 | 54 | 0 |
| 2026 - 05 | 68 | 66 | −2 |
| 2026 - 06 | 27 | 28 | +1 |
| 2026 - 07 | 19* | 18 | −1 |
| **Total ventana** | **299*** | **292** | |

\*Jul Sheets online puede diferir del CSV local (export tenía 13 en jul).

Patrón típico: registros Sheets en dic-2025 aparecen en hub el **2026-01-20**
(carga masiva a `Ambassador Broker`).

Auditoría detallada: `data/audit/brokers_registrados_*.csv`.

### NxtGen (ene-2026)

| Fuente | Registrados ene-2026 |
|---|---:|
| Sheets Gen Registro | 27 |
| Dash `Created.Date` | 48 (47 keys únicas) |

Misma lógica: Fecha Sheets manual vs alta/tipado en hub.

## Pendiente (después)

1. Campo de fecha de registro real en Ambassadors (o edición en app).
2. Reconciliar histórico cuando exista ese campo.
3. Hasta entonces: **no** usar CSV/Sheets para Registrados en producción.
