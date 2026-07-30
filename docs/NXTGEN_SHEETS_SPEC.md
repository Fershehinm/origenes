# NxtGen Sheets → qué quieren replicar en el dash

Análisis de los exports en `data/nxtgen/` cruzados con Ambassadors API y Bubble RSCG.  
**No se liga el dash a estos CSV**; sirven para entender la intención del Sheets.

## Las 4 pestañas

| Tab | Filas | Rol |
|---|---|---|
| **Registros de embajadores** | 159 | Padrón + fecha de registro operativa + actividad lookup |
| **Citas** | 374 | Extract de meetings Bubble NxtGen (≈2026), con nombre conciliado |
| **Ventas** | 11 | Extract de sales Bubble NxtGen Firmado (2026) |
| **Lista de Embajadores** | 80 | Ranking derivado de citas realizadas (no es fuente primaria) |

---

## 1) Registros de embajadores

### Campos

| Campo Sheets | Qué es | Origen |
|---|---|---|
| **Fecha** | Fecha de registro del embajador (operativa) | **Manual / proceso Mau** — no es 1:1 con ningún campo API |
| **Nombre** | Nombre como lo capturaron | Manual / captura |
| **Conciliacion Nombre** | Spelling canónico | Manual (alias) |
| **Gen** | Casi siempre vacío (14/159) | Irrelevante |
| **Gen Registro** | `YYYY - MM` de **Fecha** | **Fórmula** (100% = mes de Fecha) |
| **Fecha primera cita realizada** | Min cita Realizada del embajador | **Lookup desde Citas** (67/67) |
| **Fecha de última cita realizada** | Max cita Realizada | **Lookup desde Citas** (67/67) |
| **Fecha de última cita agendada/realizada** | Última cita (cualquier estatus útil) | Lookup Citas |
| **Días desde la última cita realizadas** | `hoy − última realizada` | **Fórmula** (72/72 ±2d) |
| **Gen primera cita realizada** | Mes de la 1ª realizada | **Fórmula** sobre fecha 1ª |

### Implicación dash

**Decisión (2026-07-28):** Registrados usa proxy
`ambassadors.contact.Created.Date` + `Type_OS` del origen. Ver
[`REGISTRADOS_CREATED_DATE.md`](REGISTRADOS_CREATED_DATE.md).

- La Fecha del Sheets es **manual**; lo pre-2026 se fechó en bloque (p. ej.
  diciembre), por eso no empata con el dash.
- Más adelante se corregirá la fecha en la app; mientras, la verdad es el hub.
- El Sheets sigue siendo referencia de *intención* de métricas, no fuente
  ligada al dash.

---

## 2) Citas

### Es casi un dump de Bubble

Match Sheets ↔ Bubble NxtGen:

- prospecto + día exacto: **87%**
- prospecto ±1 día: **100%**
- Status igual: **97%**
- Primera cita igual: **322/323**
- Embajador igual: **317/323**

Volumen 2026-01-01 → hoy: Sheets ~354 vs Bubble ~366 (mismo orden de magnitud).  
Bubble histórico total NxtGen ≈ 3883; el Sheets solo trae el recorte reciente (2026+).

### Campos

| Campo Sheets | Campo Bubble | Notas |
|---|---|---|
| Nombre del Prospecto | `meeting.Lead.full.name` | |
| Fecha de Inicio | `meeting.Start.date` | Eje de **Gen Cita** |
| Estatus | `meeting.Status_OS` | Realizada / Cancelada / Agendada |
| Embajador | `meeting.Ambassador` | |
| Primera Cita | `meeting.First.Meeting` | si/no |
| Fecha de Creación | `meeting.Created.Date` | ~66% día exacto (TZ / edits) |
| Embajador Registrado | Conciliación vs Registros | 91% = Embajador; el resto alias |
| **Gen Cita** | fórmula `ym(Fecha de Inicio)` | **100%** |

### Implicación dash

Toda la **actividad de citas** del Resumen debe salir de:

```
Bubble meetings WHERE Origin_OS = NxtGen
```

con:

- mes de actividad = mes de `Start.date` (Gen Cita)
- realizadas = `Status_OS = Realizada`
- primera = `First.Meeting = true` ∧ realizada
- embajador = `Ambassador` (luego conciliar nombre)

---

## 3) Ventas

### Campos

| Campo Sheets | Campo Bubble | Notas |
|---|---|---|
| Fecha de firma | `sale` fecha cierre / firma | Eje de **Gen Venta** |
| Nombre / Conciliacion Lead | cliente | Match por nombre flojo (2/11) — IDs mejor |
| Origen | `Origin_OS` | siempre NxtGen en el export |
| Proyecto / ID / M2 / Precio | sold property + sale | |
| Vendedor | seller | |
| Status | estado | export solo **Firmado** |
| Recompra | repurchase flag | |
| **Gen Venta** | `ym(Fecha de firma)` | 11/11 |
| Primera Cita / Gen Cita | lookup a citas del lead | |
| Ciclo Venta | `firma − 1ª cita` (días) | 10/10 |

Bubble NxtGen firmadas 2026 ≈ 9; Sheets tiene 11 → mismo universo, pequeño desfase de criterio/fecha.

### Implicación dash

```
Bubble sales WHERE Origin_OS = NxtGen AND estado Firmado
```

- mes = mes de fecha de firma (Gen Venta)
- unidades / facturación = filas firmadas / suma precio

---

## 4) Lista de Embajadores

No es una fuente de verdad de registro. Es un **ranking**:

- Embajador Registrado
- # de citas (realizadas)
- Días desde última realizada

Se puede recalcular desde Citas (72/80 nombres matchean; conteos exactos 28/72, ±2 → 54/72).  
Sirve para conciliación de nombres y “quién está activo”, no para Registrados.

---

## Qué miden las 3 tablas del Resumen (intención)

### A) Embajadores / Conversión Embajadores

Cohorte por **mes de registro** (`Gen Registro`):

| Métrica | Intención |
|---|---|
| Registrados | # embajadores con registro en ese mes |
| Cita agendada | de esa cohorte, cuántos ya tienen ≥1 cita |
| Primera cita | de esa cohorte, cuántos ya tienen 1ª realizada |
| Conversión 1ª / reg | primera / registrados |
| Activos (últimos N días) | de esa cohorte (o global — validar), cita reciente |
| Conversión activo / 1ª | activos / primera |

**Fuente deseada:** padrón con fecha de registro + citas Bubble atribuidas al embajador.

### B) Resultados / Actividad

Corte por **mes de la cita/venta** (no del registro):

| Métrica | Intención | Fuente |
|---|---|---|
| Primera cita | 1ª realizadas con `Start.date` en el mes | Bubble meetings |
| Citas agendadas | todas las meetings del mes | Bubble |
| Citas nuevas realizadas | 1ª ∧ Realizada en el mes | Bubble |
| Total realizadas | Realizada en el mes | Bubble |
| Unidades / Facturación | Firmado con firma en el mes | Bubble sales |

### C) Conversión Citas

Por mes de actividad: realizadas → cierres (atribución lead/cita → venta).  
Fuente: Bubble meetings + sales NxtGen.

---

## Mapa dash (sin CSV)

| Cosa del Sheets | Ligar el dash a |
|---|---|
| Gen Cita / volumen citas / 1ª / realizadas | **Bubble** `meeting` · `Origin_OS=NxtGen` · `Start.date`, `Status_OS`, `First.Meeting`, `Ambassador` |
| Gen Venta / unidades / $ | **Bubble** `sale` · `Origin_OS=NxtGen` · Firmado · fecha firma |
| Embajador Registrado / anti-duplicados | Lista canónica de nombres (hoy CSV; ideal: padrón hub + reglas) |
| Gen Registro / Registrados | **Proxy aceptado:** `contact.Created.Date` + `Type_OS` (ver `REGISTRADOS_CREATED_DATE.md`). Sheets Fecha es manual; no se liga. |
| Activos N días | última cita Bubble del embajador dentro de N días |
| Lista ranking | derivado; no hace falta tab aparte |

### Ambassadors hub — para qué sí sirve

- Clasificar quién es embajador NxtGen (`Type_OS`)
- Enriquecer team / método / join meeting↔lead (`RSCG.Meeting.ID`, `RSCG.Lead.ID`)
- **No** es hoy la fuente de Gen Registro ni del extract de citas/ventas del Sheets

---

## Conclusión operativa

1. **Citas y ventas del Sheets = Bubble filtrado a NxtGen (ventana reciente).** Ahí el dash debe calcar Bubble.
2. **Registrados en Sheets** = mes de una Fecha **manual** (histórico pre-2026
   fechado en bloque). El dash **no** replica eso: usa `Created.Date` y documenta
   el delta (`REGISTRADOS_CREATED_DATE.md`).
3. **Conciliación de nombres** es parte del modelo (Conciliacion Nombre / Embajador Registrado / Lista).
4. Citas/ventas del Resumen ya tienen dueño claro en Bubble; Registrados queda
   en hub hasta haber fecha de registro editable en la app.
