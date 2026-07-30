# Unión Embajadores ↔ Bubble (RSCG)

## Fuentes

| Sistema | Rol |
|---|---|
| **Bubble RSCG** (bundle vendedores) | Verdad comercial: citas, ventas, facturación, `Origin_OS` |
| **ambassadors.mx** | Hub de orígenes: registro de embajadores, equipos, método de prospección |

## Reglas de join

1. **Cita**  
   `ambassadors.meeting.RSCG.Meeting.ID` = `bubble.meeting.X_id`  
   Cobertura actual: **96.8%** (572 / 591)

2. **Lead / contacto**  
   `ambassadors.contact.RSCG.Lead.ID` = `bubble.meeting.Lead_contact_R`  
   (fallback: `bubble.sale.Client_contact_R`)  
   Cobertura actual: **96.0%** (505 / 526 con ID)

3. **Local hub**  
   `ambassadors.meeting.Client_R` = `ambassadors.contact.X_id`  
   Cobertura: **100%**

## Clasificación de embajadores

En el hub, `Origin_OS` suele venir vacío en embajadores. Se usa `Type_OS`:

- `Ambassador NxtGen` → NxtGen
- `Ambassador Broker` → Broker
- `Ambassador Other` → Embajador Otro

## Conciliación de nombres

Para evitar duplicados por spelling distinto en Bubble vs lista oficial:

| Origen | Fuente |
|---|---|
| **NxtGen** | `data/nxtgen/NxtGen Analytics - Lista de Embajadores.csv` |
| **Brokers** | `data/brokers/raw/...Lista de Brokers.csv` + `data/brokers/canonical/name_map.csv` (Conciliación Nombre / alias) |

El build aplica el mapa a `meetings.embajador` y `embajadores.nombre` (queda `embajador_raw` / `nombre_raw` con el original).

## Qué métrica sale de dónde

| Métrica | Fuente |
|---|---|
| Citas / status / vendedor / primera cita | Bubble (`meetings_clean`) |
| Ventas / facturación | Bubble (`sales_clean`) |
| Embajadores registrados | Hub: `Type_OS` + **`Created.Date`** como proxy de fecha de registro ([detalle / delta vs Sheets](REGISTRADOS_CREATED_DATE.md)) |
| Prospectos del hub | Hub (`contact` Prospecto + Origin_OS) |
| Método de prospección / team hub | Hub, vía join de cita |
| Nombre embajador en cita Bubble | `meeting.Ambassador` (muy completo en NxtGen/Broker) |

> **Registrados:** la Fecha del Sheets es manual (pre-2026 fechado en bloque a
> diciembre u otra fecha operativa). El dash acepta `Created.Date` y no depende
> del CSV. Corrección futura: editar/capturar fecha real en la app.

## Cómo regenerar

```sh
# 1) Explorar / refrescar hub
Rscript scripts/explore_ambassadors_api.R

# 2) Construir canónicos unidos
Rscript scripts/build_origenes_canonical.R
```

Salida: `data/canonical/origenes_joined.rds` y subcarpetas `nxtgen/` / `broker/`.
