# Orígenes

Aplicación Shiny independiente para analizar los diferentes orígenes comerciales
de la división.

La primera versión incluye:

- Página principal con acceso a cada canal.
- Pestaña NxtGen con filtros, KPIs, evolución mensual, proyectos y operaciones.
- Pestaña Brokers con conversión embajadores, actividad y cierre (mensual/semanal).
- Estado vacío seguro: la app no inventa datos cuando la fuente aún no está conectada.
- Diseño responsivo para escritorio, tableta y móvil.

## Ejecutar localmente

Instala las dependencias declaradas en `DESCRIPTION` y ejecuta:

```r
shiny::runApp()
```

## Datos

La app busca el primer archivo disponible en este orden:

1. La ruta definida en `ORIGENES_DATA_PATH`.
2. `data/cache/latest.rds`.
3. `data/origenes.rds`.
4. `data/origenes.csv`.

El archivo puede ser un `data.frame` o un RDS que contenga `units`,
`units_enriched`, `sales`, `sales_clean` o `sales_raw`.

Columnas canónicas:

- `fecha`
- `origen`
- `proyecto`
- `vendedor`
- `cliente`
- `estatus`
- `precio`

También se reconocen nombres habituales de Bubble, como `Date.of.sale`,
`Origin_OS`, `Project_OS`, `Seller.full.name`, `Client.full.name` y
`Purchase.price`.

### Brokers

Solo CRM Bubble (`Origin_OS = Broker`). No usa CSV/Sheets de Academy.

Prioridad (`ORIGENES_BROKERS_DATA_SOURCE=crm`):

1. Cache del dash de vendedores (`ORIGENES_VENDEDORES_BUNDLE`)
2. Bubble live (`BUBBLE_BASE` + `BUBBLE_TOKEN`)

Embajadores desde el campo `Ambassador` de meetings. Activo = cita en
últimos N días (`ORIGENES_BROKERS_ACTIVE_DAYS`, default 30).

- Granularidad mensual o semanal
- Periodo por defecto: 1 dic 2025 → fin del mes actual
- Tablas: Conversión Embajadores, Actividad, Conversión Citas

## Publicar en shinyapps.io

El nombre por defecto es `origenes` en la cuenta `division2cbr`:

```sh
Rscript scripts/deploy_shinyapps.R
```

Para cambiar el nombre o la cuenta:

```sh
Rscript scripts/deploy_shinyapps.R otro-nombre otra-cuenta
```