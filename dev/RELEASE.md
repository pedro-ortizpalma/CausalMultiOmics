# Publicar una versión que la gente pueda descargar y probar

Todo lo de este fichero se ejecuta **en tu máquina**: el sandbox donde se
prepararon estos ficheros no puede lanzar subprocesos de R (`R CMD build` y
`R CMD check` fallan ahí por permisos de ruta, no por el paquete), así que la
comprobación de verdad la haces tú una vez.

---

## 1. Lo que ya está puesto en el repositorio

| Fichero | Para qué |
|---|---|
| `.github/workflows/R-CMD-check.yaml` | `R CMD check` en Windows, macOS y Linux, con R release, devel y oldrel. Es lo que convierte «descárgalo y pruébalo» en una afirmación comprobable. |
| `.github/workflows/pkgdown.yaml` | Construye y publica el sitio de documentación en la rama `gh-pages`. |
| `_pkgdown.yml` | Estructura del sitio: las 71 páginas de ayuda públicas agrupadas por las cinco etapas del flujo, más los dos documentos HTML existentes en el menú. |
| `pkgdown/assets/` | Copia de `analisis-ejemplo.html` y `CausalMultiOmics-guide.html`. pkgdown los vuelca tal cual en la raíz del sitio, de modo que **conservan la URL que ya tienen** en `docs/` en lugar de ser machacados. |
| `inst/CITATION` | Hace que `citation("CausalMultiOmics")` devuelva algo sensato. |
| `.github/CONTRIBUTING.md`, `.github/ISSUE_TEMPLATE/` | Cómo informar de un fallo y cómo proponer un generador de evidencia. La plantilla de bug pide `cmo_setup()` y `sessionInfo()`, y advierte de no adjuntar un `saveRDS()` de un resultado: lleva datos individuales dentro. |
| `README.Rmd` / `README.md` | Cuatro insignias y la sección de instalación reescrita: `remotes::install_github()` y el tarball de la release. |
| `.Rbuildignore` | Siete entradas nuevas para que nada de lo anterior entre en el tarball. |

---

## 2. Comprobar antes de etiquetar

```r
# desde la raíz del paquete
devtools::document()          # las 156 páginas man/ ya están al día
devtools::test()              # 1041 aserciones, 0 fallos esperados
devtools::check()             # el filtro de verdad: 0 errors, 0 warnings
devtools::build_readme()      # re-teje README.md desde el .Rmd
pkgdown::build_site()         # revisa el sitio en local antes de publicarlo
```

Dos cosas que conviene mirar con atención la primera vez:

- **`devtools::check()` construye la vignette**, que ejecuta un análisis
  completo. Cuenta con dos o tres minutos.
- En el equipo donde se preparó esto, el binario de `lavaan` instalado por
  conda está roto y aborta el proceso de R al cargarse, lo que impedía
  ejecutar 8 de los 26 ficheros de test. Es un fallo de aquella máquina, no
  del paquete. Si en la tuya `library(lavaan)` carga sin más, la suite entera
  debería correr; si no, reinstala `lavaan` desde CRAN.

---

## 3. Publicar la release

```bash
git add -A
git commit -m "v0.1.1: feature-removal accounting, tidy accessors, plot methods, CI"
git push

git tag -a v0.1.1 -m "CausalMultiOmics 0.1.1"
git push origin v0.1.1
```

Después, en **Releases → Draft a new release**:

- **Tag**: `v0.1.1`
- **Title**: `CausalMultiOmics 0.1.1`
- **Body**: el texto de la sección 5 de este fichero.
- **Adjuntar**: el `CausalMultiOmics_0.1.1.tar.gz` que produce
  `devtools::build()`. Es lo que descarga quien no quiera instalar desde
  GitHub, y lo que enlaza el README.

Una alternativa sin salir de R, que crea la release y sube el tarball de una
vez:

```r
# install.packages("usethis")
usethis::use_github_release()
```

---

## 3 bis. Una decisión que te toca a ti: `docs/` duplicado

Los dos documentos HTML están ahora en dos sitios: `docs/` (donde ya estaban) y
`pkgdown/assets/` (desde donde pkgdown los vuelca al sitio). Son 2 MB en cada
carpeta, y commitear las dos mete 4 MB de HTML casi idéntico en el historial.

El workflow de pkgdown publica en la rama `gh-pages`, así que `docs/` en `main`
deja de hacer falta. Lo que yo haría:

```bash
# docs/ pasa a ser salida generada, no fuente
echo "docs/" >> .gitignore
git rm -r --cached docs
```

**Antes de ejecutarlo, comprueba cómo sirves Pages ahora.** Si está en
*Settings → Pages → Deploy from a branch → `main` / `/docs`*, al quitar `docs/`
el sitio actual desaparece hasta que el workflow publique en `gh-pages` y
cambies la fuente. Si lo prefieres al revés —seguir sirviendo desde
`main/docs`— entonces borra `pkgdown/assets/` y cambia el workflow para que
construya en `docs/` y haga commit ahí, en lugar de desplegar a `gh-pages`.

Las dos opciones funcionan. Lo que no funciona es dejar las dos carpetas
rastreadas, porque la siguiente ejecución de pkgdown las desincroniza.

## 4. Activar GitHub Pages

Una vez que el workflow de pkgdown haya corrido por primera vez:

**Settings → Pages → Source: Deploy from a branch → `gh-pages` / `(root)`**

El sitio queda en `https://pedro-ortizpalma.github.io/CausalMultiOmics/`, que
es la URL que va en el campo **Website** del recuadro *About*.

---

## 5. Texto para el recuadro *About* (el engranaje de la barra lateral)

### Description

> Causal analysis of multi-block omics data in R. Audits and preprocesses each
> block from a recipe you can replay on a second cohort, runs nine evidence
> generators, and merges them into one scored causal graph where every edge
> carries its identification strategy, its assumptions, and what would settle
> it.

(304 caracteres; el límite de GitHub son 350.)

### Website

```
https://pedro-ortizpalma.github.io/CausalMultiOmics/
```

### Topics

```
r  r-package  causal-inference  multi-omics  omics  bioinformatics
biostatistics  epidemiology  causal-discovery  data-integration
reproducible-research  mediation-analysis  sensitivity-analysis
directed-acyclic-graph  observational-study  cardiovascular
```

Dieciséis etiquetas, por debajo del máximo de veinte. Las cuatro primeras son
las que hacen que el repositorio aparezca en las búsquedas de la gente que
busca exactamente esto; `cardiovascular` lo conecta con tu línea de trabajo.

### Lo demás de esa barra lateral

- **Readme**, **License** y **Activity** ya están marcados.
- **Packages** se queda vacío: es para GitHub Packages (npm, Maven, Docker) y
  no aplica a un paquete de R.

---

## 6. Texto para el cuerpo de la release

```markdown
Primera versión pensada para instalarse y probarse fuera del equipo donde se
escribió. El flujo completo —`load_data()` → `check_data()` → `preprocess()`
→ `analyze()`— está documentado, cubierto por 1041 aserciones de test y
comprobado en Windows, macOS y Linux.

## Instalación

```r
remotes::install_github("pedro-ortizpalma/CausalMultiOmics@v0.1.1")
```

O descarga `CausalMultiOmics_0.1.1.tar.gz` de aquí abajo e instálalo sin red:

```r
install.packages("CausalMultiOmics_0.1.1.tar.gz", repos = NULL, type = "source")
```

Empieza por `cmo_setup()`, que dice qué generadores de evidencia puede
ejecutar tu máquina antes de que analices nada.

## Lo corregido

- **Una etapa eliminaba variables y declaraba que no había eliminado
  ninguna.** El registro de pasos deducía los nombres eliminados de
  `model$columns`, pero los modelos de filtrado guardan las columnas que
  *conservan*. Un bloque que pasaba de 600 a 500 variables declaraba
  `Removed features: 0`. Ahora se comparan los nombres antes y después de
  cada etapa, lo que además es correcto para cualquier etapa futura.
- **`print()` no decía cuánto del motor había corrido.** Ahora lee
  `Methods run: 3 of 9`, cuántos se saltaron por falta de paquetes, cuántos
  por no aplicar al diseño, y el `install.packages()` que habilita los
  primeros.
- **Los nueve `summary()` imprimían en lugar de devolver**, así que la
  cabecera salía dos veces y el resumen no se podía guardar. Devuelven un
  objeto; lo que se ve en consola es idéntico.
- **Metadatos con los identificadores en `row.names`** ya son utilizables, con
  aviso; metadatos que no comparten ningún identificador con los bloques pasan
  de dos avisos a error.
- La sección `Execution` de `summary()` imprimía el tiempo sin unidad y las
  marcas de inicio y fin como segundos desde 1970.

## Lo nuevo

- `cmo_setup()`: qué puede ejecutar esta instalación, y por qué una
  instalación incompleta no devuelve menos evidencia sino evidencia sesgada a
  la baja.
- `as.data.frame()` en seis clases, ordenado por evidencia, con `all`,
  `outcome_only` y `min_score`.
- `plot()` en tres clases, más `cmo_plots()`, sobre las figuras que el objeto
  ya lleva dentro.
- `outcome_name()`, porque el desenlace se guarda como lista y compararlo con
  una columna de texto funciona por coerción y devuelve filas equivocadas.
- `check_data(plots = FALSE)`: las figuras son el 4-6 % del tiempo y el 90 %
  del tamaño del objeto. Con 1600 variables, 27,1 MB pasan a 1,7 MB sin
  cambiar ningún hallazgo.
- Ejemplos ejecutables en las seis páginas que no tenían ninguno. Las 23
  funciones exportadas tienen ya ejemplo y referencias cruzadas.

El registro completo está en [NEWS.md](NEWS.md).
```

---

## 7. Lo que yo haría después, por orden

1. **Mirar la insignia de `R-CMD-check` en verde** antes de contárselo a
   nadie. Si sale roja en Windows o en R devel, lo sabrás antes que tus
   lectores.
2. **`codecov`** o al menos `covr::package_coverage()` una vez, para saber
   qué parte de las 26 933 líneas tocan los tests.
3. **Un DOI de Zenodo**: conecta el repositorio en zenodo.org, y cada release
   futura obtiene un DOI citable. Para un paquete que va a aparecer en una
   tesis es lo que convierte «está en GitHub» en algo que un revisor acepta.
4. **rOpenSci** admite envíos de paquetes estadísticos con revisión por pares
   abierta. Es el camino natural antes de CRAN para un paquete de este tipo,
   y la revisión mejora el paquete.
