# Publicar v0.1.1

Nueve pasos, en este orden. Cada uno dice primero **dónde** se hace.

Solo usas dos sitios: **RStudio** (con el proyecto abierto) y **el navegador**.
Dentro de RStudio usas dos pestañas, que están una al lado de la otra abajo a
la izquierda: **Console** (fondo blanco, el prompt es `>`) y **Terminal**
(fondo negro, el prompt acaba en `$`).

Con `CausalMultiOmics.Rproj` abierto, las dos pestañas ya están en la carpeta
correcta. No tienes que hacer `cd` en ningún momento.

---

# Paso 1 · Abrir el proyecto

**Dónde:** explorador de Windows.

Doble clic en:

```
CausalMultiOmics.Rproj  (en la carpeta del paquete)
```

**Dónde:** pestaña **Console** de RStudio. Escribe:

```r
getwd()
```

Tiene que responder `la ruta de tu paquete`.
Si responde otra cosa, no sigas: no has abierto el proyecto.

---

# Paso 2 · Comprobar que el paquete está sano

**Dónde:** pestaña **Console**.

Si es la primera vez, instala las herramientas:

```r
install.packages(c("devtools", "pkgdown", "usethis", "remotes"))
```

Ahora los cuatro comandos, **de uno en uno**, esperando a que acabe cada uno:

```r
devtools::document()
```
Regenera la documentación. Tocará muchos ficheros de `man/`. Es lo esperado.

```r
devtools::test()
```
Tiene que acabar en `[ FAIL 0 ]`. Tarda cerca de un minuto.

```r
devtools::check()
```
El importante. Tarda unos tres minutos. **Tiene que acabar en
`0 errors ✔ | 0 warnings ✔`.** Un `note` es aceptable.

```r
devtools::build_readme()
```
Regenera `README.md`.

### Si algo falla aquí, para y no sigas

- `check()` con errores: arréglalos antes de publicar nada.
- La sesión se cierra sola al cargar `lavaan`: es un binario roto, no tu
  paquete. `install.packages("lavaan")` y repite el paso.

---

# Paso 3 · Hacer público el repositorio

**Dónde:** navegador.

Ahora mismo el repositorio es **privado** (el candado junto al nombre). Eso
no bloquea solo la web: bloquea el objetivo entero. Con el repositorio
privado, quien no sea colaborador tuyo **no puede** ejecutar
`remotes::install_github("pedro-ortizpalma/CausalMultiOmics")`, no ve la
página de *Releases*, no puede descargar el `.tar.gz`, y las insignias del
README le salen rotas. GitHub Pages, además, solo funciona en repositorios
privados con un plan de pago.

## 3a · Antes de nada, comprobar el historial

**Dónde:** pestaña **Terminal**.

Hacer público un repositorio publica **todos los commits pasados**, no solo
los ficheros de ahora. Tu `.gitignore` excluye correctamente los datos de
NHANES (147 MB) y la carpeta `output/`, pero conviene confirmar que nunca
llegaron a subirse antes de que ese `.gitignore` existiera.

```bash
git count-objects -vH
```
Mira la línea `size-pack`. Si es de pocos MB, en el historial no hay datos
grandes. Si pasa de 100 MB, algo voluminoso entró en algún momento.

```bash
git log --all --diff-filter=A --name-only --pretty=format: -- tests/manual/test_data | sort -u
```
Lista todo lo que alguna vez se añadió bajo `test_data`. Lo correcto es que
solo aparezca `SOURCE.md`. Si aparece un `.csv` o un `.Rdata`, **detente y
escríbeme**: hay que limpiarlo del historial antes de publicar, y eso se hace
con `git filter-repo`, no borrando el fichero.

## 3b · Cambiar la visibilidad

**Dónde:** navegador.

**Settings** → **General** → baja del todo hasta **Danger Zone** →
**Change repository visibility** → **Change to public** → confirma
escribiendo el nombre del repositorio.

Lo que cambia al hacerlo: cualquiera puede leer el código, clonarlo,
descargar la release y abrir issues. Nadie puede modificar nada sin que tú
aceptes un pull request. Los Actions pasan a ser gratis e ilimitados, en vez
de consumir tu cuota de minutos.

## 3c · Volver a mirar Pages

**Settings** → **Pages**. Ya no debería salir el aviso de *Upgrade*. Si en
**Source** pone `None`, perfecto: lo configuras en el paso 8, cuando ya
exista la rama `gh-pages`.

---

# Paso 4 · Dejar de versionar la carpeta `docs/`

**Dónde:** pestaña **Terminal** de RStudio (la de fondo negro).

> Solo si en el paso 3 ponía `None` o `gh-pages`.

Copia estas dos líneas:

```bash
echo "docs/" >> .gitignore
git rm -r --cached docs
```

Qué hace: la carpeta `docs/` sigue en tu disco, pero deja de subirse a GitHub.
A partir de ahora la genera sola el servidor. Si no haces esto, subes 4 MB de
HTML duplicado y la web se desincroniza sola.

---

# Paso 5 · Subir los cambios

**Dónde:** pestaña **Terminal**.

Primero mira qué vas a subir:

```bash
git status
```

Luego las tres líneas, en orden:

```bash
git add -A
```
```bash
git commit -m "v0.1.1: recuento de variables eliminadas, accesores tidy, plot(), CI y sitio"
```
```bash
git push
```

**Dónde:** navegador. Entra en la pestaña **Actions** del repositorio.
Espera dos minutos: tienen que aparecer dos trabajos, **R-CMD-check** y
**pkgdown**, y ponerse en verde.

**Si R-CMD-check se pone rojo, detente aquí.** No tiene sentido publicar una
versión que no compila. Mándame el log y lo vemos.

---

# Paso 6 · Poner la etiqueta de versión

**Dónde:** pestaña **Terminal**.

> Solo si el paso 5 acabó en verde.

```bash
git tag -a v0.1.1 -m "CausalMultiOmics 0.1.1"
```
```bash
git push origin v0.1.1
```

---

# Paso 7 · Crear la release con el fichero descargable

**Dónde:** pestaña **Console**.

```r
devtools::build()
```

Esto crea el fichero que la gente va a descargar. Lo escribe **una carpeta más
arriba** del paquete, aquí:

```
la carpeta del paquete_0.1.1.tar.gz
```

Déjalo localizado en el explorador, que ahora lo vas a arrastrar.

**Dónde:** navegador. En la portada del repositorio, columna derecha,
**Releases** → **Create a new release**. Rellena:

1. **Choose a tag** → despliega y elige `v0.1.1` (ya existe, del paso 6).
2. **Release title** → escribe: `CausalMultiOmics 0.1.1`
3. **Describe this release** → abre `dev/RELEASE.md`, copia toda la sección
   **«6. Texto para el cuerpo de la release»** y pégala en el recuadro.
4. **Attach binaries** → arrastra ahí el `CausalMultiOmics_0.1.1.tar.gz`.
5. Pulsa el botón verde **Publish release**.

---

# Paso 8 · Encender la web

**Dónde:** navegador.

**Settings** → **Pages** → en **Source** elige **Deploy from a branch** →
en **Branch** elige `gh-pages` y carpeta `/ (root)` → **Save**.

Si `gh-pages` no aparece en la lista, es que el trabajo de pkgdown aún no ha
terminado. Vuelve a **Actions**, espera a que esté en verde, y repite.

Tres minutos después funciona esta dirección:

```
https://pedro-ortizpalma.github.io/CausalMultiOmics/
```

---

# Paso 9 · Rellenar el recuadro *About*

**Dónde:** navegador, portada del repositorio.

Pulsa el **engranaje** ⚙ que hay a la derecha, junto a la palabra *About*.
Se abre un panel con tres campos.

**Campo «Description»** — copia y pega esto entero:

```
Causal analysis of multi-block omics data in R. Audits and preprocesses each block from a recipe you can replay on a second cohort, runs nine evidence generators, and merges them into one scored causal graph where every edge carries its identification strategy, its assumptions, and what would settle it.
```

**Campo «Website»** — marca la casilla *Use your GitHub Pages website*.
Si no aparece, pega esto a mano:

```
https://pedro-ortizpalma.github.io/CausalMultiOmics/
```

**Campo «Topics»** — escribe cada una y pulsa Intro antes de la siguiente.
Son dieciséis:

```
r
r-package
causal-inference
multi-omics
omics
bioinformatics
biostatistics
epidemiology
causal-discovery
data-integration
reproducible-research
mediation-analysis
sensitivity-analysis
directed-acyclic-graph
observational-study
cardiovascular
```

Pulsa **Save changes**.

---

# Paso 10 · Comprobar que funciona de verdad

**Dónde:** pestaña **Console**. Antes, menú **Session → Restart R**, para que
la sesión esté limpia.

```r
remotes::install_github("pedro-ortizpalma/CausalMultiOmics@v0.1.1")
```
```r
library(CausalMultiOmics)
packageVersion("CausalMultiOmics")
```
Tiene que responder `0.1.1`.

```r
cmo_setup()
```

Si eso funciona, ya está publicado. Mejor aún si lo pruebas en otro ordenador,
porque entonces estás comprobando lo mismo que verá quien te lo descargue.

---

# Resumen de una línea por paso

| | Dónde | Qué |
|---|---|---|
| 1 | Explorador | Abrir el `.Rproj` |
| 2 | Console | `document()`, `test()`, `check()`, `build_readme()` |
| 3 | Terminal + navegador | Comprobar el historial y **hacer público** el repositorio |
| 4 | Terminal | Quitar `docs/` del control de versiones |
| 5 | Terminal | `git add`, `commit`, `push` → esperar verde en Actions |
| 6 | Terminal | `git tag v0.1.1` y `git push origin v0.1.1` |
| 7 | Console + navegador | `devtools::build()` y crear la release |
| 8 | Navegador | Settings → Pages → `gh-pages` |
| 9 | Navegador | Engranaje de *About*: descripción, web, 16 etiquetas |
| 10 | Console | Instalar desde GitHub y comprobar |

---

# Si algo sale mal

| Qué ves | Qué pasa |
|---|---|
| `git: command not found` en la Terminal | Git no está instalado o no está en el PATH. Instálalo desde git-scm.com y reinicia RStudio. |
| `git push` pide usuario y contraseña | GitHub ya no acepta contraseña. Necesitas un token: en **[Console]** `usethis::create_github_token()` y luego `gitcreds::gitcreds_set()`. |
| El trabajo *pkgdown* falla en «Build site» | Falta un tema de ayuda en `_pkgdown.yml`. El log dice cuál; añádelo a la sección que le toque. |
| La web da 404 | O la rama `gh-pages` todavía no existe, o en Settings → Pages sigue puesto `main`. |
| La insignia del README sigue gris | Aún no ha corrido el workflow. Se pone en color al terminar el primero. |
| `devtools::build()` no encuentra el `.tar.gz` | Está una carpeta por encima: en `la carpeta padre`, no dentro del paquete. |
