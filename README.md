# DCA — Diseño completamente al azar en R

Material docente para el análisis de experimentos bajo un **diseño completamente al azar (DCA)**, con aplicaciones a las ciencias agrarias. Incluye exploración de datos, gráficos personalizados, análisis clásico y análisis bayesiano, así como la preparación de informes reproducibles en **Quarto** y **R Markdown**.

El ejemplo estudia el contenido de nitrógeno en plantas de trébol sometidas a seis tratamientos de inoculación con *Rhizobium*, con cinco repeticiones por tratamiento. La variable respuesta es `nitrogeno`, expresada en miligramos, y el factor de clasificación es `tratamiento`.

## Contenido

- Estadísticas descriptivas por tratamiento e intervalos de confianza.
- Gráficos de barras, líneas, violines y cajas, con observaciones individuales.
- Modelo lineal y análisis de varianza para comparar medias de tratamientos.
- Pruebas de Levene y Shapiro–Wilk en el script clásico independiente.
- Medias estimadas por el modelo mediante `emmeans`.
- Comparaciones múltiples de Tukey y representación mediante letras.
- Ejemplos adicionales de comparaciones mediante `multcomp` en el script ampliado.
- Modelos bayesianos mediante `MCMCglmm`, con ejemplos de varianzas residuales homogéneas y heterogéneas.
- Medias posteriores, intervalos de máxima densidad posterior y probabilidades de interés.
- Exportación de tablas a Excel y almacenamiento de objetos en archivos RDS.
- Organización de documentos `.qmd` y `.Rmd` para elaborar informes en HTML, PDF o Word.

## Organización del proyecto

| Ruta | Contenido o función |
|---|---|
| `BD/dca.xlsx` | Datos utilizados por los scripts de análisis independientes. |
| `code/0_dca_inicial.r` | Introducción al manejo de datos, resúmenes y gráficos en R. |
| `code/1_dca_mejorado.r` | Desarrollo ampliado con gráficos personalizados y ejemplos clásicos y bayesianos. |
| `code/2a_dca_Clean_Clasico_Tukey.r` | Análisis clásico organizado: Levene, modelo lineal, ANOVA, Shapiro–Wilk, medias estimadas y Tukey. |
| `code/2b_dca_Clean_Moderno.r` | Análisis bayesiano con varianzas residuales diferentes por tratamiento. |
| `code/funciones.R` | Función `graficos_personalizado()` compartida por los scripts independientes. |
| `code/ResumenManejoDeListas.R` | Ejemplos docentes de listas, `lapply()`, `mapply()` y `Map()`. |
| `qmd/` | Carpeta prevista para documentos Quarto (`.qmd`). |
| `rmd/` | Carpeta prevista para documentos R Markdown (`.Rmd`). |
| `salidas/graficos/` | Gráficos generados por los programas. |
| `salidas/excel/` | Tablas de resultados en formato Excel. |
| `salidas/pdf/` | Carpeta destinada a los informes PDF. |
| `salidas/html/` | Carpeta destinada a los informes HTML. |
| `salidas/` | También contiene los archivos RDS de los scripts independientes. |

Las carpetas `qmd/` y `rmd/` se destinan a los documentos que se incorporen al proyecto. Los nombres de informes utilizados más adelante son ejemplos. Los scripts crean las carpetas de salida; los documentos se generan al renderizar el archivo Quarto o R Markdown correspondiente.

## Requisitos

Se necesita **R**. Puede trabajarse desde Positron, RStudio u otro editor, o desde una terminal.

Instale los paquetes de análisis una sola vez:

```r
install.packages(c(
  "readxl", "writexl", "car", "emmeans",
  "multcomp", "multcompView", "MCMCglmm", "coda"
))
```

Para generar informes:

```r
install.packages(c("knitr", "rmarkdown"))
```

Para documentos Quarto, instale también [Quarto CLI](https://quarto.org/docs/get-started/). El paquete de R llamado `quarto` es opcional y no sustituye esa instalación. La generación de PDF requiere una distribución LaTeX; una alternativa es [TinyTeX](https://yihui.org/tinytex/):

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

## Primeros pasos

### 1. Descargar el repositorio

Desde una terminal:

```bash
git clone https://github.com/jmcotest/DCA.git
cd DCA
```

También puede utilizar **Code → Download ZIP** en GitHub y descomprimir el proyecto.

### 2. Trabajar desde la raíz del proyecto

El directorio de trabajo de R debe ser la carpeta `DCA`, que contiene `BD/` y `code/`. Las rutas de los programas son relativas a esa ubicación.

```r
getwd()
file.exists("BD/dca.xlsx")
file.exists("code/funciones.R")
```

Las dos últimas expresiones deben devolver `TRUE`.

### 3. Revisar los datos

Los scripts independientes leen `BD/dca.xlsx`. El archivo debe contener, como mínimo:

| Columna | Descripción |
|---|---|
| `tratamiento` | Identificador del tratamiento; se convierte en factor antes de ajustar el modelo. |
| `nitrogeno` | Valor numérico de la respuesta, en mg. |

Cada fila debe corresponder a una observación. Para adaptar el análisis, mantenga estos nombres o actualice las fórmulas y referencias a columnas en los programas. La unidad experimental y la independencia de las repeticiones deben corresponder al diseño realizado.

## Análisis clásico

Ejecute desde la raíz del proyecto:

```r
source("code/2a_dca_Clean_Clasico_Tukey.r")
```

El programa ajusta el modelo:

```r
nitrogeno ~ tratamiento
```

Examina la homogeneidad de varianzas mediante Levene, evalúa el efecto de los tratamientos mediante ANOVA y aplica Shapiro–Wilk a los residuos. Calcula las medias estimadas y realiza las comparaciones de Tukey con un nivel de significancia del 5 %.

Produce, entre otros archivos:

| Archivo | Contenido |
|---|---|
| `salidas/graficos/lineas-violines_inicial.jpg` | Exploración gráfica de los datos. |
| `salidas/graficos/lineas_clasico_homogeneo.jpg` | Medias estimadas, observaciones e identificación de grupos de Tukey. |
| `salidas/excel/modelo_clasico.xlsx` | Tabla ANOVA y resumen de resultados por tratamiento. |
| `salidas/modelo_clasico.rds` | Modelo ajustado, ANOVA, pruebas de Shapiro–Wilk y Levene, y tabla de resultados. |

No rechazar una hipótesis en una prueba de supuestos no demuestra que el supuesto se cumpla. Los resultados deben revisarse junto con los gráficos y el conocimiento del experimento. Compartir una letra de Tukey indica que no se detectó una diferencia al nivel elegido; no demuestra equivalencia.

## Análisis bayesiano

Como alternativa al análisis clásico, ejecute:

```r
source("code/2b_dca_Clean_Moderno.r")
```

El script independiente utiliza `MCMCglmm` y permite una varianza residual por tratamiento mediante:

```r
rcov = ~ idh(tratamiento):units
```

El programa utiliza `nitt = 1e6`. Resume las distribuciones posteriores de las medias por tratamiento mediante estimaciones puntuales e intervalos HPD del **90 %** y calcula:

| Indicador | Interpretación en este ejemplo |
|---|---|
| `pbest` | Probabilidad posterior de que el tratamiento tenga la mayor media de nitrógeno entre los tratamientos evaluados. |
| `pworst` | Probabilidad posterior de que tenga la menor media. |
| `pref` | Probabilidad posterior de que su media supere 25 mg. |
| `pcheck` | Probabilidad posterior de que su media supere la de Compuesto. |

Estas probabilidades se calculan sobre las **medias de los tratamientos**, no sobre una futura observación individual. Se expresan en escala de 0 a 1. El criterio de mayor media es el utilizado para definir «mejor» en este ejemplo y debe ajustarse al objetivo de cada experimento.

Las salidas incluyen:

- `salidas/graficos/lineas-violines_moderno_heterogeneo.jpg`.
- `salidas/excel/modelo_moderno.xlsx`.
- `salidas/modelo_moderno.rds`.

Antes de usar las simulaciones para una conclusión definitiva, revise las distribuciones previas, la mezcla, la autocorrelación y el tamaño efectivo de muestra. Para repetir una ejecución MCMC de forma controlada, establezca una semilla dentro del script antes de llamar a `MCMCglmm()`.

## Conservación de las salidas

**Los scripts `1_dca_mejorado.r`, `2a_dca_Clean_Clasico_Tukey.r` y `2b_dca_Clean_Moderno.r` eliminan y recrean la carpeta `salidas/` al iniciar.** Por ello, ejecutar el análisis bayesiano después del clásico borra las salidas anteriores, incluidos los informes que se hayan guardado allí.

Ejecute el análisis elegido, genere su informe y conserve los archivos que necesite fuera de `salidas/` antes de ejecutar otro script. Los programas también limpian los objetos de la sesión de R mediante `rm(list = ls(all = TRUE))`.

La carpeta `salidas/` está excluida del control de versiones mediante `.gitignore`: sus archivos se generan localmente y no se incluyen automáticamente al subir cambios a GitHub.

## Documentos Quarto

Guarde los documentos `.qmd` en `qmd/`. El flujo previsto consiste en ejecutar el análisis en R y después leer su RDS e incorporar el gráfico existente desde el documento.

Por ejemplo, si dispone de `qmd/informe_clasico.qmd`, puede generar el informe desde la raíz del proyecto:

```bash
quarto render qmd/informe_clasico.qmd --to html --output-dir ../salidas/html
quarto render qmd/informe_clasico.qmd --to pdf --output-dir ../salidas/pdf
```

En estos comandos, `--output-dir` se interpreta con relación al archivo de entrada; por eso se utiliza `../salidas/` cuando el documento se encuentra en `qmd/`. El formato solicitado debe estar habilitado en el encabezado YAML del documento.

En cada documento, configure la raíz de ejecución o adapte las rutas para encontrar `salidas/`, que se encuentra fuera de `qmd/`.

## Documentos R Markdown

Guarde los documentos `.Rmd` en `rmd/`. Por ejemplo, para generar un informe HTML desde `rmd/informe_clasico.Rmd`, ejecute en R desde la raíz del proyecto:

```r
dir.create("salidas/html", recursive = TRUE, showWarnings = FALSE)

rmarkdown::render(
  input = "rmd/informe_clasico.Rmd",
  output_format = "html_document",
  output_dir = file.path(getwd(), "salidas", "html"),
  knit_root_dir = getwd()
)
```

Con `knit_root_dir = getwd()`, el código del documento puede leer rutas relativas a la raíz del proyecto:

```r
resultados <- readRDS("salidas/modelo_clasico.rds")
```

Sustituya el nombre de archivo del ejemplo por el documento que haya incorporado a `rmd/`.

## Reutilización de los objetos RDS

Los archivos RDS permiten elaborar informes sin repetir el ajuste del modelo:

```r
resultados <- readRDS("salidas/modelo_clasico.rds")
names(resultados)
str(resultados, max.level = 1)

resultados$anova
resultados$levene
resultados$shapiro
```

En la versión actual de los scripts independientes, el nombre `modelo` aparece dos veces dentro de la lista guardada: una vez para el ajuste y otra para la tabla de resultados. `resultados$modelo` devuelve el primer elemento con ese nombre. Para recuperar ambos:

```r
posiciones <- which(names(resultados) == "modelo")
ajuste <- resultados[[posiciones[1]]]
tabla_resultados <- resultados[[posiciones[2]]]
```

Al adaptar la exportación, puede asignar nombres distintos, por ejemplo `ajuste` y `tabla_resultados`. Si modifica esos nombres, actualice también los documentos que lean el RDS.

## Autor

**José Miguel Cotes Torres**  
Material de apoyo para la enseñanza de estadística y diseño experimental en ciencias agrarias.

Repositorio: [jmcotest/DCA](https://github.com/jmcotest/DCA).
