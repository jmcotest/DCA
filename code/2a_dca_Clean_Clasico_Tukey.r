rm(list=ls(all=TRUE))
library(writexl)
#Generando estructuras de carpetas
# Eliminar la carpeta salidas y todo su contenido
if (dir.exists(file.path(".", "salidas"))) {
  unlink(file.path(".", "salidas"), recursive = TRUE)
}

# Crear nuevamente las carpetas
for (carpeta in c("graficos", "excel", "pdf", "html")) {
  dir.create(
    file.path(".", "salidas", carpeta),
    recursive = TRUE,
    showWarnings = FALSE
  )
}
dir.create(file.path(".","script"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","rmd"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","qmd"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","BD"),showWarnings = FALSE, recursive=TRUE)

#Datos 
datos<-readxl::read_excel(file.path(".","BD","dca.xlsx"))
datos<-as.data.frame(datos)
datos$tratamiento_c<-as.character(datos$tratamiento)
datos$tratamiento<-as.factor(datos$tratamiento)
str(datos)
levels(datos$tratamiento)
nlevels(datos$tratamiento)

#####################################
# 1. ANÁLISIS GRÁFICO DE LOS DATOS  #
#####################################
source(file.path(".","code","funciones.R"))
#Estadistica descriptiva
descriptiva<-aggregate(nitrogeno~tratamiento,data=datos,resumen)
# Operaciona previas a la gráfica descriptiva
descriptiva<-data.frame(tratamiento=descriptiva$tratamiento,descriptiva$nitrogeno)

# Uso de la funcion graficos
graficos_personalizado(
  data_g = descriptiva,formula_g=tratamiento~Media+Mediana+Inferior+Superior,
  datos=datos,formula_m=nitrogeno~tratamiento,
  ordenar="Media", typeplot="lineas-violines", prefixjpg="inicial",
  xlab=expression("Cepas de " * italic("Rhizobium") * " sp."), 
  ylab="Nitrógeno (mg)")
  #Para el de lineas
  #Fig 1. Gráfico descriptivo para el contenido de nitróneno en 6 tratamientos de cepas de Rhizoboum.
  #.      La barras indican la media del tratamiento la cual también está resaltada en negro. La Mediana
  #       se resalta en color naranja. Las observaciones originales están en color azul y las barras 
  #.      de error indican los límites del intervalo de confianza de 95%.

#####################################
# 2. MODELACIÓN ESTADÍSTICA         #
#####################################
modelo<-formula(nitrogeno~tratamiento)

###############################
# 2.1 ANALISIS CLÁSICO        #
###############################

######################################################
# 2.1.1. Las varianzas homogeneas o hereogéneas      #

# NOTA IMPORTANTE:
#   ESTO SÓLO SE HACE SÍ POR TEORÍA TENEMOS DUDAS 
#   DE LA HOMOGENEIDAD/HETEROGENEIDAD DE LAS VARIANZAS.

#   ASÍ A CONTINUACIÓN SE PRESENTA EL CÓDIGO PARA
#   DETERMINAR HOMOGENEIDAD DE VARIANZAS 
#   POR RAZONES EMPÍRICAS.

# RECORDAR: 
#   LAS HIPÓTESIS NULAS JAMÁS SE PUEDEN PROBAR, 
#   SOLO SE PUEDEN RECHAZAR.
####################################################

lt<-car::leveneTest(modelo,data=datos)
lt

######################################################
# 2.1.2. ANOVA varianza homogenea     #
  m <- lm(modelo, data = datos)
  summary(m)

  anova_m<-anova(m)
  anova_m

# Prueba de Shapiro-Wilk
sh<-shapiro.test(residuals(m))
sh
######################################################
# 2.1.3. Cálculo de los predichos obtenidos por mínimos cuadrados - LSMEAN     #
# Deben usarse en el caso de desbalance por repeticiones
media <- emmeans::emmeans(m, ~ tratamiento)

#######################################################
# 2.1.4. Prueba de comparacion múltiple     #
#Opcion 1: Usando la función TukeyHSD
tukey <- TukeyHSD(aov(m), "tratamiento")
tukey

library(multcomp)
library(multcompView)
letras <- multcomp::cld(media,adjust = "tukey", Letters = letters, 
  decreasing = TRUE, alpha = 0.05)
letras

#####################################
# 3. COMPLEMENTAR Y AJUSTAR GRÁFICO #
#####################################
####################################################################################
# Conversión del resultado a un data.frame
tabla_letras <- as.data.frame(letras)
# Eliminación de espacios adicionales en la columna de letras
tabla_letras$.group <- trimws(tabla_letras$.group)
# Visualización de la tabla final
tabla_letras

analisis_post_modelo <- merge(descriptiva,tabla_letras, by.x = "tratamiento",
 by.y = "tratamiento", all.x = TRUE)

analisis_post_modelo[["LS Mean"]] <- analisis_post_modelo[["emmean"]] 

graficos_personalizado(
  data_g = analisis_post_modelo,formula_g=tratamiento~`LS Mean`+Mediana+lower.CL+upper.CL,
  datos=datos,formula_m=nitrogeno~tratamiento,
  ordenar="LS Mean", typeplot="lineas-violines",
  prefixjpg="clasico_homogeneo",
  xlab=expression("Cepas de " * italic("Rhizobium") * " sp."), 
  ylab="Nitrógeno (mg)", 
  add = function(g) {
    with(g,{
      text(x=bp, y=upper.CL, labels=.group, col = "#1B5E20", cex=0.9, offset=0.5,pos=3L)
    })
  }
)

###########################################################
# 4. Guardando resultados en archivos de salida           #
###########################################################
write_xlsx(list(anova=anova_m, modelo=analisis_post_modelo), 
path = file.path(".","salidas","excel","modelo_clasico.xlsx"))

saveRDS(list(modelo=m,anova=anova_m,shapiro=sh,levene=lt, modelo=analisis_post_modelo), 
        file = file.path(".","salidas","modelo_clasico.rds"),compress = "xz")
