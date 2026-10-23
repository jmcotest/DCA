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
datos <- data.frame(
      tratamiento = factor(rep(c("3D0K1", "3D0K5", "3D0K4", "3D0K7", "3D0K13", "Compuesto"), each = 5)),
      nitrogeno = c(19.4, 32.6, 27.0, 32.1, 33.0,
                    17.7, 24.8, 27.9, 25.2, 24.3,
                    17.0, 19.4, 9.1, 11.9, 15.8,
                    20.7, 21.0, 20.5, 18.8, 18.6,
                    14.3, 14.4, 11.8, 11.6, 14.2,
                    17.3, 19.4, 19.1, 16.9, 20.8)
    )

datos$tratamiento_c<-as.character(datos$tratamiento)
str(datos)

#library(readxl)
datosexcel<-readxl::read_excel(file.path(".","BD","dca.xlsx"))
datosexcel<-as.data.frame(datosexcel)
datos$tratamiento_c<-as.character(datos$tratamiento)
datosexcel$tratamiento<-as.factor(datosexcel$tratamiento)
str(datosexcel)
levels(datosexcel$tratamiento_f)
nlevels(datosexcel$tratamiento_f)

#####################################
# 1. ANÁLISIS GRÁFICO DE LOS DATOS  #
#####################################

#Estadistica descriptiva

#Funciones previas
descriptiva<-aggregate(nitrogeno~tratamiento,data=datos,mean)
descriptivasd<-aggregate(nitrogeno~tratamiento,data=datos,sd)

#personalizando aggregate forma 1
descriptiva<-aggregate(nitrogeno~tratamiento,data=datos,function(x){
  Media<-mean(x)
  sd <- sd(x)
  n <- length(x)
  # t(alpha/2,gl)*s/raiz(n)
  delta<-qt(0.025,n-1,lower.tail=FALSE)*sd/sqrt(n)
  return(c(Media=Media,Ds=sd,N=n,Delta=delta,Inferior=Media-delta,Superior=Media+delta,
  Maximo=max(x),Minimo=min(x),Mediana=median(x)))
})
descriptiva

#personalizando agregate forma 2
resumen<-function(x){
  Media<-mean(x)
  sd <- sd(x)
  n <- length(x)
  # t(alpha/2,gl)*s/raiz(n)
  delta<-qt(0.025,n-1,lower.tail=FALSE)*sd/sqrt(n)
  return(c(Media=Media,Ds=sd,N=n,Inferior=Media-delta,Superior=Media+delta,
  Maximo=max(x),Minimo=min(x),Mediana=median(x)))
}
descriptiva<-aggregate(nitrogeno~tratamiento,data=datos,resumen)

descriptiva_tapply<-tapply(datos$nitrogeno,datos$tratamiento,resumen)
descriptiva2<-do.call(rbind,descriptiva_tapply)

##Formas de acceder a una lista
str(descriptiva)
str(descriptiva_tapply)
descriptiva_tapply$`3D0K1`
descriptiva_tapply[[1]]
names(descriptiva_tapply)
descriptiva_tapply[["3D0K1"]]
str(descriptiva2)

# Gráfico de cajas y bigotes para los datos
boxplot(nitrogeno~tratamiento,data=datos,col="blue",xlab="Tratamiento", ylab="Nitrógeno (mg)")
# Operaciona previas a la gráfica descriptiva
descriptiva<-data.frame(tratamiento=descriptiva$tratamiento,descriptiva$nitrogeno)
#descriptiva<-descriptiva[order(descriptiva$Media),]

# Ordenar los datos originales según el mismo orden
datos_originales <- datos[order(
  match(as.character(datos$tratamiento),as.character(descriptiva$tratamiento)
  )),]
datos_originales


# Personalización de gráficos
graficos_personalizado<-function(data_g,formula_g,datos, formula_m,prefixjpg =NULL,typeplot="barras",ordenar=NULL,
  colores=list(color_barras  = "#7BC96F",
    color_datos   = "#2E7D32",
    color_Mediana = "#FF8C42",
    color_Media   = "#E65100",
    color_error   = "#1B5E20"
  ),ylim=NULL,add=NULL,...){
  
  #######################################################################################
  # Función para crear gráficos personalizados
  # data_g: data.frame con las estadísticas descriptivas debe contener las columnas 
  #           de tratamiento, Media, Mediana, Inferior, Superior. Puede contener otras 
  #           columnas adicionales que se pueden usar para personalizar el gráfico. 
  #           P.e. Estimate o letras de comparacion múltiple o probabilidades.
  # formula_g: fórmula para extraer las columnas correspondientes de tratamiento,
  #            Media, Mediana, Inferior, Superior
  # datos: data.frame con los datos originales debe contener mínimo dos columnas una
  #        de ellas es la variable de respuesta y la otra es la variable de clasificación
  # formula_m: fórmula para extraer las variables de respuesta y clasificación
  # prefixjpg: prefijo para el nombre del archivo de salida (opcional)
  # typeplot: tipo de gráfico ("barras", "lineas", "lineas-violines", "boxplot")
  # ordenar: columna para ordenar los tratamientos (opcional)
  #          p.e. Media o Mediana o tratamiento 
  # colores: lista de colores para personalizar el gráfico 
  # add: función para agregar elementos adicionales al gráfico (opcional)
  # ...: argumentos adicionales para la función de gráficos
  #########################################################################################
  
  # Variables obtenidas de la fórmula
  v_resp   <- all.vars(formula_m)[[1]]
  v_clasif <- all.vars(formula_m)[[2]]

  # Extraer observaciones originales
  ydatos <- datos[[v_resp]]
  grupo  <- datos[[v_clasif]]

  # Ordenar tratamientos según el estadístico seleccionado
  if (!is.null(ordenar)) {
    data_g <- data_g[order(data_g[[ordenar]], decreasing = TRUE),, drop = FALSE]
  }

  # Reordenar observaciones originales
  grupo <- factor(datos[[v_clasif]],levels = as.character(data_g[[v_clasif]]))
  idx <- order(grupo)

  ydatos <- ydatos[idx]
  grupo <- grupo[idx]
    
  # Crear variables locales a partir de las columnas
  #list2env(as.list(data_graf), envir = environment())
  #if (!is.null(Estimate)) Media <- Estimate
  Media <- data_g[[all.vars(formula_g)[[2]]]]
  Mediana <- data_g[[all.vars(formula_g)[[3]]]]
  Inferior <- data_g[[all.vars(formula_g)[[4]]]]
  Superior <- data_g[[all.vars(formula_g)[[5]]]]
  tratamiento <- data_g[[all.vars(formula_g)[[1]]]]  

  list2env(colores, envir = environment())

  num_oro<-(1+sqrt(5))/2
  if (!is.null(prefixjpg)) {
    jpeg(file.path(".","salidas","graficos",paste0(typeplot,"_",prefixjpg,".jpg")),units="cm",
    height=15,width=num_oro*15,res=600,quality = 100)
  }
  args <- list(...)

  if ("ylim" %in% names(args)) ylim <- args$ylim else {
    u_mayor <- 5
    ylim <- u_mayor * c( floor(min(Inferior) * 0.9 / u_mayor), ceiling(max(Superior) * 1.1 / u_mayor))
  }

  if (typeplot=="barras"){
    bp<-barplot(Media-min(ylim),offset=min(ylim),
    ylim=ylim,names.arg = tratamiento,cex.axis = 0.7,
    col=adjustcolor(color_barras, alpha.f = 0.3),...)
    x<-bp
    test<-data.frame(x0=bp,y0=Inferior,x1=bp,y1=Superior)
  }
  if (typeplot=="lineas"||typeplot=="lineas-violines"){
    bp<-seq_along(Media)
    plot(x=bp,y=Media,axes=FALSE,
      ylim=ylim,xlim=c(min(bp)-0.5,max(bp)+0.5),...)
      
  # Dibujar violines
    if (typeplot == "lineas-violines") {
      for (i in bp) {
        y <- ydatos[grupo == levels(grupo)[i]]
        y <- y[is.finite(y)]
        # evitar errores si hay menos de 2 observaciones o 
        # si todas las observaciones son iguales
        if (length(y) < 2 || length(unique(y)) < 2) next
        d <- density(y)
        ancho <- 0.35 * d$y / max(d$y)
        polygon(x = c(i - ancho, rev(i + ancho)), y = c(d$x, rev(d$x)),
          col = adjustcolor(color_barras, alpha.f = 0.25),
          border = color_barras
        )
      }
    }
    axis(1,seq_along(Media),tratamiento,las=1,cex.axis=0.9,tick=FALSE)
    axis(2)
  }

  if (typeplot == "boxplot") {
    bp <- seq_along(Media)
    boxplot(
      split(ydatos, grupo, drop = TRUE),
      names = tratamiento,
      col = adjustcolor(color_barras, alpha.f = 0.3),
      ylim = ylim,
      cex.axis = 0.7,
      ...
    )
  }

  if (typeplot!="boxplot"){
    # Conversión de unidades del eje X a pulgadas
    escala_x <- par("pin")[1] / diff(par("usr")[1:2])
    # Longitud correspondiente al 15 % de la separación
    separacion <- median(diff(sort(bp)))
    length_error <- 0.05 * separacion * escala_x
    arrows(bp,Inferior,bp,Superior,col=color_error,
    length = length_error, lwd=1.5,code=3,angle=90)
    arrows(bp,Inferior,bp,Superior,col=color_error,
    length = length_error, lwd=1.5,code=3,angle=45)
  }

  points(x=bp,y=Mediana,pch=18,cex=0.9,col=color_Mediana)
  points(x=bp,y=Media,pch=16,cex=0.9,col=color_Media)
  points(x=jitter(as.numeric(grupo), amount = 0.05),y=ydatos,pch=24,cex=0.5,col=color_datos,bg=color_datos)

  # Leyenda
  legend("topright",legend = c("Observaciones", all.vars(formula_g)[[2]], "Mediana"),
    pch = c(24, 16, 18),pt.bg = c(color_datos, NA, NA),
    col = c(color_datos, color_Media, color_Mediana),
    pt.cex = c(0.8, 1.0, 1.2),bty = "n",cex = 0.85)
  
  #pasar todas la variables de data_g dentro de la función
  if (is.function(add)) add(c(data_g, list(bp = bp, grupo = grupo)))
  if (!is.null(prefixjpg)) dev.off()

}

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

#   ASÍ A CONOTINUACION SE PRESENTA EL CODIGO PARA
#   DETERMINAR HOMOGENEIDAD DE VARIANZAS 
#   POR RAZONES EMPIRICAS.

# RECORDAR: 
#   LAS HIPÓTESIS NULAS JAMÁS SE PUEDEN PROBAR, 
#   SOLO SE PUEDEN RECHAZAR.
####################################################

#Opción 1: Prueba de Bartlett
bartlett.test(modelo,data=datos)

#Opción 2: Prueba de Levene
car::leveneTest(modelo,data=datos)

######################################################
# 2.1.2. ANOVA varianza homogenea     #

  m <- lm(modelo, data = datos)
  summary(m)
  str(m)
  anova_m<-anova(m)
  anova_m
  str(anova_m)

######################################################
# 2.1.3. Cálculo de los predichos obtenidos por mínimos cuadrados - LSMEAN     #
# Deben usarse en el caso de desbalance por repeticiones
#Opcion 1: Usando la función predict
nuevos <- data.frame(tratamiento = levels(datos$tratamiento))
media<-predict(m, newdata = nuevos)
#Opcion 2: Usando la función emmeans
media <- emmeans::emmeans(m, ~ tratamiento)

#######################################################
# 2.1.4. Prueba de comparacion múltiple     #
#Opcion 1: Usando la función TukeyHSD
tukey <- TukeyHSD(aov(m), "tratamiento")
print(tukey)
plot(tukey)

library(multcomp)
library(multcompView)
letrasTukey<-letras <- multcomp::cld(media,adjust = "tukey", Letters = letters, 
  decreasing = TRUE, alpha = 0.05)
letras

#Opcion 2: Usando la función glht y Tukey
tukey <- glht(aov(m),linfct = mcp(tratamiento = "Tukey"))
summary(tukey)
plot(tukey)

#Opcion 3: Usando la función glht y Dunnett
# # Definir Compuesto como referencia
datos$tratamiento <- relevel(datos$tratamiento, ref = "Compuesto")
dunnett <- glht(aov(m),linfct = mcp(tratamiento = "Dunnett"),
                 alternative = "two.sided")
summary(dunnett)
confint(dunnett)
plot(dunnett)
#El comando de abajo es para obtener las letras de comparacion múltiple
# no es correcto para dunnet . Lea la nota del resultado de la funcion.
letras <- multcomp::cld(media,adjust = "dunnett", Letters = letters, 
 decreasing = TRUE, alpha = 0.05)
letras

#####################################
# 3. COMPLEMENTAR Y AJUSTAR GRÁFICO #
#####################################
####################################################################################
# Conversión del resultado a un data.frame
letras<-letrasTukey
tabla_letras <- as.data.frame(letras)
# Eliminación de espacios adicionales en la columna de letras
tabla_letras$.group <- trimws(tabla_letras$.group)
# Visualización de la tabla final
tabla_letras

descriptiva_post_modelo <- merge(descriptiva,tabla_letras, by.x = "tratamiento",
 by.y = "tratamiento", all.x = TRUE)

graficos_personalizado(
  data_g = descriptiva_post_modelo,formula_g=tratamiento~emmean+Mediana+lower.CL+upper.CL,
  datos=datos,formula_m=nitrogeno~tratamiento,
  ordenar="emmean", typeplot="lineas",
  prefixjpg="clasico_homogeneo",
  xlab=expression("Cepas de " * italic("Rhizobium") * " sp."), 
  ylab="Nitrógeno (mg)", 
  add = function(g) {
    with(g,{
      text(x=bp, y=Superior, labels=.group, col = "#1B5E20", cex=0.9, offset=0.5,pos=3)
    })
  }
)

###########################################################
# 4. Guardando resultados en archivos de salida           #
###########################################################
write_xlsx(list(anova=anova_m, descriptiva_post_modelo=descriptiva_post_modelo), 
path = file.path(".","salidas","excel","descriptiva_post_modelo.xlsx"))

#########################################################################################
#########################################################################################
###############################
# 2.2 ANALISIS MODERNO        #
###############################

######################################################
# 2.1.1. Las varianzas homogeneas o hereogéneas      #
# RECORDAR:
#  Esto solo se hace si es necesario, es decir, si por teoría no se puede deducir 
#  que las varianzas sean homogéneas o heterogéneas

library(MCMCglmm)
# Modelo para varianzas homogéneas
m0<-MCMCglmm(modelo, data = datos, nitt=1e6,verbose=FALSE)

# Modelo con varianzas heterogeneas
m1<-MCMCglmm(modelo, data = datos, rcov=~idh(tratamiento):units, nitt=1e6,verbose=FALSE)

# Comparación de modelos
  summary(m0)
  summary(m1)

  m0$DIC
  m1$DIC
  m0$DIC
  m0$DIC-m1$DIC

# Interpretación:
#  - Menor DIC: mejor ajuste considerando complejidad.
#  - $\Delta$DIC < 2: evidencia débil a favor de m1.
#  - $\Delta$DIC entre 3 y 7: evidencia moderada a favor de m1.
#  - $\Delta$DIC > 10: evidencia fuerte a favor de m1.

#Colocando el modelo final
m<-m1

## Cálculo de los predichos obtenidos Bayes - Bayesian Linear Prediction
#Opción 1. A mano
grupo <- datos[["tratamiento"]]
MCMC_trt<-list(m$Sol[,1],
  m$Sol[,1]+m$Sol[,2],
  m$Sol[,1]+m$Sol[,3],
  m$Sol[,1]+m$Sol[,4],
  m$Sol[,1]+m$Sol[,5],
  m$Sol[,1]+m$Sol[,6])
names(MCMC_trt)<-levels(grupo)

#Opción 2. Usando matrices
# Predicciones de todas las iteraciones MCMC
tmp<- m[["X"]] %*% t(m[["Sol"]])
grupo <- datos[["tratamiento"]]
indices <- split(seq_len(nrow(tmp)), grupo)

resultado <- lapply(indices, function(i) colMeans(tmp[i, , drop = FALSE]))

#Resumen de distribucion *a posteriori*
resumenMCMC<-lapply(MCMC_trt,function(x) c(mean(x),HPDinterval(x,prob=0.90)))
resumenMCMC<-do.call(rbind,resumenMCMC)
colnames(resumenMCMC)<-c("Est.Bayes", "lower.HPD", "upper.HPD")

# Analisis de la distribución *a posteriori* para cada tratamiento: 
# Probabilidad de ser el mejor y el peor tratamiento
# Esto es mejor que las pruebas de comparación multiples en cuanto a una toma 
# de decisiones más clara.
# El autor quien realizó la prouesta es "Julian Besag, David Higdon, 
# Bayesian Analysis of Agricultural Field Experiments, 
# Journal of the Royal Statistical Society Series B: Statistical Methodology, 
# Volume 61, Issue 4, November 1999, Pages 691–746, 
# https://doi.org/10.1111/1467-9868.00201"

MatrixMCMC<-as.matrix(do.call(cbind,MCMC_trt))
rank_MatrixMCMC<-apply(MatrixMCMC,1,rank)
pbest<-rowSums(rank_MatrixMCMC==nlevels(grupo))/ncol(rank_MatrixMCMC)
pworst<-rowSums(rank_MatrixMCMC==1)/ncol(rank_MatrixMCMC)

pbest_c<-format(round(pbest,4), nsmall=4) 
pworst_c<-format(round(pworst,4), nsmall=4)

#Nuevo probabilidad de superar un valor de referencia
p_ref<-sapply(MCMC_trt,function(x) sum(x > 25) / length(x))
p_ref_c<-sapply(p_ref,function(x) format(round(x,4), nsmall=4))

#Nuevo probabilidad de superar el testigo Compuesto
p_check<-sapply(MCMC_trt,function(x) sum(x > MCMC_trt[["Compuesto"]]) / length(x))
p_check_c<-sapply(p_check,function(x) format(round(x,4), nsmall=4))
p_check_c["Compuesto"]<-""
#Juntando complementos
complementos<-data.frame(tratamiento=rownames(resumenMCMC), resumenMCMC, pbest=pbest_c, pworst=pworst_c, pref=p_ref_c, pcheck=p_check_c)

#####################################
# 3. COMPLEMENTAR Y AJUSTAR GRÁFICO #
#####################################
descriptiva_post_modelo <- merge(descriptiva,complementos, by.x = "tratamiento",
 by.y = "tratamiento", all.x = TRUE)

graficos_personalizado(
  data_g = descriptiva_post_modelo,formula_g=tratamiento~Est.Bayes+Mediana+lower.HPD+upper.HPD,
  datos=datos,formula_m=nitrogeno~tratamiento,
  typeplot="lineas-violines",
  ordenar="Est.Bayes",
  prefixjpg="moderno_heterogeneo", 
  xlab=expression("Cepas de " * italic("Rhizobium") * " sp."), 
  ylab="Nitrógeno (mg)",
  add = function(g) {
    with(g, {
      text(x=bp, y=lower.HPD, labels=pbest, col = "#000000", cex=0.6,offset=0.2,pos=1)
      text(x=bp, y=upper.HPD, labels=pworst, col = "#000000", cex=0.6,offset=0.2,pos=3)
      abline(h=25,lty=2,lwd=2,col = "#8B0000")
      text(x=bp, y=25, labels=pref, col = "#8B0000", cex=0.6,adj=0,offset=0.2,pos=3)
      text(x=bp, y=Est.Bayes, labels=pcheck, col = "#E65100", cex=0.6,adj=0,offset=0.4,pos=2)
    })
  }
)

# 
# Gráfica resumen más completo basado en el resumen de la distribución *a posteriori*
# Fig n. Contenido promedio de nitrógeno en plantas de trébol inoculadas con diferentes cepas de Rhizobium.
#        Las barras de error indican los límites del intervalo de mas alta densidad a posterior de 90%. En la
#        parte superior de límite se indica la probabilidad de que ese tratamiento sea el mejor mientras en el
#        límite inferior se indica la probabilidad de ser el peor. En rojo se indica la probabilidad de que ese
#        tratamiento supere la concentración de 25 mg. Al lado izquiero de la media se indica la probabilidad de
#        superar el testigo Compuesto. La línea punteada indica el valor de referencia de 25 mg.

