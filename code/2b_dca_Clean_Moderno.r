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

#Estadistica descriptiva
#personalizando agregate
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
# Operaciona previas a la gráfica descriptiva
descriptiva<-data.frame(tratamiento=descriptiva$tratamiento,descriptiva$nitrogeno)

source(file.path(".","code","funciones.R"))


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

#########################################################################################
#########################################################################################
#####################################
# 2. MODELACIÓN ESTADÍSTICA         #
#####################################
modelo<-formula(nitrogeno~tratamiento)

###############################
# 2.2 ANALISIS MODERNO        #
###############################

######################################################
# 2.1.1. Las varianzas homogeneas o hereogéneas      #
# RECORDAR:
#  Esto solo se hace si es necesario, es decir, si por teoría no se puede deducir 
#  que las varianzas sean homogéneas o heterogéneas

library(MCMCglmm)
# Modelo con varianzas heterogeneas
m<-MCMCglmm(modelo, data = datos, rcov=~idh(tratamiento):units, nitt=1e6,verbose=FALSE)

## Cálculo de los predichos obtenidos Bayes - Bayesian Linear Prediction
# Predicciones de todas las iteraciones MCMC
tmp<- m[["X"]] %*% t(m[["Sol"]])
grupo <- datos[["tratamiento"]]
indices <- split(seq_len(nrow(tmp)), grupo)

MCMC_trt <- lapply(indices, function(i) colMeans(tmp[i, , drop = FALSE]))

#Resumen de distribucion *a posteriori*
resumenMCMC<-lapply(MCMC_trt,function(x) c(mean(x),HPDinterval(as.mcmc(x),prob=0.90)))
resumenMCMC<-do.call(rbind,resumenMCMC)
colnames(resumenMCMC)<-c("Est Bayes", "lower.HPD", "upper.HPD")

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
analisis_post_modelo <- merge(descriptiva,complementos, by.x = "tratamiento",
 by.y = "tratamiento", all.x = TRUE)

analisis_post_modelo[["E Bayes"]] <- analisis_post_modelo[["Est.Bayes"]] 

graficos_personalizado(
  data_g = analisis_post_modelo,formula_g=tratamiento~`E Bayes`+Mediana+lower.HPD+upper.HPD,
  datos=datos,formula_m=nitrogeno~tratamiento,
  typeplot="lineas-violines",
  ordenar="E Bayes",
  prefixjpg="moderno_heterogeneo", 
  xlab=expression("Cepas de " * italic("Rhizobium") * " sp."), 
  ylab="Nitrógeno (mg)",
  add = function(g) {
    with(g, {
      text(x=bp, y=lower.HPD, labels=pworst, col = "#000000", cex=0.6,offset=0.2,pos=1)
      text(x=bp, y=upper.HPD, labels=pbest, col = "#000000", cex=0.6,offset=0.2,pos=3)
      #abline(h=25,lty=2,lwd=2,col = "#8B0000")
      #text(x=bp, y=25, labels=pref, col = "#8B0000", cex=0.6,adj=0,offset=0.2,pos=3)
      #text(x=bp, y=`E Bayes`, labels=pcheck, col = "#E65100", cex=0.6,adj=0,offset=0.4,pos=2)
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

###########################################################
# 4. Guardando resultados en archivos de salida           #
###########################################################
write_xlsx(list(modelo=analisis_post_modelo), 
path = file.path(".","salidas","excel","modelo_moderno.xlsx"))

saveRDS(list(modelo=m, modelo=analisis_post_modelo), 
        file = file.path(".","salidas","modelo_moderno.rds"),compress = "xz")

