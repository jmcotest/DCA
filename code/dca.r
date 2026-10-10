rm(list=ls(all=TRUE))

#Generando estructuras de carpetas
dir.create(file.path(".","salidas","graficos"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","salidas","excel"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","salidas","pdf"),showWarnings = FALSE, recursive=TRUE)
dir.create(file.path(".","salidas","html"),showWarnings = FALSE, recursive=TRUE)
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

library(readxl)
datosexcel<-read_excel(file.path(".","BD","dca.xlsx"))
datosexcel<-as.data.frame(datosexcel)
datosexcel$tratamiento_f<-as.factor(datosexcel$tratamiento)
str(datosexcel)
levels(datosexcel$tratamiento_f)
nlevels(datosexcel$tratamiento_f)

#Estadistica descriptiva

#Funciones previas
descriptiva<-aggregate(nitrogeno~tratamiento,data=datos,mean)
descriptivasd<-aggregate(nitrogeno~tratamiento,data=datos,sd)

#personalizando aggregate forma 1
descriptiva<-aggregate(nitrogeno~tratamiento,data=datos,function(x){
  media<-mean(x)
  sd <- sd(x)
  n <- length(x)
  # t(alpha/2,gl)*s/raiz(n)
  delta<-qt(0.025,n-1,lower.tail=FALSE)*sd/sqrt(n)
  return(c(Media=media,Ds=sd,N=n,Delta=delta,Inferior=media-delta,Superior=media+delta,
  Maximo=max(x),Minimo=min(x),Mediana=median(x)))
})
descriptiva

#personalizando agregate forma 2
resumen<-function(x){
  media<-mean(x)
  sd <- sd(x)
  n <- length(x)
  # t(alpha/2,gl)*s/raiz(n)
  delta<-qt(0.025,n-1,lower.tail=FALSE)*sd/sqrt(n)
  return(c(Media=media,Ds=sd,N=n,Delta=delta,Inferior=media-delta,Superior=media+delta,
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


boxplot(nitrogeno~tratamiento,data=datos)
barras_personalizado<-function(media,mediana,inferior,superior,color_barras,color_datos,
 color_media,color_mediana,names_trt,repeticiones,ydatos,jpg=FALSE){
  num_oro<-(1+sqrt(5))/2
  if (jpg) {
    jpeg(file.path(".","salidas","graficos","barras.jpg"),units="px",
    height=5*1200,width=num_oro*5*1200,res=1200,quality = 100)
  }

  bp<-barplot(media,xlab=expression("Cepas de " * italic("Rhizobium") * " sp."),
  ylab="Nitrógeno (mg)",ylim=c(0,40),names.arg = names_trt,cex.axis = 0.7,
  col=adjustcolor(color_barras, alpha.f = 0.3))
  arrows(bp,inferior,bp,superior,
  length = 0.05, lwd=1.5,code=3,angle=90)
  arrows(bp,inferior,bp,superior,
  length = 0.05, lwd=1.5,code=3,angle=45)
  points(x=rep(bp,each=5),y=ydatos,pch=24,cex=0.5,col=color_datos)
  points(x=bp,y=mediana,pch=18,cex=0.9,col=color_mediana)
  points(x=bp,y=media,pch=16,cex=0.9,col=color_media)

  # Leyenda
  legend("topright",legend = c("Observaciones", "Media", "Mediana"),
    pch = c(24, 16, 18),pt.bg = c(color_datos, NA, NA),
    col = c(color_datos, color_media, color_mediana),
    pt.cex = c(0.8, 1.0, 1.2),bty = "n",cex = 0.85)
  #Fig 1. Gráfico descriptivo para el contenido de nitróneno en 6 tratamientos de cepas de Rhizoboum.
  #.      La barras indican la media del tratamiento la cual también está resaltada en negro. La mediana
  #       se resalta en color naranja. Las observaciones originales están en color azul y las barras 
  #.      de error indican los límites del intervalo de confianza de 95%.
  if (jpg) dev.off()
}
barras_personalizado(
  media    = descriptiva$nitrogeno[, "Media"],
  mediana  = descriptiva$nitrogeno[, "Mediana"],
  inferior = descriptiva$nitrogeno[, "Inferior"],
  superior = descriptiva$nitrogeno[, "Superior"],
  names_trt = descriptiva$tratamiento,
  color_barras  = "#397F9F",
  color_datos   = "#6485BC",
  color_mediana = "#D55E00",
  color_media   = "#111827",
  repeticiones=5,ydatos = datos$nitrogeno[order(datos$tratamiento)],jpg=TRUE)


#barplot(descriptiva2[,"Media"])
