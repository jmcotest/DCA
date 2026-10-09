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

##Formas de acceder a una lista
str(descriptiva)
str(descriptiva_tapply)
descriptiva_tapply$`3D0K1`
descriptiva_tapply[[1]]
names(descriptiva_tapply)
descriptiva_tapply[["3D0K1"]]
