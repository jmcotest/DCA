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
