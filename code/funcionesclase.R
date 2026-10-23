
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
