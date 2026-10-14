W<-list(a=1L:5L,b=rnorm(15L,mean=100L,sd=5L),c=c(4L,3L,12L,8L))
W[[2L]]
mean(W[[2L]])
W[["b"]]
W$b
resultados<-list()
for(i in 1L:3L) resultados[[i]]<-(mean(W[[i]])) #muy costoso por el for

resultados<-lapply(W,mean)

CV<-function(x){
  media<-mean(x)
  de<-sd(x)
  return(de/media*100L)
}

resultados<-lapply(W,CV)

H<-list(a=runif(5),b=rgamma(15,100,1),c=rpois(4,5))

W[[1L]]-H[[1L]]
resultado2<-list()
for(i in 1:3) resultado2[[i]]<-(W[[i]]-H[[i]])

#cuando son mas de dos listas las involucradas
mapply(function(x,y){
  return(x-y)
},x=W,y=H)

Map(function(x,y){
  return(x-y)
},x=W,y=H)

restandolistas<-function(x,y){
  return(x-y)}

Map(restandolistas,x=W,y=H)
