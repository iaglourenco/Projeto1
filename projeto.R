n0<-read.csv(file="digitos/0_001.BMP.inv.pgm",header = FALSE, sep = " ")

n0<-n0[-1,]
n0<-n0[-1,]
n0<-n0[-1,]

n0<-n0[,-18]

vec<-as.vector(t(n0))
length(vec)
vec<-vec[-4097]
vec<-as.numeric(vec)

matrix<-matrix(vec,nrow = 64,ncol = 64,byrow = TRUE)

image(matrix,useRaster = TRUE,axes=FALSE)

