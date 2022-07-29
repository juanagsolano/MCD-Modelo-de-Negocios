#PCA

datos <- read.csv('C:/Users/chccr/Dropbox/Maestría MCD/MCD Modelo de Negocios/pca_project/AumentadaPFLD.csv',header =T,sep=",")
datos = as.matrix(datos)

ACP <- prcomp(x = datos ,center = T, scale. = F)
summary(ACP)

lambda <- as.matrix(ACP$sdev)[1:2,]
lambda = lambda * lambda

PCs <- as.matrix(ACP$rotation)
PCs <- PCs[,1:2]
PCs

PC_cuadrado = as.matrix(PCs * PCs)

PCs <- data.frame(
  PC29=ACP[["x"]][,29],
  PC30=ACP[["x"]][,30]
  )

riesgos = (((datos %*% PC_cuadrado[,1]) * lambda[1]) + ((datos %*% PC_cuadrado[,2]) * lambda[2])) / sum(lambda)

plot(riesgos)
plot(PCs$PC29,PCs$PC30)

write.csv(riesgos,"C:/Users/chccr/Dropbox/Maestría MCD/MCD Modelo de Negocios/pca_project/riesgos.csv")

