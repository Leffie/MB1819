
library(rgdal)
library(maptools)

# set working directory
# setwd("..")
# setwd("Users/David/Desktop/ISEL/MB/Ficheiros_TrabalhoMB")
#Ler ficheiro Shapefile concelhos:

ogrInfo(".", "Portugal_Municipios") #arguments: Where the data is stored, file name
concelhos <- readOGR(".", "Portugal_Municipios")
plot(concelhos, axes=TRUE, border="black")
head(concelhos@data)
dim(concelhos@data) #3223 freguesias
concelhos_data<-concelhos@data
concelhos_data

#a
##################################
#criar a spatial weights objet a partir da SpatialPolygonsDataframe distritos
#primeiro a objeto da classe nb e em seguida a listw

library(spdep)

###Estrutura de Vizinhanças - para Concelhos
concelhos_nb <- poly2nb(concelhos, row.names = NULL)
class(concelhos_nb)
concelhos_nb
card(concelhos_nb)
summary.nb(concelhos_nb)
concelhos_listw<-nb2listw(concelhos_nb)

###,style="W",zero.policy=T)
class(concelhos_listw)
concelhos_listw
concelhos_listw$weights
################################################################
#ler ficheiro de dados.csv - BaseDados16
dados2016<-read.csv2("BaseDados16.csv",header=TRUE)
is.data.frame(dados2016)
names(dados2016)
dim(dados2016)
summary(dados2016)
# ###################################################################
# ### Mapas de cores Escolaridade
library(maptools)
library(sp)
c.quentes <- colorRampPalette(c("green", "red"))
summary(dados2016$Escolaridade)
brks <- c(4,5,6,7,8,9,10,11)
library(classInt)
PrId_CI <- classIntervals(dados2016$Escolaridade, style = "fixed", fixedBreaks = brks)
PrId_CIc <- findColours(PrId_CI, c.quentes(6))
pdf("Escolaridade.pdf")
plot(concelhos, col=PrId_CIc)
title(main="Escolaridade 2016")
legend("bottomright", fill=attr(PrId_CIc, "palette"),legend=names(attr(PrId_CIc, "table")), bty="n")
dev.off()

# Mapa de cores Indice Rural 

summary(dados2016$IndRural)
brks <- c(0,0.1,0.2,0.3,0.5,0.6,0.7,0.8,0.9)
brks2 <- c(0,0.2,0.4,0.6)
library(classInt)
PrId_CI <- classIntervals(dados2016$IndRural, style = "fixed", fixedBreaks = brks)
PrId_CIc <- findColours(PrId_CI, c.quentes(6))
pdf("IndRural.pdf")
plot(concelhos, col=PrId_CIc)
title(main="Indice Rural 2016")
legend("bottomright", fill=attr(PrId_CIc, "palette"),
       legend=names(attr(PrId_CIc, "table")), bty="n")
dev.off()

#b

####Dependência Espacial###################
#Moran I-Concelhos
moran.test(dados2016$Escolaridade,concelhos_listw,alternative="two.sided")
moran.test(dados2016$IndRural,concelhos_listw,alternative="two.sided")

moran.test(dados2016$Escolaridade,concelhos_listw,
           randomisation=FALSE,alternative="two.sided")

moran.test(dados2016$IndRural,concelhos_listw,
           randomisation=FALSE,alternative="two.sided")
#Grafico de Moran
moran.plot(dados2016$Rendim,concelhos_listw,
           xlab="Rendimento", ylab="")
moran.plot(dados2016$Escolaridade,concelhos_listw,
           xlab="Escolaridade", ylab="")
moran.plot(dados2016$IndRural,concelhos_listw,
           xlab="IndRural", ylab="")
######Testes Normalidade Escolaridae###############
xb <- mean(dados2016$Escolaridade)
sx <- sd(dados2016$Escolaridade)
t1 <- ks.test(dados2016$Escolaridade, "pnorm", xb, sx)
t1
t2 <- shapiro.test(dados2016$Escolaridade)
t2

######Testes Normalidade IndRural###############
xb <- mean(dados2016$IndRural)
sx <- sd(dados2016$IndRural)
t1 <- ks.test(dados2016$IndRural, "pnorm", xb, sx)
t1
t2 <- shapiro.test(dados2016$IndRural)
t2
##Correlaçãao entre variáveis
corr <- cor(dados2016 [,2:14])
round(corr, 2)
####Modelo glm-Classico##

Escolaridade<-glm(Escolaridade ~ IndRural + Rendimento,
                     family=gaussian,data=dados2016)

summary(Escolaridade)
moran.test(Escolaridade$residuals,concelhos_listw,alternative="two.sided")

library("CARBayes")
#Modelo glm Bayesiano-MCMC##################
modelo.glm<-S.glm(formula=Escolaridade ~IndRural+ Rendimento,
                  family="gaussian", data=dados2016, burnin=20000, n.sample= 100000,thin=10)
print(modelo.glm)
moran.test(modelo.glm$residuals$response,concelhos_listw,alternative="two.sided")

###An´alise Espacial hier´arquica###########
W.concelhos<-nb2mat(concelhos_nb,style="B",zero.policy=T)
#Modelo com covari´aveis e efeitos espaciais estruturados utilizando modelo de Leroux
modeloLeroux.1<-S.CARleroux(formula=Escolaridade ~IndRural+ Rendimento,
                            family="gaussian", data=dados2016,W=W.concelhos,
                            burnin=20000, n.sample=100000,thin=10)
print(modeloLeroux.1)
modeloLeroux.1$modelfit
#Gr´afico Efeitos Aleatorios Leroux modelo 1###
names(modeloLeroux.1$samples)
summary(modeloLeroux.1$samples$phi)
head(modeloLeroux.1$fitted.values)
c.quentes <- colorRampPalette(c("yellow1", "red"))

phi<-colMeans(modeloLeroux.1$samples$phi)
summary(phi)
length(phi)
brks <- c(-0.1,-0.01,-0.005,0,0.005,0.01,0.1)
library(classInt)
phi_CI <- classIntervals(phi, style = "fixed", fixedBreaks = brks)

phi_CIc <- findColours(phi_CI, c.quentes(6))
pdf("EfeitosaleatoriosLeroux_1.pdf")
plot(concelhos, col=phi_CIc)
title(main="EfeitosaleatoriosLeroux_1")
legend("bottomright", fill=attr(phi_CIc, "palette"),
       legend=names(attr(phi_CIc, "table")), bty="n")
dev.off()
head(modeloLeroux.1$samples$beta)
#Ind Rural
mean(modeloLeroux.1$samples$beta[,2])
plot(modeloLeroux.1$samples$beta[,3])
#Perc. Pop ativa/Centros Saude
mean(modeloLeroux.1$samples$beta[,2])
plot(modeloLeroux.1$samples$beta[,3])
moran.test(modeloLeroux.1$residuals$response,concelhos_listw,
           alternative="two.sided")
moran.plot(modeloLeroux.1$residuals$response,concelhos_listw,
           xlab="Modelo Leroux1_resid", ylab="")


