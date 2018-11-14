
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

##################################
#criar a spatial weights objet a partir da SpatialPolygonsDataframe distritos
#primeiro a objeto da classe nb e em seguida a listw

library(spdep)

###Estrutura de Vizinhan¸cas - para Concelhos
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

