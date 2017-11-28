#' ---
#' title: "Les Champs de Sirene"
#' author: "François Sémécurbe"
#' date: "27 novembre 2017"
#' ---


library(sf)
setwd("Z:/champs_siren/")

##########################
### load data ############
##########################

tabac=read.csv("tabac.csv",sep=",",as.is=T)
tabac[1:10,]
##########################
### load shp #############
##########################

arrondissement=st_read("arrondissement.shp")
arrondissement=st_transform(arrondissement, 2154)
arrondissement=arrondissement[order(arrondissement$C_ARINSEE),]
arrondissement[1:5,]
plot(arrondissement)

#calcul des coordonnées des centroides
coord_arrondissement=(st_coordinates(st_centroid(arrondissement)))
arrondissement$x=coord_arrondissement[,1]
arrondissement$y=coord_arrondissement[,2]

########################################
##### calcul de distance ###############
########################################
#utilisation simple
tabac_distance=tabac
distance=((outer(tabac_distance$x,arrondissement$x,"-"))^2+(outer(tabac_distance$y,arrondissement$y,"-"))^2)^0.5
distance=data.frame(distance)
names(distance)=paste0("arr",75001:75020)
tabac_distance=cbind(tabac_distance,distance)

#########################################
### du point à un objet géographique ####
#########################################
geometry=st_as_sfc(paste0("POINT (",tabac$x," ",tabac$y,")"),crs=2154)
tabac_sf=st_sf(tabac,geometry=geometry)
tabac_sf[1:10,]
plot(tabac_sf)

#########################################
### Exemples                         ####
#########################################
#calcul intersection
contain=st_contains(arrondissement,tabac_sf[1,],sparse=F)
tabac_sf[1,"ARRONDISSEMENT"]
arrondissement$C_ARINSEE[contain] 

#matrice de contiguïté
(conti=st_touches(arrondissement,arrondissement,sparse=F))
plot(arrondissement[conti[20,],],max.plot=1)#les arrondissements contigus au 20eme  

#représentation spatiale des points sous la forme d'une nappe spatiale
library(btb)
library(cartography)

tabac$un=1#la variable à lisser
lissage=kernelSmoothing(tabac[,c("x","y","un")],200,2000)
lissage$dens=lissage$un*25#pour recuperer une densite
grid <- st_as_sf(smoothingToGrid(lissage, "2154"))
choroLayer(grid, var = "dens", nclass = 5,method =  "fisher-jenks",border=NA)   
plot(arrondissement,add=T)




