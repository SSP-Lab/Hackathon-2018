##################################################################
#Etape finale : on fusionne le fichier RP (1000 individus environ des DEPTS 13 et 44)
#avec le fichier SIRUS en filtrant sur la m?me commune et les 5 NAF (2 premi?res lettres) les plus probables
#puis on d?finir le SIREN pr?dit en compl?tant avec des calculs de distance sur RS et adresse 
##################################################################

setwd("C:/Users/ISFE3730/Desktop/Hackathon/Final/Hack_final")
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#CONSTRUCTION DE LA BASE RP + SIRUS + communes
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
library(data.table)
library(dplyr)
library(tidytext)
#Import du fichier SIRUS et conservation des seuls departements 13 et 44
#-----------------
sirus2017<-fread("sirus_2017.csv")
#On filtre sur les DEPT 13 et 44
head(sirus2017)
sirus2017<-sirus2017[substr(sirus2017$adr_et_post,1,2)=="44" | substr(sirus2017$adr_et_post,1,2)=="13",]

#selection de colonnes
sirus2017.V2<-sirus2017[,c(1,2,3,4,15,19,20,21,35,36,8)]
head(sirus2017.V2)

#creation de la variable siret_final
sirus2017.V2$nic[nchar(sirus2017.V2$nic)==2]<-paste("000",sirus2017.V2$nic[nchar(sirus2017.V2$nic)==2],sep="")
sirus2017.V2$nic[nchar(sirus2017.V2$nic)==3]<-paste("00",sirus2017.V2$nic[nchar(sirus2017.V2$nic)==3],sep="")
sirus2017.V2$nic[nchar(sirus2017.V2$nic)==4]<-paste("0",sirus2017.V2$nic[nchar(sirus2017.V2$nic)==4],sep="")
sirus2017.V2$SIRET_FINAL<-paste(sirus2017.V2$sirus_id,sirus2017.V2$nic,sep="") 


#On ajoute le code commune INSEE :
fread("CorrespInseePostal.csv")->CorrespInsee
head(CorrespInsee)
CorrespInsee<-CorrespInsee[,c(1,3)]
colnames(CorrespInsee)<-c("CodeInseeEnt","adr_et_post")
as.numeric(CorrespInsee$adr_et_post)->CorrespInsee$adr_et_post
CorrespInsee[duplicated(CorrespInsee)==FALSE,]->CorrespInsee
dim(CorrespInsee)

dim(sirus2017.V2)
#443124

CorrespInsee %>% group_by(adr_et_post) %>% mutate(numligne=row_number())->CorrespInsee
#Pusieurs codes communes INSEE  peuvent correspondre à un code postal

sirus2017.V2<-merge(sirus2017.V2,CorrespInsee,by="adr_et_post",allow.cartesian=TRUE)
dim(sirus2017.V2)
#775132
head(sirus2017.V2)

sirus2017.V2[,-c(1)]->sirus2017.V2
colnames(sirus2017.V2)[12]<-"adr_et_post"

#Donnees RP :
#-----------------
rec2017<-fread("rp_final_2017.csv")
#On ne prend QUE les DEPT 13 et 44
rec2017.1344<-rec2017[substr(rec2017$DEPCOM_CODE,1,2)=="44" | substr(rec2017$DEPCOM_CODE,1,2)=="13",]
rec2017.1344->rec

#Chargement des communes
#-----------------
#install.packages("sf")
library(sf)
library(readr)
communes=st_read("communes-20170112.shp")
communes=st_transform(communes, 2154)
communes=communes[order(communes$insee),]
#calcul des coordonneees des centroides
coord_communes=(st_coordinates(st_centroid(communes)))
communes$x=coord_communes[,1]
communes$y=coord_communes[,2]
#filtre sur les departements 13 et 44
library(dplyr)
library(data.table)
library(stringr)
communes=communes %>% select(insee,nom,geometry,x,y)
communes_filtre=communes[substr(communes$insee,1,2)=="44" | substr(communes$insee,1,2)=="13",]

#correspondance communes avec les adjacentes
corresp_cont = st_join(communes_filtre, communes_filtre, join = st_touches)

corresp_cont[1:20,]

#Recuperation des 5 NAF les plus probables :
#-----------------
#On ajoute les 5 NAF les plus probables calcules precedemment :
load("TAB4")
colnames(rec)
rec[,c(1,2,3,5,6,7,11,15,25)]->rec
rownames(rec)->rec$line
merge(rec,Tab4,by="line")->rec

#On prend un echantillon de 1000 individus :
s<-runif(dim(rec)[1])
save(s,file="s")
#load("s")
rec<-rec[s<=0.01,]
save(rec,file="rec")
dim(rec)
colnames(rec)

#Fusion fichier RP et SIRUS (filtre commune + NAF )
#-----------------
library("sqldf")
as.numeric(rec$CLT_C_C)->rec$adr_et_post

#jointure 1 entre rp et sirus sur la ville
as.numeric(sirus2017.V2$adr_et_post)->sirus2017.V2$adr_et_post
data_final.V1 <- left_join(rec, sirus2017.V2, by="adr_et_post") 
#filtre sur les NAF
data_final.V1<- data_final.V1 %>% filter(substr(ape,1,2)==NAF1 | substr(ape,1,2)==NAF2 | substr(ape,1,2)==NAF3 | substr(ape,1,2)==NAF4 | substr(ape,1,2)==NAF5) 

#jointure sur les communes adjacentes
corresp_cont$CLT_C_C=corresp_cont$insee.x
colnames(corresp_cont)
corresp_cont[,c(10,5)]->corresp_cont
corresp_cont=subset(as.data.frame(corresp_cont),select=c(CLT_C_C,insee.y))
prep.merge=left_join(x=rec,y=corresp_cont)
prep.merge$adr_et_post=NULL
prep.merge$adr_et_post=prep.merge$insee.y
prep.merge$insee.y=NULL
prep.merge$adr_et_post=as.numeric(paste(prep.merge$adr_et_post))
as.numeric(sirus2017.V2$adr_et_post)->sirus2017.V2$adr_et_post
data_final.V1BIS=left_join(prep.merge, sirus2017.V2, by="adr_et_post") 
#filtre sur les NAF
data_final.V1BIS<- data_final.V1BIS %>% filter(substr(ape,1,2)==NAF1 | substr(ape,1,2)==NAF2 | substr(ape,1,2)==NAF3 | substr(ape,1,2)==NAF4 | substr(ape,1,2)==NAF5) 

#concatenation des 2 listes
data_final.V2=rbind(data_final.V1,data_final.V1BIS)
data_final.V2=data_final.V1

#triage des donnees
data_final.V2=data_final.V2[order(data_final.V2$line),] 
head(data_final.V2)

#ANALYSE DES PREMIERS RESULTATS
#-------------------------------
data_final.V2$VALID<-rep(0,dim(data_final.V2)[1])
data_final.V2$VALID[data_final.V2$SIRET_FINAL==data_final.V2$SIRET_DEC]<-1

data_final.V2 %>% group_by (CABBI)  %>% mutate(VALID_MAX=max(VALID))->data_final.V2

data_final.Analyse<-data_final.V2[data_final.V2$VALID_MAX==data_final.V2$VALID,]
data_final.Analyse %>% group_by(CABBI) %>% mutate(numligne=row_number())->data_final.Analyse
data_final.Analyse<-data_final.Analyse[data_final.Analyse$numligne==1,]
dim(data_final.Analyse)
#Plus que 769 DE sur 884 : une partie des DE ne parvient pas à être fusionnée avec la base des SIRHUS

table(data_final.Analyse$VALID_MAX)
#446 SIRET TROUVES  pour 769 recensements au début soit 57% (mais uniquement 50% des 884 du début)

#dim(rec)
#table(rec$DEPCOM_CODE,useNA="always")

#colnames(data_final.V2)
#data_final.Analyse<-data_final.V2[,c("CABBI","SIRET_DEC","SIRET_FINAL","VALID")]

#######################################################################
#nettoyage des adresses du RP
data_final.V2$adressedata_final<-tolower(data_final.V2$NOMVOI_X)
data_final.V2$adressedata_final<-gsub("[éèëê]","e",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("[àäâ]","a",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("[ôö]","o",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("[ûùü]","u",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("[îï]","i",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("[ç]","c",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("[,().]","",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("l'"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("d'"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("c'"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("-"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("_"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("%"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(","," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("/"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub("'"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" des "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" les "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" de "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" la "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" sur "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" le "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" du "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" en "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" sous "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" et "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" bt "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" l "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" d "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" bo "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" bp "," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" des$"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" les$"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" de$"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" la$"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" sur$"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" le$"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" du$"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" en$"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" sous$"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" et$"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" bt$"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" l$"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" d$"," ",data_final.V2$adressedata_final)
data_final.V2$adressedata_final<-gsub(" bo$"," ",data_final.V2$adressedata_final)

#nettoyage des adresses sirus
data_final.V2$adr_sir<-tolower(data_final.V2$adr_et_voie_lib)
data_final.V2$adr_sir<-gsub("[éèëê]","e",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("[àäâ]","a",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("[ôö]","o",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("[ûùü]","u",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("[îï]","i",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("[ç]","c",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("[,().]","",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("l'"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("d'"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("c'"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("-"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("_"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("%"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(","," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("/"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub("'"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" des "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" les "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" de "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" la "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" sur "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" le "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" du "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" en "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" sous "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" et "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" bt "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" l "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" d "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" bo "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" bp "," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" des$"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" les$"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" de$"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" la$"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" sur$"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" le$"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" du$"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" en$"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" sous$"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" et$"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" bt$"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" l$"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" d$"," ",data_final.V2$adr_sir)
data_final.V2$adr_sir<-gsub(" bo$"," ",data_final.V2$adr_sir)

data_adresse=data_final.V2[,c("adressedata_final","adr_sir")]

#install.packages("stringdist")
#install.packages("stringr")
library(stringdist)
library(stringr)

#calcul de la distance de levensthein sur les adresses
str_trim(data_final.V2$adressedata_final)->data_final.V2$adressedata_final
str_trim(data_final.V2$adr_sir)->data_final.V2$adr_sir
data_final.V2$dist_ad=stringdist(data_final.V2$adressedata_final,data_final.V2$adr_sir)
#minadr=which.min(dist_ad)
#print(paste0("Adresse la plus proche: ", recherchetest[minadr,18],". Distance : ",dist_ad[minadr] ))


##########################################################################
#nettoyage des raisons sociales RP
data_final.V2$RS2<-tolower(data_final.V2$RS_X)
data_final.V2$RS2<-gsub("[éèëê]","e",data_final.V2$RS2)
data_final.V2$RS2<-gsub("[àäâ]","a",data_final.V2$RS2)
data_final.V2$RS2<-gsub("[ôö]","o",data_final.V2$RS2)
data_final.V2$RS2<-gsub("[ûùü]","u",data_final.V2$RS2)
data_final.V2$RS2<-gsub("[îï]","i",data_final.V2$RS2)
data_final.V2$RS2<-gsub("[ç]","c",data_final.V2$RS2)
data_final.V2$RS2<-gsub("[,().]","",data_final.V2$RS2)
data_final.V2$RS2<-gsub("l'"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub("d'"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub("c'"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub("-"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub("_"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub("%"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(","," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub("/"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub("'"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" des "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" les "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" de "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" la "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" sur "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" le "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" du "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" en "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" sous "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" et "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" bt "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" l "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" d "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" bo "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" bp "," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" des$"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" les$"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" de$"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" la$"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" sur$"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" le$"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" du$"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" en$"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" sous$"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" et$"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" bt$"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" l$"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" d$"," ",data_final.V2$RS2)
data_final.V2$RS2<-gsub(" bo$"," ",data_final.V2$RS2)

#nettoyage des differentes raisons sociales SIRUS
data_final.V2$enseignecorrige<-tolower(data_final.V2$enseigne_et1)
data_final.V2$enseignecorrige<-gsub("[éèëê]","e",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("[àäâ]","a",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("[ôö]","o",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("[ûùü]","u",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("[îï]","i",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("[ç]","c",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("[,().]","",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("l'"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("d'"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("c'"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("-"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("_"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("%"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(","," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("/"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub("'"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" des "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" les "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" de "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" la "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" sur "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" le "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" du "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" en "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" sous "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" et "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" bt "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" l "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" d "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" bo "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" bp "," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" des$"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" les$"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" de$"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" la$"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" sur$"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" le$"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" du$"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" en$"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" sous$"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" et$"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" bt$"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" l$"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" d$"," ",data_final.V2$enseignecorrige)
data_final.V2$enseignecorrige<-gsub(" bo$"," ",data_final.V2$enseignecorrige)

data_final.V2$adr_et_l1corrige<-tolower(data_final.V2$adr_et_l1)
data_final.V2$adr_et_l1corrige<-gsub("[éèëê]","e",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("[àäâ]","a",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("[ôö]","o",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("[ûùü]","u",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("[îï]","i",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("[ç]","c",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("[,().]","",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("l'"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("d'"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("c'"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("-"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("_"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("%"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(","," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("/"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub("'"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" des "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" les "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" de "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" la "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" sur "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" le "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" du "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" en "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" sous "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" et "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" bt "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" l "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" d "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" bo "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" bp "," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" des$"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" les$"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" de$"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" la$"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" sur$"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" le$"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" du$"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" en$"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" sous$"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" et$"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" bt$"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" l$"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" d$"," ",data_final.V2$adr_et_l1corrige)
data_final.V2$adr_et_l1corrige<-gsub(" bo$"," ",data_final.V2$adr_et_l1corrige)


data_final.V2$adr_et_l2corrige<-tolower(data_final.V2$adr_et_l2)
data_final.V2$adr_et_l2corrige<-gsub("[éèëê]","e",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("[àäâ]","a",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("[ôö]","o",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("[ûùü]","u",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("[îï]","i",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("[ç]","c",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("[,().]","",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("l'"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("d'"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("c'"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("-"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("_"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("%"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(","," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("/"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub("'"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" des "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" les "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" de "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" la "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" sur "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" le "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" du "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" en "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" sous "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" et "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" bt "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" l "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" d "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" bo "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" bp "," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" des$"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" les$"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" de$"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" la$"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" sur$"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" le$"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" du$"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" en$"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" sous$"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" et$"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" bt$"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" l$"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" d$"," ",data_final.V2$adr_et_l2corrige)
data_final.V2$adr_et_l2corrige<-gsub(" bo$"," ",data_final.V2$adr_et_l2corrige)

data_final.V2$denomcorrige<-tolower(data_final.V2$denom)
data_final.V2$denomcorrige<-gsub("[éèëê]","e",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("[àäâ]","a",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("[ôö]","o",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("[ûùü]","u",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("[îï]","i",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("[ç]","c",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("[,().]","",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("l'"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("d'"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("c'"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("-"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("_"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("%"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(","," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("/"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub("'"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" des "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" les "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" de "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" la "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" sur "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" le "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" du "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" en "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" sous "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" et "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" bt "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" l "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" d "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" bo "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" bp "," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" des$"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" les$"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" de$"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" la$"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" sur$"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" le$"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" du$"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" en$"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" sous$"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" et$"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" bt$"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" l$"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" d$"," ",data_final.V2$denomcorrige)
data_final.V2$denomcorrige<-gsub(" bo$"," ",data_final.V2$denomcorrige)

data_final.V2$denomcondesecorrige<-tolower(data_final.V2$denom_condense)
data_final.V2$denomcondesecorrige<-gsub("[éèëê]","e",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("[àäâ]","a",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("[ôö]","o",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("[ûùü]","u",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("[îï]","i",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("[ç]","c",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("[,().]","",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("l'"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("d'"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("c'"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("-"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("_"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("%"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(","," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("/"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub("'"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" des "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" les "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" de "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" la "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" sur "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" le "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" du "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" en "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" sous "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" et "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" bt "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" l "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" d "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" bo "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" bp "," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" des$"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" les$"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" de$"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" la$"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" sur$"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" le$"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" du$"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" en$"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" sous$"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" et$"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" bt$"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" l$"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" d$"," ",data_final.V2$denomcondesecorrige)
data_final.V2$denomcondesecorrige<-gsub(" bo$"," ",data_final.V2$denomcondesecorrige)


#calcul de la distance de levenstheim sur les raisons sociales
data_final.V2$dist_enseigne=stringdist(data_final.V2$RS2,data_final.V2$enseignecorrige)
data_final.V2$dist_l1=stringdist(data_final.V2$RS2,data_final.V2$adr_et_l1corrige)
data_final.V2$dist_l2=stringdist(data_final.V2$RS2,data_final.V2$adr_et_l2corrige)
data_final.V2$dist_denom=stringdist(data_final.V2$RS2,data_final.V2$denomcorrige)
data_final.V2$dist_denomcondense=stringdist(data_final.V2$RS2,data_final.V2$denomcondesecorrige)

#ne pas prendre en compte les distances quand un des elements est a NA
data_final.V2$dist_ad[data_final.V2$adressedata_final==""]<-NA
data_final.V2$dist_ad[data_final.V2$adr_sir==""]<-NA
data_final.V2$dist_enseigne[data_final.V2$RS2==""]<-NA
data_final.V2$dist_l1[data_final.V2$RS2==""]<-NA
data_final.V2$dist_l2[data_final.V2$RS2==""]<-NA
data_final.V2$dist_denom[data_final.V2$RS2==""]<-NA
data_final.V2$dist_denomcondense[data_final.V2$RS2==""]<-NA
data_final.V2$dist_enseigne[data_final.V2$enseignecorrige==""]<-NA
data_final.V2$dist_l1[data_final.V2$adr_et_l1corrige==""]<-NA
data_final.V2$dist_l2[data_final.V2$adr_et_l2corrige==""]<-NA
data_final.V2$dist_denom[data_final.V2$denomcorrige==""]<-NA
data_final.V2$dist_denomcondense[data_final.V2$denomcondesecorrige==""]<-NA

colnames(data_final.V2)

#selection de la distance minimale sur la raison sociale
data_final.V2$dist_rs=apply(data_final.V2[,40:44],1,min,na.rm=TRUE)
data_final.V2$dist_rs[data_final.V2$dist_rs==Inf]<-NA

#calcul de la distance ponderee sur ladresse et la raison sociale 
data_final.V2$score<-(1/2*data_final.V2$dist_rs+data_final.V2$dist_ad)/2
data_final.V2$score[is.na(data_final.V2$dist_rs)]<-data_final.V2$dist_ad[is.na(data_final.V2$dist_rs)]
data_final.V2$score[is.na(data_final.V2$dist_ad)]<-1/2*data_final.V2$dist_rs[is.na(data_final.V2$dist_ad)]

#data_final$score<-data_final$dist_ad
data_final.V3<-data_final.V2 #[,c(3,41,16,64,65,125,126,113)]
data_final.V3$ID<-as.character(data_final.V3$CABBI)

data_final.V3 %>% group_by(ID) %>% mutate(MINSCORE=min(score))->data_final.V3


library(dplyr)

#selection des lignes avec la distance minimale
data_final.V4<-data_final.V3[data_final.V3$score==data_final.V3$MINSCORE,]

data_final.V4 %>% group_by(CABBI) %>% mutate(numligne=row_number())->data_final.V4

#data_final.V2<-data_final.V2[data_final.V2$numligne==1,]
#dim(data_final.V2)
#merge(data_final.V2,rec,by="CABBI",all.x=TRUE)->data_final.V2
   #data_final.V2<-data_final.V2[!is.na(nchar(data_final.V2$siren)),]


data_final.V5=data_final.V4[data_final.V4$numligne==1,]

data_final.V5$VALID_FINAL<-rep(0,dim(data_final.V5)[1])
data_final.V5$VALID_FINAL[data_final.V5$SIRET_FINAL==data_final.V5$SIRET_DEC]<-1

table(data_final.V5$VALID_FINAL)
#247 reco de siret sont correctes soit 32 % des recensements de départ



data_final.export<-data_final.V5[,c("CABBI","SIRET_DEC","SIRET_FINAL","VALID")]

write.csv(data_final.export,file="datafinal_export.csv") 