setwd("F:/Hackathon-2018/données")
#############################################
#Objectif : calcul des TF-IDF pour un mot et un NAF(2 premières lettres) donné
#Périmètre : échantillon de 70% du fichier RP 2017 (une fois DEPT 13 et 44 exclus)
#############################################

library(data.table)
library(dplyr)
library(tidytext)

#IMPORT BASE
#--------------------------

rec2017<-fread("rp_final_2017.csv")
dim(rec2017)
colnames(rec2017)

table(rec2017$DEPCOM_CODE)
rec2017$DEPT<-substr(rec2017$DEPCOM_CODE,1,2)
table(rec2017$DEPT)

#FICHIER APPRENTISSAGE
#--------------------------
#On ne prend pas les DEPT 13 et 44
rec2017.V2<-rec2017[rec2017$DEPT!="44" & rec2017$DEPT!="13",]
dim(rec2017.V2)
table(rec2017.V2$DEPT)

head(rec2017.V2)

t<-runif(dim(rec2017.V2)[1])

save(t,file="t")
#load("t")

rec<-rec2017.V2[t<=0.7,]
dim(rec)
#1 088 220 lignes

rec$Libelle2<-tolower(rec$ACTET_X)

#Application de la syntaxe NormalisationLIbelles.R qui permet 
#de supprimer les accents, supprimer les ponctuations, renommer certains mots ...

#Transformation en tibble
#--------------------------

rec.text <- data_frame(text = rec$Libelle2)
substr(rec$ACTET_C,1,2)->rec.text$ID
rownames(rec)->rec.text$line
head(rec.text)

#Construction des vecteurs de mots et Suppression de qques STOPWORD supplémentaires
#--------------------------

rec.text<- rec.text %>% unnest_tokens( value, text)

rec.text<- rec.text %>%  filter(!value %in% c("dans","n","z","a","et","en","des","pour","autant"
,"l","b","d"))
rec.text

#Suppression des pluriels
rec.text$value<-gsub("[s]$","",rec.text$value)

rec.text %>%  count(value, sort = TRUE) 

#Calcul TF - IDF
#--------------------------
#TF = 1
#IDF

rec.text$n<-1
#comptage nb fois où un mot apparait pour un NAF donné
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
dim(table(rec$ACTET_C))->NbNAFTotal
colnames(rec.text)
#on supprime les lignes acec ACTET_C non renseigné :
rec.text[rec.text$ID!="",]->rec.text
rec.text %>% group_by(value,ID) %>% mutate(NbFoisMotNAF=sum(n))%>% ungroup()  -> comptage2

comptage2[,-c(2)]->comptage2

#Nombre de NAF qui contiennent le mot
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#on dédoublonne la base :
comptage2[duplicated(comptage2)==FALSE,]->comptage2

comptage2 %>% group_by(value) %>% mutate(NbNAFAvecMot=sum(n)) %>% ungroup() -> comptage3

#Nombre de NAF rencontrés (au total indépendamment des mots)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
as.data.frame(table(substr(rec$ACTET_C,1,2)))->tab
head(tab)
colnames(tab)<-c("ID","NbNAF")

as.data.table(comptage3)->Rec.NAF
head(Rec.NAF)
colnames(Rec.NAF)

merge(Rec.NAF,tab,by=c("ID"),all.x=TRUE)->Rec.NAF
dim(Rec.NAF)

#CALCUL TFIDF
#-----------------------

dim(Rec.NAF)
Rec.NAF[,-c(3)]->Rec.NAF

Rec.NAF$TFIDF<-(Rec.NAF$NbFoisMotNAF /Rec.NAF$NbNAF) * log10(NbNAFTotal/(Rec.NAF$NbNAFAvecMot))

Rec.NAF[order(Rec.NAF$ID,-Rec.NAF$TFIDF),]->Rec.NAF
head(Rec.NAF)
save(Rec.NAF,file="Rec.NAF")

write.csv(Rec.NAF,"Rec.NAF.csv")

#Exemple :

Rec.NAF[Rec.NAF$ID =="01",]
Rec.NAF[Rec.NAF$value =="agricole",]