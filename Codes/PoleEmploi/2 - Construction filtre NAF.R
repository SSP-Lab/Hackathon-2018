
##################################################################
#Determination des 5 NAF (2 premiers caractères) les plus probables pour chaque intitulé de ACTET_X
#sur échantillon de travail : départements 13 et 44 du fichier RP 2017
##################################################################

#COnstruction fichier de travail : RP 2017, depts 13 et 44
#Rappel : l'apprentissage des TFIDF Mot-NAF a été effectué sur 70% du RP 2017 (et hors DEPTS 13 et 44)
#-----------------

rec2017<-fread("rp_final_2017.csv")
rec2017$DEPT<-substr(rec2017$DEPCOM_CODE,1,2)
table(rec2017$DEPT)
#On ne prend QUE les DEPT 13 et 44
rec2017.1344<-rec2017[rec2017$DEPT=="44" | rec2017$DEPT=="13",]
dim(rec2017.1344)
rec2017.1344->rec
dim(rec)

rec$Libelle2<-tolower(rec$ACTET_X)
#Application de la syntaxe NormalisationLIbelles.R qui permet 
#de supprimer les accents, supprimer les ponctuations, renommer certains mots ...

#Transformation en tibble

library(dplyr)
rec.text <- data_frame(text = rec$Libelle2)
substr(rec$ACTET_C,1,2)->rec.text$ID
rownames(rec)->rec.text$line

#Construction des vecteurs de mots et Suppression de qques STOPWORD supp.

library(tidytext)

rec.text<- rec.text %>%
  unnest_tokens( value, text)

rec.text<- rec.text %>%  filter(!value %in% c("dans","n","z","du","de","le","la","les","a","et","en","des","pour","autant"
                                              ,"l","b","d"))
rec.text

#Suppression des pluriels
rec.text$value<-gsub("[s]$","",rec.text$value)

save(rec.text,file="rec.test.1Mot")

#Import fichier comprenant les TFIDF
#-----------------

load("Rec.NAF")
head(Rec.NAF)

Rec.NAF[,c(1,2,6)]->TFIDF.NAF
head(TFIDF.NAF)

#Exemples :
TFIDF.NAF[TFIDF.NAF$value=="fabrication",]
TFIDF.NAF[TFIDF.NAF$value=="brioche",]
TFIDF.NAF[TFIDF.NAF$value=="transport",]

#Matrice Mots/NAF
#-----------------

library(reshape2)
library(tidyr)

MatriceNAF <- spread(TFIDF.NAF, ID, TFIDF)
MatriceNAF  [1:10,1:10]

#Calculer rangs associés à plusieurs mots
#-----------------

#On repart du fichier de recensement (échantillon test)
#rec.text

#load("rec.test.1Mot")

Mot<-rec.text

#On fusionne avec la matrice des mots/NAF

left_join(Mot,MatriceNAF,"value")->RechercheNAF

#On remplace les NAF par des zero
replace(RechercheNAF,is.na(RechercheNAF),0)->RechercheNAF

#Pour un ensemble de mots donné (=une activité déclarée) : on calcule la moyenne des TDIDF par colonne
#---------------------------------
RechercheNAF %>% group_by (line) %>% summarise_all(funs(mean)) ->RechercheNAF.2

#Exemples :
head(RechercheNAF)
head(RechercheNAF.2)

#On repasse à un format par ligne
#---------------------------------
colnames(RechercheNAF.2)
head(RechercheNAF.2)

gather(RechercheNAF.2,condition, TFIDF,"01":"99")->data.long
head(data.long)
data.long[data.long$TFIDF>0,]->data.long

data.long[order(data.long$line,-data.long$TFIDF),-c(2)]->data.long

head(data.long)

##################################################################
#DETERMINATION 5 PREMIERS NAF POUR CHAQUE INTITULE ACTET_X
##################################################################

head(data.long)

data.long %>% group_by (line) %>% mutate (rang.1Mot=rank(-TFIDF))->Matrice_Finale
Matrice_Finale.V3<-Matrice_Finale[Matrice_Finale$rang.1Mot<=5,]

Matrice_Finale.V3$ligne<-Matrice_Finale.V3$rang.1Mot
NAF1<-Matrice_Finale.V3[Matrice_Finale.V3$ligne==1,c(1,3)]
colnames(NAF1)[2]<-"NAF1"
NAF2<-Matrice_Finale.V3[Matrice_Finale.V3$ligne==2,c(1,3)]
colnames(NAF2)[2]<-"NAF2"
NAF3<-Matrice_Finale.V3[Matrice_Finale.V3$ligne==3,c(1,3)]
colnames(NAF3)[2]<-"NAF3"
NAF4<-Matrice_Finale.V3[Matrice_Finale.V3$ligne==4,c(1,3)]
colnames(NAF4)[2]<-"NAF4"
NAF5<-Matrice_Finale.V3[Matrice_Finale.V3$ligne==5,c(1,3)]
colnames(NAF5)[2]<-"NAF5"

merge(NAF1,NAF2,BY=c("line"))->Tab1
merge(Tab1,NAF3,BY=c("line"))->Tab2
merge(Tab2,NAF4,BY=c("line"))->Tab3
merge(Tab3,NAF5,BY=c("line"))->Tab4

save(Tab4,file="TAB4")

load("TAB4")

##################################################################
#PERFORMANCES
##################################################################

rownames(rec)->rec$line
dim(rec)
merge(rec,Tab4,by="line")->TABFINAL

colnames(TABFINAL)
TABFINAL[,c(1,4,50:54)]->TABFINAL

substr(TABFINAL$ACTET_C,1,2)->TABFINAL$ACTET_C_BIS
TABFINAL$RES<-(TABFINAL$NAF1==TABFINAL$ACTET_C_BIS | 
                         TABFINAL$NAF2==TABFINAL$ACTET_C_BIS |
                         TABFINAL$NAF3==TABFINAL$ACTET_C_BIS |
                         TABFINAL$NAF4==TABFINAL$ACTET_C_BIS |
                         TABFINAL$NAF5==TABFINAL$ACTET_C_BIS)
prop.table(table(TABFINAL$RES,useNA="always"))
#78% avec au moins un des 5 NAF prédits (2 premières lettres) sont égaux aux NAF (2 premières lettres) de ACTET_C
