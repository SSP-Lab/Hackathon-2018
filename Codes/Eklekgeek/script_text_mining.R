#### Hackathon - Script de calcul des distances textuelles et géographique
## Pour confirmer / valider les Sirets dont un pool a été obtenu après le scraping
## effectué sur la première page d'un moteur de recherche (Qwant, Bing, Google)

install.packages("stringdist")
install.packages("data.table")
library(stringdist)
library(stringi)
library(tm)
library(stringr)
library(readr)
library(data.table)
library(FactoMineR)
options(scipen=999)


### Import csv - Sirus et FIchier après scraping avec pool de Siret sur un échantillon
sirus <- fread("C:/Users/maxim/Documents/Hackathon_Insee.git/données/sirus_2017.csv") 
scraping <- read.csv2("C:/Users/maxim/Documents/Hackathon_Insee.git/données/no doublon_final.csv",
                      numerals="no.loss", stringsAsFactors = F)

scraping$siret<-str_pad(scraping$siret, 14, side = c("left"), pad = "0")

sirus<-subset(sirus, select = c("sirus_id", "nic", "ape", "eff_3112_et",
                                "enseigne_et1", "nom_comm_et","cj", "sigle",
                                "denom", "denom_condense", "x", "y"))

sirus$sirus_id<-str_pad(sirus$sirus_id, 9, side = c("left"), pad = "0")
sirus$nic<-str_pad(sirus$nic, 5, side = c("left"), pad = "0")
sirus$siret<-paste0(sirus$sirus_id,sirus$nic)

# Table avec l'ensemble des infos après fusion
scraping_merge <- merge(scraping, sirus, by.x = "siret", by.y = "siret", all.x = T, all.y = F)

### Nettoyage texte
nettoyage <- function(texte) {
  documents <- Corpus(VectorSource(texte))
  # Suppression Accents
  accent <- function(x) stri_trans_general(x, "Latin-ASCII") # cela signifie qu'on remplace un caractère encodé en Latin1 par son équivalent le plus proche en ASCII, il n'y a par exemple pas de caractères accentués en ASCII
  documents <- tm_map(documents, content_transformer(accent))
  # On garde chiffres et lettres seulement
  documents <- tm_map(documents, content_transformer(gsub), pattern = "[^a-zA-Z0-9]", replacement = " ")
  # Passage minuscule
  documents <- tm_map(documents, content_transformer(tolower))
  documents <- tm_map(documents, stripWhitespace) #n'enleve pas le tout premier espace
  documents <- tm_map(documents, content_transformer(gsub), pattern = "^\\s+", replacement = "")
  return(documents$content)
}
#nettoyage(scraping_merge$rs_rp)



### Calcul distance - ON prend distance de Jaro car normé entre 0 (match parfait) et 1

### Variables à prendre en compte pour potentiel matching
# denom
# denom_condense
# enseigne_et1
# nom_comm_et
# sigle

## On prend la meilleure distance parmi les 5 variables de raison sociale de Sirus.
calcul_matching_jw <- function(table) {
  # Nettoyage
  table_travail <- table
  table_travail$nettoyeA <- nettoyage(table_travail$rs_rp)
  table_travail$nettoyeB1 <- nettoyage(table_travail$denom)
  table_travail$nettoyeB2<- nettoyage(table_travail$denom_condense)
  table_travail$nettoyeB3<- nettoyage(table_travail$enseigne_et1)
  table_travail$nettoyeB4<- nettoyage(table_travail$nom_comm_et)
  table_travail$nettoyeB5 <- nettoyage(table_travail$sigle)
  table_travail$dist1 <- stringdist(table_travail$nettoyeA,table_travail$nettoyeB1, method = "jw")
  table_travail$dist2 <- stringdist(table_travail$nettoyeA,table_travail$nettoyeB2, method = "jw")
  table_travail$dist3 <- stringdist(table_travail$nettoyeA,table_travail$nettoyeB3, method = "jw")
  table_travail$dist4 <- stringdist(table_travail$nettoyeA,table_travail$nettoyeB4, method = "jw")
  table_travail$dist5 <- stringdist(table_travail$nettoyeA,table_travail$nettoyeB5, method = "jw")
  table_travail[is.na(table_travail)] <- 1
  distance_meilleure <- with(table_travail, pmin(dist1, dist2, dist3, dist4))
  return(distance_meilleure)
}
  

scraping_merge$meilleure_dist_jw<- calcul_matching_jw(scraping_merge)

## Calcul distance géographique (euclidienne)

scraping_merge$x_rpnum<-as.numeric(scraping_merge$x_rp)
scraping_merge$y_rpnum<-as.numeric(scraping_merge$y_rp)
scraping_merge$distance_geo <- sqrt((scraping_merge$x_rpnum-scraping_merge$x)^2 + (scraping_merge$y_rpnum-scraping_merge$y)^2) 


### ACP sur les variables de qualité du scraping
## On utilise l'indicateur de concordance en variable supplémentaire.

table_qualite<-subset(scraping_merge, 
          select = c("position_page","meilleure_dist_jw","distance_geo","is_equals"))

res.pca = PCA(table_qualite, scale.unit=TRUE,quali.sup=4, ncp=2, graph=T) 

res.pca$ind

dimdesc(res.pca, axes=c(1,2))
plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=4)

scraping_merge["indic_synthetique"]<-res.pca$ind$coord[,"Dim.1"]

## Tracé des densités en fonction de concordance ou non, pour la projection sur 1er axe
projectionT <- subset(scraping_merge, scraping_merge$is_equals == "True")
projectionF <- subset(scraping_merge, scraping_merge$is_equals == "False")
plot(density(projectionT$indic_synthetique), col = "green", main = "Score synthétique", lwd=8)
lines(density(projectionF$indic_synthetique), col = "red", lwd = 8)
legend("topright", 95, legend=c("Match", "Non match"),
       col=c("green", "red"), lty=1,lwd = 5)

write.csv(scraping_merge, "E:/eval/table_qualite.csv")



## Choix d'un seuil de confiance (sur indicateur synéthétique) pour valider le scraping.
temp<-scraping_merge
temp$indic_arrondi <- round(temp$indic_synthetique, digits =1)
table(temp$indic_arrondi, temp$is_equals)
## Ici on prend -1.5 ..