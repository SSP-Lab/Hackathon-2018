library(rvest)
library(curl)
library(dplyr)
library(stringr)
library(data.table)

setwd("/Users/ingacarrelet/Hackathon-2018-master/illumines/tables")

############ Utiliser Selenium pour contourner le pb de bloquage du site #########
#library(RSelenium)
#RSelenium::rsDriver()
#remDr <- remoteDriver(browserName = "chrome")
#remDr$open()
#appURL <- 'https://fr.kompass.com/'
#remDr$navigate(appURL)
#remDr$findElement("id", "username")$sendKeysToElement(list("pietrodelsud@hotmail.com"))
#remDr$findElement("id", "password")$sendKeysToElement(list("Apt84%Marseille13", key='enter'))
#appURL <- 'https://fr.kompass.com/'
#remDr$navigate(appURL)



############## import des donnees ##################
rp17_reg<-fread("rp17_reg.csv", stringsAsFactors = F)
rp17_filtree<-rp17_reg[rp17_reg$I_MCA_C=="E" | rp17_reg$I_MCA_C=="",] #on selectionne les lignes pour lesquelles le codage MCA a été jugé mauvais

## creation des variables à recuperer par scraping
rp17_filtree$sirus_id<-rep("",dim(rp17_filtree)[1])
rp17_filtree$siret_scr<-rep("",dim(rp17_filtree)[1])
rp17_filtree$apet<-rep("",dim(rp17_filtree)[1])
rp17_filtree$rs2<-as.character(rp17_filtree$RS_X)
rp17_filtree$reg2<-as.character(rp17_filtree$LIBGEO)

# fonction recuperant les n derniers caracteres d une chaine, utilisee plus bas dans le code pour recuperer le code NAF
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

nb<-dim(rp17_filtree)[1]

# on boucle la procedure
for (a in 1:nb){
  #on met un sleep pour temporiser entre les requetes, on le place ici a cause du "next" plus bas
  Sys.sleep(0.5)
  
  i<-as.character(rp17_filtree$REG[a])
  j<-as.character(substr(rp17_filtree$DEPCOM_CODE[a],1,2))
  k<-as.character(rp17_filtree$DEPCOM_CODE[a])
  l<-as.character(rp17_filtree$reg2[a])
  m<-as.character(rp17_filtree$rs2[a])
  # les elements dans i,j,k,l et m font partie du lien de la recherche qui nous interesse
  
  # on construit le 1er lien
  link1<-paste("https://fr.kompass.com/searchCompanies?acClassif=&localizationCode=FR_",i,"_",j,"_",k,"&localizationLabel=",l,"&localizationType=town&text=%22",m,"%22&searchType=COMPANYNAME", sep="")
  # on recupere notre 1ere page
  page1 <- read_html(link1)
  
  # on recupere les elements pour acceder au 2nd lien
  lien <- html_nodes(page1, ".details h2 a") %>% html_attr("href")
  if(is.na(lien[1])) next # si il est vide on stop cette iteration et passe a la prochaine
  
  # on construit le 2eme lien
  link2<-paste("https://fr.kompass.com",lien[1],sep="") 
  #pour commencer nous avons limité à un seul lien récupéré, 
  #il faudrait poursuivre et généraliser à plusieurs liens pour ensuite mettre en place la procedure de matching avec l'apet ou l'adresse pour selectionner le siret le plus pertinent
  # on recupere notre 2ere page contenant les infos qui nous interessent
  page2<-read_html(link2)
  
  # recuperation infos
  rs_recup2<-html_nodes(page2, ".presentation.global ul li p") %>% html_text()
  # on isole le siret (puis le siren)
  rp17_filtree$siret[a]<-str_replace_all(rs_recup2[4], fixed(" "), "")
  rp17_filtree$sirus_id[a]<-substr(siret,1,9)
  # on isole l apet
  activ <- (html_nodes(page2, ".activities.extra span")%>% html_text())[1]
  activ
  rp17_filtree$apet[a]<-substr(substrRight(activ, 7),2,6)
  
}

# export du fichier test rendu lors du hackathon
#res_illum<-rp17_filtree[c(1,5,13),c("CABBI","siret","apet")]
#colnames(res_illum)[3]<-"naf"
#res_illum
#write.csv2(res_illum,"/Users/ingacarrelet/Hackathon-2018-master/res_illum.csv", sep = ";", row.names = F, quote=F)
