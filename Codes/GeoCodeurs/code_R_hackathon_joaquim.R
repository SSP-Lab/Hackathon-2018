setwd("C:/Users/Joaquim/Desktop/hackathon ssplab/données")
library(stringr)
library(stringdist)
library(tm)





#imortation données à coder
don2014 <-read.csv2("rp_final_2014.csv",header =TRUE, sep = ";")

#importation "apprentissage" 
sirus2014 <-read.csv2("sirus_2014.csv",header =TRUE, sep = ";")


#### retirer variable
sirus<-subset(sirus2014, select=c("sirus_id","nic","adr_et_loc_geo","enseigne_et1","nom_comm_et","adr_et_compl","adr_et_voie_num","adr_et_voie_repet","adr_et_voie_type","adr_et_voie_lib","adr_et_cedex","x","y","SourceXYW","qual","adr_depcom","denom","denom_condense","sigle","enseigne"))



##### séparation ille et vilaine
sirus2014_35<-sirus[substr(sirus$adr_et_loc_geo,1,2)==35,]
write.csv2(sirus2014_35,"sirus2014_35.csv")

sirus2014_35<-read.csv2("sirus2014_35.csv")



rp2014_35<-don2014[substr(don2014$CLT_C_C,1,2)==35 | (substr(don2014$CLT_C_C,1,2)=="" && substr(don2014$DEPCOM_CODE,1,2)==35) ,]
write.csv2(rp2014_35,"rp2014_35.csv", sep=";",quote = FALSE )


rp2014_35<-read.csv2("rp2014_35.csv",header =TRUE, sep = ";")









### ligne de code à mettre en fonction
stringdist(as.String(rp2014_35[1,4][[1]]),sirus2014_35$denom,"dl")
which.min(stringdist(as.String(rp2014_35[1,4][[1]]),sirus2014_35$denom,"dl"))


#### programme appliqué sur l'ensemble du département
table <-subset(rp2014_35,select=c("RS_X","SIRET_DEC","CABBI"))
base<-subset(sirus2014_35,select = c("denom","sirus_id","nic","enseigne_et1"))

result<-data.frame(input=table$RS_X,id=table$SIRET_DEC, rep="",stringsAsFactors=F)
for (i in (1:length(table$RS_X))) {
  mini<-which.min(stringdist(as.String(table[i,1]),base$denom,"dl"))
  result[i,3] = base[mini,2]*100000+base[mini,3]
  if(i%%100==0){print(i)}
}


result$bon<-ifelse(result$rep== result$id ,1,0 )
sum(result$bon)/length(result$bon)*100


#### creation base bretagne
## sirus 2014 bretagne
sirus2014_bret <- subset(sirus, substr(sirus$adr_et_loc_geo,1,2)==22 | substr(sirus$adr_et_loc_geo,1,2)==29 | substr(sirus$adr_et_loc_geo,1,2)==35 | substr(sirus$adr_et_loc_geo,1,2)==56)
write.csv2(sirus2014_bret,"sirus2014_bret.csv")
test<-read.csv2("sirus2014_bret.csv")

## rp 2014 bretagne
rp2014_bret<-don2014[ (substr(don2014$CLT_C_C,1,2)==22 | (substr(don2014$CLT_C_C,1,2)=="" && substr(don2014$DEPCOM_CODE,1,2)==22))| (substr(don2014$CLT_C_C,1,2)==29 | (substr(don2014$CLT_C_C,1,2)=="" && substr(don2014$DEPCOM_CODE,1,2)==29)) | (substr(don2014$CLT_C_C,1,2)==35 | (substr(don2014$CLT_C_C,1,2)=="" && substr(don2014$DEPCOM_CODE,1,2)==35)) | (substr(don2014$CLT_C_C,1,2)==56 | (substr(don2014$CLT_C_C,1,2)=="" && substr(don2014$DEPCOM_CODE,1,2)==56)) ,]


#### test de distance sur bretagne

table <-subset(rp2014_bret,select=c("RS_X","SIRET_DEC"))
base<-subset(sirus2014_bret,select = c("denom","sirus_id","nic","enseigne_et1"))

result<-data.frame(input=table$RS_X,id=table$SIRET_DEC, rep="",stringsAsFactors=F)
for (i in (1:length(table$RS_X))) {
  mini<-which.min(stringdist(as.String(table[i,1]),base$denom,"dl"))
  result[i,3] = base[mini,2]*100000+base[mini,3]
  if(i%%100==0){print(i)}
}


result$bon<-ifelse(result$rep== result$id ,1,0 )
sum(result$bon)/length(result$bon)*100



### table de maelle

tabmaelle<-read.csv2("pour_joaquim.csv")

### table pour rajouter la denomination associé au siret proposé
base$siret<-base$sirus_id*100000+base$nic
baseint<-subset(base,select=c("siret","denom"))

## création table maelle avec rajout de la dénom associé au siret proposé

tabmaelleInt<-merge(x=tabmaelle,y=baseint, by.x = "SIRET",by.y="siret")




comparaison <-function(tab){

  # unicité du cabbi
  vectcabbi<-unique(tab$CABBI)
  # creation table resultat et assertion de cabbi
  tableresult<-data.frame(cabbi=vectcabbi,siretverif="",stringsAsFactors=F)
  #insertion cabbi
  #tableresult$cabbi<-vectcabbi
  
  #variable intermediaire
  siretinter<-c()
  siretfinal<-c()
  listesiret<-c()
  vtrue<-c()
  clonecabbi<-vectcabbi
  for(i in clonecabbi){
    print ("entrer premire bloucle : n ")
    print(i)
    noms<-c()
    siretinter<-c()
    vtrue<-tab$CABBI==i
    siretinter<-tab$SIRET[vtrue]
    print(length(siretinter))
    for(j in 1:length(siretinter)){
      noms<-c(noms,as.String( base$denom[ base$siret ==siretinter[j]]) )
    }
   indice<- which.min(stringdist(as.String(tab$RS_X),noms,"dl"))
   siretfinal<-siretinter[indice]
   print(siretfinal)
   ## ici je fais un vecteur que je compte attacher apres en esperant que l'ordre ne change pas
   listesiret<-c(listesiret,siretfinal)
  }

  tableresult$siretverif<-listesiret
  return(tableresult)
}

tablecomparaison<-comparaison(tabmaelleInt)

#### fusion résultats pour coparaison


final<-merge(x=tabmaelleInt,y=tablecomparaison, by.x="CABBI",by.y = "cabbi")


vectdouble<-!duplicated(final$cabbi)
tempo<-final[vectdouble,]

tempo$verifaction<-ifelse(tempo$siret_final== tempo$siretverif ,1,0 )
resf<-tempo$verifaction

write.csv2(tempo,"vérif_multiple_geo.csv")








