
#############################


# rp<-read.csv("C:/Users/Beatrice/Desktop/Hackathon-2018/DonneesTransmisesHackathon/rp_final_2017.csv",sep=";")
# str(rp) # 1643901 obs.
# 
# liste_opq<-c("POMPIERS(?)","MINISTERE","EDUCATION","LYCEE","ECOLE","CRECHE","GARDERIE","COLLEGE","UNIVERSIT(E|AIRE)",
# "PUBLI(QUE|C)S? ","HOPITA(L|UX)","AP(-)?(^)?HP","HOSPITALIER(S)?","POLICE","GENDARMERIE","FONCTIONNAIRE",
# "DEPARTEMENT(AL)?(AUX)?","MAIRIE", "PREFECT(URE)?(ORAL)?(ORAUX)?","RETRAITE","CLINIQUE","TRIBUNAL","BRIGADE",
# "(L')?(')?INTERIEUR","MUNICIP(AL)?(ALES)?(AUX)?","DEPARTEMENT(AL)?(ALES)?(AUX)?"," NATION(AL)?(ALES)?(AUX)?",
# "ADMINISTRATION","ENSEIGNEMENT", "JUSTICE","SANTE"," COLLECTIVITE(S)?","TERRITORIA(L?)(UX?)(LES?)","EHPAD",
# "RECTORAT","INSPECTION","AMBULANCE","CHU","CHR","CASERNE")
# 
# rpfp<-rp %>%
# select('CABBI','RS_X', 'ACTET_C', 'ACTET_X', 'NOMVOI_X', 'NUMVOI_X', 'TYPEVOI_X',
# 'BISTER_X', 'CPLADR_X', 'CLT_X','DLT_X', 'PLT_X', 'DEPCOM_CODE',
# 'PROFS_X', 'SIRET_DEC','SIRETC','SIRETM','SourceXYW', 'x', 'y','qual','VARDOMPART_X','I_MCA_C','CLT_C_C') %>%
# filter(substr(ACTET_C,1,2) %in% c('84', '85', '86', '87', '88') |
# grepl(paste(paste0("(",liste_opq,")"), collapse='|'),RS_X) |
# grepl(paste(paste0("(",liste_opq,")"), collapse='|'),ACTET_X))
# 
# str(rpfp) # 563295 obs.
# 
# set.seed(12345)
# sub_rpfp<-rpfp[sample(nrow(rpfp),30000,replace=FALSE),]
# str(sub_rpfp)


################## remplacer le (x,y) quand il n'est pas bon
#on part d'une sous-table du rp sur le champ opq : sub_rpfp

#' Utilisation du plus proche voisin (dans base sirus) pour remplacer les (x,y) viciés
#' 
#' @param sub_rpfp Données rp (éventuellement sous-échantillon)
#' @param sirus Base de données sirus
#' 
#' @return Dataframe initial avec les (x,y) éventuellement recodés
#' 

ppv_xy <- function(sub_rpfp, sirus){
  
  rp_xy_mauvais<-sub_rpfp[sub_rpfp$qual %in% c(8.5,9)&!is.na(sub_rpfp$NOMVOI_X)& sub_rpfp$NOMVOI_X!=" " & sub_rpfp$NOMVOI_X!="",]
  str(rp_xy_mauvais) #6 132 observations
  
  table(sub_rpfp$qual ,!is.na(sub_rpfp$NOMVOI_X)& sub_rpfp$NOMVOI_X!=" " & sub_rpfp$NOMVOI_X!="")
  #pour 40 % des observations pour lesquelles qual=8.5 ou 9, la voie n'est pas du tout renseign?e
  
  adr<-rp_xy_mauvais$NOMVOI_X
  comm<-rp_xy_mauvais$CLT_C_C
  
  xy_ameliore<-lapply(1:nrow(rp_xy_mauvais), function(i) ({
  add <- try(get.banAPI(paste0(adr[i],'&','citycode=',comm[i]))$features[1,]$properties[1,c("x","y","name","citycode","score")])
  if (stringr::str_detect(add,"^Error")) data.frame(x=NA,y=NA,name=NA,citycode=NA,score=NA) else data.frame(add)
  }))
  
  vect.xy<-do.call(rbind,xy_ameliore)
  
  rp_xy_mauvais$x_ameliore<-vect.xy[,1]
  rp_xy_mauvais$y_ameliore<-vect.xy[,2]
  
  xy_ameliore<-rp_xy_mauvais[,c(1,ncol(rp_xy_mauvais)-1,ncol(rp_xy_mauvais))] 
  
  sub_rpfp_bis<-merge(sub_rpfp,xy_ameliore,all.x=TRUE,by.x="CABBI")
  str(sub_rpfp_bis)
  table(is.na(sub_rpfp_bis$x_ameliore))
  
  sub_rpfp_bis$xbis<-ifelse(sub_rpfp_bis$qual %in% c(8.5,9) &
  !is.na(sub_rpfp_bis$x_ameliore),sub_rpfp_bis$x_ameliore,sub_rpfp_bis$x)
  
  sub_rpfp_bis$ybis<-ifelse(sub_rpfp_bis$qual %in% c(8.5,9) &
  !is.na(sub_rpfp_bis$y_ameliore),sub_rpfp_bis$y_ameliore,sub_rpfp_bis$y)
  
  
  ######### On d?finit les sous-champs ?ducation/sant?/autres apu
  
  liste_85<-c("EDUCATION","LYCEE","ECOLE","CRECHE","GARDERIE","COLLEGE","UNIVERSIT(E|AIRE)","ENSEIGNEMENT")
  liste_86<-c("HOPITA(L|UX)","AP(-)?(^)?HP","HOSPITALIER(S)?","RETRAITE","CLINIQUE","SANTE","EHPAD","AMBULANCE","CHU","CHR")
  
  sub_rpfp_bis$groupe_85<-ifelse(grepl(paste(paste0("(",liste_85,")"), collapse='|'),sub_rpfp_bis$RS_X)|
  grepl(paste(paste0("(",liste_85,")"), collapse='|'),sub_rpfp_bis$ACTET_X)|substr(sub_rpfp_bis$ACTET_C,1,2) =='85',1,0)
  table(sub_rpfp_bis$groupe_85)
  
  sub_rpfp_bis$groupe_86<-ifelse(sub_rpfp_bis$groupe_85==0&(grepl(paste(paste0("(",liste_86,")"), collapse='|'),sub_rpfp_bis$RS_X)|
  grepl(paste(paste0("(",liste_86,")"), collapse='|'),sub_rpfp_bis$ACTET_X)|
  substr(sub_rpfp_bis$ACTET_C,1,2) %in% c('86','87','88')),1,0)
  table(sub_rpfp_bis$groupe_86)
  
  sub_rpfp_bis$groupe_84<-ifelse(sub_rpfp_bis$groupe_85==0&sub_rpfp_bis$groupe_86==0,1,0)
  table(sub_rpfp_bis$groupe_84)
  
  sub_rpfp_bis$com<-as.numeric(paste0(sub_rpfp_bis$CLT_C_C))
  
  
  #### On matche avec le plus proche voisin de sirus ? tour de r?le avec chacun des 3 groupes
  # sirus<-read.csv("C:/Users/Beatrice/Desktop/Hackathon-2018/DonneesTransmisesHackathon/sirus_o_p_q.csv",sep=";")
  
  sirus$nic2<-ifelse(sirus$nic/100<1,as.character(paste0("000",as.character(sirus$nic))),ifelse(sirus$nic/1000<1,paste0("00",as.character(sirus$nic)),
  ifelse(sirus$nic/10000<1,paste0("0",as.character(sirus$nic)),as.character(sirus$nic))))
  sirus$siret<-paste0(sirus$sirus_id,sirus$nic2)
  
  sirus$com<-as.numeric(paste0(sirus$adr_depcom))
  
  ###enseign : groupe_85
  
  sub_sirus<-droplevels(sirus[substr(sirus$apet,1,2)==85 & !is.na(sirus$com),])
  sub_rp<-sub_rpfp_bis[sub_rpfp_bis$groupe_85==1&!is.na(sub_rpfp_bis$com),] #7909 dans le groupe dont 7462 non NA
  
  sub_rp<-sub_rp[order(sub_rp$com),]
  
  vect<-unique(sub_rp$com)
  str(sub_rp)
  siret_imp<-c()
  for(k in vect){
  sub_rp2<-sub_rp[sub_rp$com==k,]
  sub_sirus2<-sub_sirus[sub_sirus$com==k,]
  add<-try(t(do.call(rbind,lapply(1:nrow(sub_rp2), function (j) 
  	sub_sirus2$siret[which.min(lapply(1:nrow(sub_sirus2),
   	function(i) sqrt((sub_rp2$x[j]-sub_sirus2$x[i])^2+(sub_rp2$y[j]-sub_sirus2$y[i])^2)))]))))
  if (length(add)==0){ 
  	siret_imp<-cbind(siret_imp,matrix(NA,ncol=nrow(sub_rp2),nrow=1))
  } 
  else{
  	siret_imp<-cbind(siret_imp,add)
  }
  }
  
  table(siret_imp==sub_rp$SIRET_DEC)[2]/table(is.na(sub_rp$SIRET_DEC))[1] 
  #sur ceux pour lesquels SIRET_DEC n'est pas manquant, on r?cup?re 37 % de r?ponses correctes
  
  sub_rp_85<-sub_rp
  sub_rp_85$siret_imp_85<-c(siret_imp)
  #table(sub_rp_85$siret_imp_85==sub_rp_85$SIRET_DEC)
  sub_rp_85b<-sub_rp_85[,c(1,ncol(sub_rp_85))]
  
  
  ###sant? social : groupe 86 (86 ? 88)
  
  sub_sirus<-droplevels(sirus[substr(sirus$apet,1,2) %in% c(86,87,88) & !is.na(sirus$com),])
  sub_rp<-sub_rpfp_bis[sub_rpfp_bis$groupe_86==1&!is.na(sub_rpfp_bis$com),] #13xxx dans le groupe dont 12361 non NA
  str(sub_rp)
  
  sub_rp<-sub_rp[order(sub_rp$com),]
  
  vect<-unique(sub_rp$com)
  str(sub_rp)
  siret_imp<-c()
  for(k in vect){
  sub_rp2<-sub_rp[sub_rp$com==k,]
  sub_sirus2<-sub_sirus[sub_sirus$com==k,]
  add<-try(t(do.call(rbind,lapply(1:nrow(sub_rp2), function (j) 
  	sub_sirus2$siret[which.min(lapply(1:nrow(sub_sirus2),
   	function(i) sqrt((sub_rp2$x[j]-sub_sirus2$x[i])^2+(sub_rp2$y[j]-sub_sirus2$y[i])^2)))]))))
  if (length(add)==0){ 
  	siret_imp<-cbind(siret_imp,matrix(NA,ncol=nrow(sub_rp2),nrow=1))
  } 
  else{
  	siret_imp<-cbind(siret_imp,add)
  }
  }
  
  siret_imp2<-cbind(siret_imp,NA)
  # table(siret_imp2==sub_rp$SIRET_DEC)[2]/table(is.na(sub_rp$SIRET_DEC))[1] 
  #sur ceux pour lesquels SIRET_DEC n'est pas manquant, on r?cup?re 22 % de r?ponses correctes
  
  sub_rp_86<-sub_rp
  sub_rp_86$siret_imp_86<-c(siret_imp2)
  #table(sub_rp_86$siret_imp_86==sub_rp_86$SIRET_DEC)
  sub_rp_86b<-sub_rp_86[,c(1,ncol(sub_rp_86))]
  
  ###autres apu : groupe 84
  
  sub_sirus<-droplevels(sirus[substr(sirus$apet,1,2) ==84 & !is.na(sirus$com),])
  sub_rp<-sub_rpfp_bis[sub_rpfp_bis$groupe_84==1&!is.na(sub_rpfp_bis$com),] #xxxx dans le groupe dont xx non NA
  
  sub_rp<-sub_rp[order(sub_rp$com),]
  
  vect<-unique(sub_rp$com)
  str(sub_rp)
  siret_imp<-c()
  for(k in vect){
  sub_rp2<-sub_rp[sub_rp$com==k,]
  sub_sirus2<-sub_sirus[sub_sirus$com==k,]
  add<-try(t(do.call(rbind,lapply(1:nrow(sub_rp2), function (j) 
  	sub_sirus2$siret[which.min(lapply(1:nrow(sub_sirus2),
   	function(i) sqrt((sub_rp2$x[j]-sub_sirus2$x[i])^2+(sub_rp2$y[j]-sub_sirus2$y[i])^2)))]))))
  if (length(add)==0){ 
  	siret_imp<-cbind(siret_imp,matrix(NA,ncol=nrow(sub_rp2),nrow=1))
  } 
  else{
  	siret_imp<-cbind(siret_imp,add)
  }
  }
  return(siret_imp)
}

# table(siret_imp==sub_rp$SIRET_DEC)[2]/table(is.na(sub_rp$SIRET_DEC))[1] 
# #sur ceux pour lesquels SIRET_DEC n'est pas manquant, on r?cup?re 35 % de r?ponses correctes
# 
# sub_rp_84<-sub_rp
# sub_rp_84$siret_imp_84<-c(siret_imp)
# #table(sub_rp_84$siret_imp_84==sub_rp_84$SIRET_DEC)
# sub_rp_84b<-sub_rp_84[,c(1,ncol(sub_rp_84))]
# 
# 
# ###on remerge les tables
# merge1<-merge(sub_rpfp_bis,sub_rp_85b,all.x=TRUE,by="CABBI")
# merge2<-merge(merge1,sub_rp_86b,all.x=TRUE,by="CABBI")
# sub_rpfp_fin<-merge(merge2,sub_rp_84b,all.x=TRUE,by="CABBI")
# str(sub_rpfp_fin)
# 
# sub_rpfp_fin$siret_xy<-ifelse(!is.na(sub_rpfp_fin$siret_imp_85),sub_rpfp_fin$siret_imp_85,
# 	ifelse(!is.na(sub_rpfp_fin$siret_imp_86),sub_rpfp_fin$siret_imp_86,sub_rpfp_fin$siret_imp_84))
# 
# 
# table(sub_rpfp_fin$siret_xy==sub_rpfp_fin$SIRET_DEC)/nrow(sub_rpfp_fin)
# table(sub_rpfp_fin$siret_xy==sub_rpfp_fin$SIRETM)/nrow(sub_rpfp_fin)












