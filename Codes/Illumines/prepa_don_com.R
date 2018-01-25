library(data.table)

setwd("/Users/ingacarrelet/Hackathon-2018-master/illumines/tables")

##### import des tables du RP ####
rp14<-fread("rp_final_2014.csv",stringsAsFactors=FALSE)
rp17<-fread("rp_final_2017.csv",stringsAsFactors=FALSE)

###### a chaque code commune insee on associe son code region (ancienne codification 22 regions)
don_cog<-read.csv("communes.csv",sep=";")
dep_reg<-read.csv("dep_reg.csv",sep=";")

dep_reg_uniq <-  dep_reg[!duplicated(dep_reg), ]


don_com_reg<-merge(don_cog,dep_reg_uniq,by="DEP",all.x=TRUE) # table qui associe pour cahaque commune, la region et le departement correspondant

##### appariement aux RP pour recuperer region ######
rp14_reg <- merge(rp14,don_com_reg,by.x = "DEPCOM_CODE",by.y = "CODGEO",all.x = TRUE)
rp17_reg <- merge(rp17,don_com_reg,by.x = "DEPCOM_CODE",by.y = "CODGEO",all.x = TRUE)


write.csv(rp14_reg,"rp14_reg.csv")
write.csv(rp17_reg,"rp17_reg.csv")



