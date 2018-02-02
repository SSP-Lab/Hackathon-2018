library(dplyr)
library(ggplot2)
library(foreach)
library(tm)
library(stringdist)

source(paste0(getwd(),"/R/getGEOAPI.R"),encoding = "UTF-8")
source(paste0(getwd(),"/R/api_siret.R"),encoding = "UTF-8")
source(paste0(getwd(),"/R/check_validation.R"),encoding = "UTF-8")


# =====================================
# IMPORT ET NETTOYAGE DES DONNEES
# =====================================



# IMPORT DATA
df_rp2017 <- readr::read_csv2(paste0(getwd(),"/Dossier fourni/Hackathon-2018/données/DonneesTransmisesHackathon/rp_final_2017.csv"))

# KEEP RELEVENT INFORMATION
df_rp2017 <- df_rp2017 %>%
  select('RS_X', 'CABBI', 'ACTET_C', 'ACTET_X', 'NOMVOI_X', 'NUMVOI_X', 'TYPEVOI_X',
         'BISTER_X', 'CPLADR_X', 'CLT_X','DLT_X', 'CLT_C_C', 'PLT_X', 'DEPCOM_CODE',
         'PROFS_X', 'SIRET_DEC', 'x', 'y',"SourceXYW",'qual') %>%
  filter(substr(ACTET_C,1,2) %in% c('84', '85', '86', '87', '88') |
           grepl("POMPIERS(?)",RS_X) | grepl("MINISTERE",RS_X) | grepl("EDUCATION",RS_X) |
           grepl("LYCEE",RS_X) | grepl("ECOLE",RS_X) | grepl("CRECHE",RS_X) |
           grepl("GARDERIE",RS_X) | grepl("PUBLI(QUE|C)S? ",RS_X) | grepl("HOPITA(L|UX)",RS_X) |
           grepl("AP(-)?(^)?HP",RS_X) | grepl("HOSPITALIER(S)?",RS_X) | grepl("POLICE",RS_X) |
           grepl("GENDARMERIE",RS_X) | grepl("FONCTIONNAIRE",RS_X) | grepl("ETAT",RS_X) |
           grepl("DEPARTEMENT(AL)?(AUX)?",RS_X) | grepl("MAIRIE",RS_X) |
           grepl("PREFECT(URE)?(ORAL)?(ORAUX)?",RS_X) | grepl("TRIBUNAL",RS_X) |
           grepl("BRIGADE",RS_X) | grepl("(L')?(')?INTERIEUR",RS_X) |
           grepl("MUNICIP(AL)?(ALES)?(AUX)?",RS_X) | grepl("DEPARTEMENT(AL)?(ALES)?(AUX)?",RS_X) |
           grepl(" NATION(AL)?(ALES)?(AUX)?",RS_X) | grepl("ADMINISTRATION", RS_X) |
           grepl(" COLLECTIVITE(S)?",RS_X) | grepl("TERRITORIA(L?)(UX?)(LES?)", RS_X) |
           grepl("POMPIERS(?)",ACTET_X) | grepl("MINISTERE",ACTET_X) | grepl("EDUCATION",ACTET_X) |
           grepl("LYCEE",ACTET_X) | grepl("ECOLE",ACTET_X) | grepl("CRECHE",ACTET_X) |
           grepl("GARDERIE",ACTET_X) | grepl("PUBLI(QUE|C)S? ",ACTET_X) | grepl("HOPITA(L|UX)",ACTET_X) |
           grepl("AP(-)?(^)?HP",ACTET_X) | grepl("HOSPITALIER(S)?",ACTET_X) | grepl("POLICE",ACTET_X) |
           grepl("GENDARMERIE",ACTET_X) | grepl("FONCTIONNAIRE",ACTET_X) | grepl("ETAT",ACTET_X) |
           grepl("DEPARTEMENT(AL)?(AUX)?",ACTET_X) | grepl("MAIRIE",ACTET_X) |
           grepl("PREFECT(URE)?(ORAL)?(ORAUX)?",ACTET_X) | grepl("TRIBUNAL",ACTET_X) |
           grepl("BRIGADE",ACTET_X) | grepl("(L')?(')?INTERIEUR",ACTET_X) |
           grepl("MUNICIP(AL)?(ALES)?(AUX)?",ACTET_X) | grepl("DEPARTEMENT(AL)?(ALES)?(AUX)?",ACTET_X) |
           grepl(" NATION(AL)?(ALES)?(AUX)?",ACTET_X) | grepl("ADMINISTRATION", ACTET_X) |
           grepl("ENSEIGNEMENT", ACTET_X) | grepl("JUSTICE", ACTET_X) | grepl("SANTE", ACTET_X) |
           grepl(" COLLECTIVITE(S)?",RS_X) | grepl("TERRITORIA(L?)(UX?)(LES?)", RS_X)) 


# +++++++++++++++++++++++++++++++++++++++
# RAISON SOCIALE NETTOYEE DES ERREURS
# +++++++++++++++++++++++++++++++++++++++

liste.words <- c("MINISTERE DE L'INTERIEUR","MINISTERE DE LA DEFENSE","EDUCATION NATIONALE",
           "GENDARMERIE NATIONALE","MINISTERE DE LA JUSTICE")

modif <- tm::removeWords(df_rp2017$RS_X,liste.words)
df_rp2017$old_RSX <- df_rp2017$RS_X
df_rp2017$RS_X <- sapply(1:length(modif), function(i)({
  if (modif[i] == "" & !is.na(modif[i])) df_rp2017$RS_X[i] else modif[i]
}))



# +++++++++++++++++++++++++++++++++++
# RAISON SOCIALE REMPLACEE QUAND
# NOM VOIE EST UNE RAISON SOCIALE
# +++++++++++++++++++++++++++++++++++

listemots <- c("POMPIERS(?)", "MINISTERE",
               "LYCEE", "ECOLE", "CRECHE", "COLLEGE",
               "GARDERIE", "HOPITA(L|UX)",
               "AP(-)?(^)?HP", "HOSPITALIER(S)?", "POLICE",
               "GENDARMERIE", "MAIRIE",
               "PREFECTURE", "TRIBUNAL", "RETRAITE",
               "BRIGADE", "CLINIQUE", "BASE",
               "COLLECTIVITE")
listemots.exclus <- c("RUE","AVENUE","ROUTE","PLACE")

# SUBSTITUE RAISON SOCIALE
df_rp2017$RS_X[
  grepl(paste(paste0("(",listemots,")"), collapse='|'),df_rp2017$NOMVOI_X) &
    !grepl(paste(listemots.exclus, collapse='|'), df_rp2017$NOMVOI_X) &
    is.na(df_rp2017$TYPEVOI_X)
  ] <- df_rp2017$NOMVOI_X[
    grepl(paste(paste0("(",listemots,")"), collapse='|'),df_rp2017$NOMVOI_X) &
      !grepl(paste(listemots.exclus, collapse='|'), df_rp2017$NOMVOI_X) &
      is.na(df_rp2017$TYPEVOI_X)
    ]

# TEST: 1000 PREMIERS CAS
df_rp2017 <- df_rp2017 %>% filter(RS_X != "") %>% .[1:1000,]

# METTRE QUELQUE PART
# filter(is.na(VARDOMPART_X)) & qual ==9)


# ====================================
#     ETAPE I/ API SIRENE ADDOK
# ====================================

# ++++++++++++++++++++++++++++
# COMMUNE & RAISON SOCIALE
# ++++++++++++++++++++++++++++

# INITIALISATION DES CLUSTERS
cl <- parallel::makeCluster(parallel::detectCores()-1, outfile = "")
doParallel::registerDoParallel(cl)

# APPELS PARALLELISES DE LA FONCTION get.banAPI
result <- foreach(i = 1:nrow(df.special), .combine = "list",
                  .multicombine = TRUE,
                  .maxcombine = nrow(df.special),
                  .errorhandling = "pass", .export = 'get.banAPI',
                  .packages = c("jsonlite", "httr")) %dopar% {
                    return(
                      get.banAPI(
                        paste0(df.special$RS_X,"&citycode=",df.special$CLT_C_C)[i], geo.place = T
                      )$features[1,]$properties
                    )
                  }

# STOP LES CLUSTERS
parallel::stopCluster(cl)

result <- lapply(1:length(result), function(i)({
  if (stringr::str_detect(result[[i]], "^Error")){
    result <- rep(NA,5)
  } else{
    result <- data.frame(result[[i]]) %>% select_(.dots = c("citycode", "score","id","context","name"))
  }
})
)

result <- tbl_df(do.call(rbind,result))
df <- tbl_df(cbind(result,SIRET_DEC = df.special$SIRET_DEC))

check_validation(df)

df <- df %>% dplyr::mutate(siret = ifelse(result$score > 0.5, id, NA)) %>%
  dplyr::mutate(siret.imput = ifelse(score > 0.5, 1, NA)) %>% select(siret,siret.imput)

#sum(!is.na(df$siret))

df <- tbl_df(cbind(df_rp2017,df))



# ====================================
# ETAPE II/ DISTANCE TEXTUELLE
# ====================================


# IMPORT DATA
sirus_o_p_q <- readr::read_csv2(paste0(getwd(), "/Dossier fourni/Hackathon-2018/sirus_o_p_q.csv")) %>%
  select_(.dots = c("sirus_id", "nic", "apet",
                    "adr_depcom","denom"))


#On concatene siren et nic dans la base sirus
sirus_o_p_q$nic2<-ifelse(sirus_o_p_q$nic/100<1,as.character(paste0("000",as.character(sirus_o_p_q$nic))),ifelse(sirus_o_p_q$nic/1000<1,paste0("00",as.character(sirus_o_p_q$nic)),
                                                                                                                ifelse(sirus_o_p_q$nic/10000<1,paste0("0",as.character(sirus_o_p_q$nic)),as.character(sirus_o_p_q$nic))))
sirus_o_p_q$siret<-paste0(sirus_o_p_q$sirus_id,sirus_o_p_q$nic2)


#On liste les codes communes
adresse = df$DEPCOM_CODE
vect <- data.frame(codeinsee = df$DEPCOM_CODE) %>% arrange(codeinsee) %>%
              filter(codeinsee!="") %>%
              mutate(v=as.character(codeinsee)) %>%
              select(v) %>% unique(.)
liste<-as.vector(vect$v)


#On charge la base des communes adjacentes
communes_adjacentes_2017 <- readr::read_csv(paste0(getwd(),
                                                    "/Dossier fourni/Hackathon-2018/données/communes_adjacentes_2017.csv"))

#on identifie pour chaque commune la liste de ses voisines et elle-meme
vector_voisins <- sapply(communes_adjacentes_2017$insee_voisins, function(y)strsplit(y, "|", fixed = TRUE))
adj<-function(insee)return(c(insee,vector_voisins[[which(communes_adjacentes_2017$insee==insee)]]))


#On recupere le siret de l'établissement qui minimise la distance cosinus
siret_imp<-c()
for(k in liste){
  
  sub_rp <- adresse[adresse==k]
  sub_sirus<-sirus_o_p_q[sirus_o_p_q$adr_depcom %in% adj(k),]
  
  siret_add<-try(t(do.call(rbind,lapply(1:length(sub_rp), function (j)({
    
    distances <- lapply(1:nrow(sub_sirus), function(i) stringdist( df$RS_X[j] , sub_sirus$denom[i] , method="cosine"))
    
    distances <- do.call(rbind, distances)
    siret2 <- sub_sirus$siret[which.min(distances)]
    if (distances[which.min(distances)]>0.5) NA else siret2
         
  })))))
  
  if (length(siret_add)==0){
    siret_imp<-cbind(siret_imp, matrix(NA,ncol=length(sub_rp), nrow=1))
  } else{
    siret_imp<-cbind(siret_imp,siret_add)
  }
  
}




siret_imp <- as.character(siret_imp)


#df2 <- df
#df2$siret.imput[is.na(df2$siret) & !is.na(siret_imp)] <- 2
#df2$siret[is.na(df2$siret) & !is.na(siret_imp)] <- paste0(
#  substr(siret_imp[is.na(df2$siret) & !is.na(siret_imp)],1,9), substr(siret_imp[is.na(df2$siret) & !is.na(siret_imp)],11,15)
#)
  
sum(!is.na(df2$siret))


# BILAN ETAPE 2
mean(df2$siret.imput ==2,na.rm=T)
truc = df2[df2$siret.imput ==2,]
mean(as.character(truc$SIRET_DEC) == as.character(truc$siret),na.rm = T)

# BILAN ETAPE 1 + ETAPE 2
mean(df2$siret.imput %in% c(1,2),na.rm=T)
truc = df2[df2$siret.imput %in% c(1,2),]
mean(as.character(truc$SIRET_DEC) == as.character(truc$siret),na.rm = T)




# ====================================
#     ETAPE II/ API SIRENE
# ====================================

ligne <- df %>% filter(is.na(siret)) %>%
  select_(.dots = c('CABBI', 'RS_X', 'NOMVOI_X', 'NUMVOI_X', 'TYPEVOI_X',
                                  'BISTER_X', 'CLT_X', 'CLT_C_C'))
ligne[1:3,]
ligne<- apply(ligne, 2, function(x) as.character(x)) 
ligne<- unname(ligne)

cl <- parallel::makeCluster(parallel::detectCores()-1, outfile = "")
doParallel::registerDoParallel(cl)

result <- foreach(i = 1:nrow(ligne), .combine = "list",
                  .multicombine = TRUE,
                  .maxcombine = nrow(ligne),
                  .errorhandling = "pass", .export = 'api_siret',
                  .packages = c("jsonlite", "httr")) %dopar% {
                    return(
                      data.frame(api_siret(as.character(ligne[i,])))[1,]
                    )
                  }

parallel::stopCluster(cl)

result <- do.call(rbind,result)


df2 <- df
df2$siret.imput[is.na(df2$siret)] <- ifelse(!is.na(result$siren) & !is.na(result$siren),
                                      2, NA)
df2$siret[is.na(df2$siret)] <- ifelse(!is.na(result$siren) & !is.na(result$siren),
                                      paste0(result$siren,result$nic), df2$siret)

#sum(!is.na(df2$siret))


# BILAN ETAPE 2
mean(df2$siret.imput ==2,na.rm=T)
truc = df2[df2$siret.imput ==2,]
mean(as.character(truc$SIRET_DEC) == as.character(truc$siret),na.rm = T)

# BILAN ETAPE 1 + ETAPE 2
mean(df2$siret.imput %in% c(1,2),na.rm=T)
truc = df2[df2$siret.imput %in% c(1,2),]
mean(as.character(truc$SIRET_DEC) == as.character(truc$siret),na.rm = T)


# LE DATAFRAME UTILISE POUR HACKATHON
savedf <- df %>% select(CABBI,siret) %>% filter(!is.na(siret))
readr::write_delim(savedf, "hackathon.csv", delim = ";")

# ===========================
# API BAN
# ===========================
test = df_rp2017[1:300,]


cl <- parallel::makeCluster(parallel::detectCores()-1, outfile = "")
doParallel::registerDoParallel(cl)

result <- foreach(i = 1:nrow(test), .combine = "list",
                  .multicombine = TRUE,
                  .maxcombine = nrow(test),
                  .errorhandling = "pass", .export = 'get.banAPI',
                  .packages = c("jsonlite", "httr")) %dopar% {
                    return(
                      get.banAPI(
                        paste0(test$NOMVOI_X,"&citycode=",test$CLT_C_C)[i]
                      )$features[1,]$properties
                    ) 
                  }

parallel::stopCluster(cl)


result <- lapply(1:length(result), function(i)({
  if (stringr::str_detect(result[[i]], "^Error")){
    result <- rep(NA,5)
  } else{
    result <- data.frame(result[[i]]) %>% select_(.dots = c("x", "y", "postcode", "name",
                                                            "score"))
  }
})
)

result <- tbl_df(do.call(rbind,result))


localization <- lapply(1:100, function(i)({
  result <- try(get.banAPI(
    paste0(test$NOMVOI_X,"&citycode=",test$CLT_C_C)[i]
    )$features[1,]$properties)
  if (stringr::str_detect(result, "^Error")){
      result <- rep(NA,5)
    } else{
      result <- result %>% select_(.dots = c("x", "y", "postcode", "name",
                                             "score"))
    }
}))

df.ban <- cbind(do.call(rbind,localization),test[1:100,c("x","y","NOMVOI_X","CLT_C_C")])

ggplot(data = df.ban) + geom_histogram(aes(x = score, y=..density..))

colnames(df.ban)[which(colnames(df.ban) %in% c("x","y"))[3:4]] <- c("old_x","old_y")
df.ban <- df.ban %>% dplyr::mutate(dist = sqrt((x-old_x)^2 + (y-old_y)^2))

ggplot(data = df.ban) + geom_histogram(aes(x = dist, y=..density..)) + xlim(0,10000)





source(paste0(getwd(),"/R/api_siret.R"))

cl <- parallel::makeCluster(parallel::detectCores()-1, outfile = "")
doParallel::registerDoParallel(cl)

result <- foreach(i = 1:1000, .combine = "list",
                  .multicombine = TRUE,
                  .errorhandling = "pass", .export = 'get.banAPI',
                  .packages = c("jsonlite", "httr")) %dopar% {
                    return(
                      get.banAPI(
                        paste0(df.special$NOMVOI_X,"&citycode=",df.special$CLT_C_C)[i], geo.place = T
                      )$features[1,]$properties
                    )
                  }

parallel::stopCluster(cl)




# ===========================
# API BAN
# ===========================
test = df_rp2017[1:300,]


cl <- parallel::makeCluster(parallel::detectCores()-1, outfile = "")
doParallel::registerDoParallel(cl)

result <- foreach(i = 1:nrow(test), .combine = "list",
                  .multicombine = TRUE,
                  .maxcombine = nrow(test),
                  .errorhandling = "pass", .export = 'get.banAPI',
                  .packages = c("jsonlite", "httr")) %dopar% {
                    return(
                      get.banAPI(
                        paste0(test$NOMVOI_X,"&citycode=",test$CLT_C_C)[i]
                      )$features[1,]$properties
                    ) 
                  }

parallel::stopCluster(cl)


result <- lapply(1:length(result), function(i)({
  if (stringr::str_detect(result[[i]], "^Error")){
    result <- rep(NA,5)
  } else{
    result <- data.frame(result[[i]]) %>% select_(.dots = c("x", "y", "postcode", "name",
                                                            "score"))
  }
})
)

result <- tbl_df(do.call(rbind,result))


# =========================
# PLUS PROCHES VOISINS
# =========================

df3 <- df
result = ppv_xy(df3,sirus_o_p_q)

df3$siret.imput[is.na(df3$siret)] <- ifelse(!is.na(result$siren) & !is.na(result$siren),
                                            2, NA)
df3$siret[is.na(df3$siret)] <- ifelse(!is.na(result$siren) & !is.na(result$siren),
                                      paste0(result$siren,result$nic), df2$siret)



