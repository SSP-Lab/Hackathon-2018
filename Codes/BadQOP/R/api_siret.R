#' Call API Sirene to get siret
#' 
#' @param df A row (to use a dataframe see parallelization used in
#' README) with following information, between parenthesis which
#' column it should be found: Code Insee (8), Numéro voie (4),
#' Nom voie (3), Type de voie (5)
#' @return Result of Sirene API with siret and nic 

api_siret<-function(df){
  
  url <- "https://prototype.api-sirene.insee.fr/ws/siret"
  
  df <- unname(df)

  query <- paste('q=CodeCommuneEtablissement:',df[8],
                  ' AND NumeroVoieEtablissement:',as.numeric(df[4]),
                  ' AND LibelleVoieEtablissement:"',df[3],'"',
                  ' AND TypeVoieEtablissement:', df[5],
                  ' AND EtatAdministratif:A', sep="")
  
  reponse <- httr::GET(url=url,query=URLencode(query))
  
  if(httr::status_code(reponse)==200){
    
    data<-as.data.frame(jsonlite::fromJSON(httr::content(reponse,"text")))
    A<-data[,"Etablissements.UniteLegale"]
    B<-data[,"Etablissements.Adresse"]
    data2<-data.frame(v1 = df[1], data[, c("Header.Statut",
                                    "Header.Total",
                                    "Etablissements.Siren",
                                    "Etablissements.Nic")],A$Denomination, A$ActivitePrincipale,
                 A$TrancheEffectifsUniteLegale,
                 B$NumeroVoieEtablissement, B$IndiceRepetitionEtablissement,
                 B$TypeVoieEtablissement, B$LibelleVoieEtablissement,
                 B$LibelleCommuneEtablissement, B$CodeCommuneEtablissement)
    colnames(data2)<-c("v1", "v2", "v3", "siren", "nic", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13","v14")
    
  }
  else{
    data2 <- data.frame(v1 = as.character(df[1]), matrix(NA, ncol = 13, nrow = 1))
    colnames(data2)<-c("v1", "v2", "v3", "siren", "nic", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13","v14")
  }
  return(data.frame(data2))
}
