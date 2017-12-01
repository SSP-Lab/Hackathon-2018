####
# siret_q.R
#
# Fonction outil pour faciliter la recherche
# multicritères sur l'API-Sirene 
# (cf. documentation API-Sirene p. 25-29)
#
# Martin Chevalier (Insee-DMS)
# 01/12/2017
# Version 0.1
####


# Impératif depuis l'Insee
# file.edit('~/.Renviron')
# http_proxy=proxy-rie.http.insee.fr:8080
# https_proxy=proxy-rie.http.insee.fr:8080

# Fonction siret_q() : effectuer une recherche multicritère Siret
# - q (OBLIGATOIRE) : requête multicritère (chaîne de caractère de longueur 1)
# - verbose : afficher ou non la progresion, TRUE par défaut
# - nombre : taille de la page, ie nombre d'établissements à récupérer
# en une requête (cf. documentation API-Sirene p. 29), 100 par défaut
# - url : URL du service API-Sirene, https://prototype.api-sirene.insee.fr/ws/siret
# par défaut

siret_q <- function(
  q
  , verbose = TRUE
  , nombre = 100
  , url = "https://prototype.api-sirene.insee.fr/ws/siret"
){

  # Etape 0 : Packages requis
  require(httr)
  require(jsonlite)
  
  # Etape 1 : Envoi des requêtes sur l'API et conversion en liste R
  Curseur <- ""; CurseurSuivant <- "*"; resultat <- list(); num_requete <- 1
  if(verbose) message("Envoi des requêtes (chacune récupère ", nombre, " observations) : ", appendLF = FALSE)
  while(CurseurSuivant != Curseur){
    if(verbose) message(if(num_requete %% 10 > 0) "." else num_requete, appendLF = FALSE)
    requete <- paste0(url, "?q=", q, "&curseur=", CurseurSuivant, "&nombre=", nombre)
    reponse_json <- GET(requete)
    reponse_list <- fromJSON(content(reponse_json, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    resultat[[length(resultat) + 1]] <- reponse_list
    Curseur <- reponse_list$Header$Curseur
    CurseurSuivant <- reponse_list$Header$CurseurSuivant
    if(CurseurSuivant == Curseur){if(verbose & num_requete %% 10 > 0) message(num_requete)} else 
      num_requete <- num_requete + 1
  }
  cat("\n")

  # Etape 2 : Mise en forme dans un data.frame

  # Etape 2.1 : Mise à plat des listes par siret
  out <- do.call(base::c, lapply(resultat, `[[`, "Etablissements"))
  out <- lapply(out, function(x) lapply(
    c(x[1:5], x[["UniteLegale"]], x[["Adresse"]], x[["Periodes"]][[1]])
    # NOTE: présuppose une seule période
    , function(x) if(is.null(x)) NA else x
  ))

  # Etape 2.2 Traitement des exceptions
  if(any(sapply(out, function(x) length(setdiff(c("UnitePurgee", "Doublon"), names(x))) > 0))){
    for(var in c("UnitePurgee", "Doublon")){
      out <- lapply(out, function(x){
        if(!(var %in% names(x))) c(x, setNames(list(NA), var)) else{
          c(x[setdiff(names(x), var)], x[var])
        }
      })
    }
  }
  
  # Etape 2.3 : Permutation des dimensions de la liste 
  # et transformation en data.frame
  out <- as.data.frame(lapply(
    setNames(seq_along(out[[1]]), names(out[[1]]))
    , function(i) sapply(out, `[[`, i)
  ), stringsAsFactors = FALSE)
  
  return(out)
  
}

# Reprise de l'exemple du tutoriel proposé à l'adresse
# https://github.com/SSP-Lab/Hackathon-2018/blob/master/outils/API%20Sirene/r-nb-tuto-api-sirene.Rmd
# df <- siret_q("Denomination:notaire")
# str(df)
