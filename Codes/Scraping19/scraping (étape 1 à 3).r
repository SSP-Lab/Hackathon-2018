library('stringr')
library('jsonlite')
library('dplyr')
library('tidyr')
library('httr')

#Conversion code NAF en ajoutant le . au milieu
str_to_naf <- function(x) { return(paste0(str_sub(x,1,2),'.',str_sub(x,3,5))) }
#Test d'un dataframe pour voir s'il est null ou vide
is_vide <- function(x) { if (is.null(x)) return (TRUE) else if (nrow(df) ==0) return (TRUE) else return (FALSE) }

#Création d'un df pour contenir le résultat du rapprochement Sirene RP
response <- data.frame(CABBI = character(0), siret1 = character(0), siret2 = character(0), siret3 = character(0),
										     siret1_i = character(0), siret2_i =character(0), siret3_i = character(0),
										     siret1_d = character(0), siret2_d =character(0), siret3_d = character(0))

sirus <- read.csv("sirus_2017.csv", header = TRUE, sep= ";", encoding="UTF-8")
data_rp_init <- read.csv("rp_final_2017.csv", header = TRUE, sep= ";", encoding="UTF-8")

#Filtre des données pour identifier celles qui sont complètes et exploitables
data_rp_ok <- data_rp_init %>%
          filter(!(CLT_C_C == "") & !(RS_X == "") & !(ACTET_C_C == "") & !(NOMVOI_X == ""))

#Extraction des premières lignes pour limiter la taille du traitement
data_rp <- data_rp_ok [1:1000,]
#data_rp <- data_rp_ok [1001:20000,]

for (z in 201:300) {

	#Construction d'une chaine de caractère contenant des informations intéressantes du RP à croiser avec celles de SIRENE
	ch1_old <- paste0(data_rp$ACTET_X[z], " ", data_rp$RS_X[z], " ", data_rp$NOMVOI_X[z]," ", data_rp$PROFI_X[z]," ", data_rp$PROFS_X[z])
	ch1 <- strsplit(as.character(ch1_old)," ")
	ch1 <- ch1[[1]][ch1[[1]] != "DE"]
	
	#Recherche d'échos possibles dans SIRET : l'idée est d'augmenter le périmètre des recherche en fonction du nb d'échos précédents
	df <- rechercher_siret(paste0("CodeCommuneEtablissement:", data_rp$CLT_C_C[z], " AND Denomination:\"", data_rp$RS_X[z], "\""))
	if (!is.null(df)) df <- df %>%
			  filter(!(EtatAdministratifEtablissement == "F"))
	if (is_vide(df)) df <- rechercher_siret(paste0("CodeCommuneEtablissement:", data_rp$CLT_C_C[z], " AND Denomination.phonetisation:\"", data_rp$RS_X[z], "\""))
	if (!is.null(df)) df <- df %>%
			  filter(!(EtatAdministratifEtablissement == "F"))
	if (is_vide(df)) {
		df2 <- rechercher_siret(paste0("CodeCommuneEtablissement:", data_rp$CLT_C_C[z], " AND ActivitePrincipale:\"", str_to_naf(data_rp$ACTET_C_C[z]), "\""))
		df3 <- rechercher_siret(paste0("CodeCommuneEtablissement:", data_rp$CLT_C_C[z], " AND LibelleVoieEtablissement:\"", data_rp$NOMVOI_X[z], "\""))
		df <- rbind (df, df2, df3)
	}
	print (paste0(z, " ", data_rp$CABBI[z], " ", data_rp$RS_X[z]))
	
	#Suppression des SIRET sans activité
	if (!is.null(df)) if (nrow(df)>0) {
			df <- df %>%
			  filter(!(EtatAdministratifEtablissement == "F"))
		print(nrow(df))
		
		#Création du SIRET à partir des données SIREN & NIC
		df <- unite(df, Siret, 1:2, sep="", remove=FALSE) 

		#Initialisationd de deux indicateurs de "proximité" pour sélectionner les meilleurs SIRET
		df$MATCH_I <- 1 / nrow(df)
		df$MATCH_DIST_V <- 1 / nrow(df)
		
		if (nrow(df)>1) {
			for (i in 1:nrow(df)) {
				
				#Calcul distance de Levenshtein sur les lbellés de voies
				dist_voie <- stringdist(data_rp$NOMVOI_X[z],df$LibelleVoieEtablissement[i])
				
				#Calcul distance sur des informations collectées entre le RP & SIRENE : nb de mots communs ! à amélorier
				ch2_old <- paste0(df$LibelleActiviteEtablissement[i], " ", df$LibelleVoieEtablissement[i], " ", df$Nom[i], " ", df$Sigle[i], " ", df$ActivitePrincipale[i], " ", df$ComplementAdresseEtablissement[i], " ", df$Enseigne1[i])
				ch2 <- strsplit(str_to_upper(as.character(chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), ch2_old)))," ")
				ch2 <- ch2[[1]][ch2[[1]] != "DE"]

				nb <- 0
				for (j in 1:length(ch1)) nb = nb + sum(str_detect(ch1[j], ch2))
				df$MATCH_I[i] <- nb
				df$MATCH_DIST_V[i] <- dist_voie

				}
			}
		#Tri
		df <- df[order(df$MATCH_I,decreasing=T),]
		x <- df[order(df$MATCH_DIST_V,decreasing=F),]
		#Ajout des 3 meilleurs echos dans un df avec le CABBI comme id
		response <- rbind(response, data.frame(CABBI = as.character(data_rp$CABBI[z]), siret1 = as.character(x [1, c('Siret')]), siret2=as.character(x [2, c('Siret')]), siret3=as.character(x [3, c('Siret')]), siret1_i = as.character(x [1, c('MATCH_I')]), siret2_i = as.character(x [2, c('MATCH_I')]), siret3_i = as.character(x [3, c('MATCH_I')]), siret1_d = as.character(x [1, c('MATCH_DIST_V')]), siret2_d = as.character(x [2, c('MATCH_DIST_V')]), siret3_d = as.character(x [3, c('MATCH_DIST_V')])))
		}
	else {
		response <- rbind(response, data.frame(CABBI = as.character(data_rp$CABBI[z]), siret1 = '', siret2='', siret3='', siret1_i = '', siret2_i ='', siret3_i = '', siret1_d = '', siret2_d = '', siret3_d = ''))
	}
}

#Export des résultats
write.csv2(response, "scrap19-03.csv", na="", row.names = FALSE, quote = FALSE)

