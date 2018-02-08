Hackathon
================
Bad QOP
24 janvier 2018

Ce document présente les principales parties du code utilisé dans le document `main.R` qui nous a permis de faire notre codification automatique. Les librairies nécessaires sont les suivantes:

``` r
library(dplyr)
library(ggplot2)
library(foreach)
library(tm)
library(stringdist)
library(curl)
library(httr)
library(sp)
```

et les fonctions `R` appelées au cours du code sont les suivantes

``` r
source(paste0(getwd(),"/R/getGEOAPI.R"),encoding = "UTF-8")
source(paste0(getwd(),"/R/api_siret.R"),encoding = "UTF-8")
source(paste0(getwd(),"/R/check_validation.R"),encoding = "UTF-8")
source(paste0(getwd(),"/R/ppv_xy.R"),encoding = "UTF-8")
```

Ces documents ont une documentation `roxygen` pour aider à leur compréhension.

Les fichiers `csv` ne sont pas importés dans le repo afin de ne pas surcharger l'espace de stockage. Le document `script.R` montre quelle a été la ligne de code nécessaire pour importer ces fichiers. Ce `markdown` prend comme donnée l'importation des fichiers, peu importe le chemin.

Etape 0: Sélection des individus potentiellement concernés par les secteurs O,P,Q
=================================================================================

Base `rp`
---------

On suppose que le dataframe `rp_final_2017.csv` est importé sous le nom `df_rp2017` (par example via `readr::read_csv2`). Pour sélectionner nos individus, on utilise des expressions régulières qui ressortent généralement chez les individus des catégories O,P,Q

``` r
# ON GARDE LES VARIABLES QUI NOUS INTERESSENT
df_rp2017 <- df_rp2017 %>%
  select('RS_X', 'CABBI', 'ACTET_C', 'ACTET_X', 'NOMVOI_X',
         'NUMVOI_X', 'TYPEVOI_X',
         'BISTER_X', 'CPLADR_X', 'CLT_X','DLT_X',
         'CLT_C_C', 'PLT_X', 'DEPCOM_CODE',
         'PROFS_X', 'SIRET_DEC',
         'x', 'y',"SourceXYW",'qual',
         'SIRETC','SIRETM','qual','VARDOMPART_X','I_MCA_C')

# LISTE DES PROFESSIONS ASSOCIEES A OPQ
liste_opq <- c("POMPIERS(?)","MINISTERE","EDUCATION","LYCEE","ECOLE",
               "CRECHE","GARDERIE","COLLEGE","UNIVERSIT(E|AIRE)",
               "PUBLI(QUE|C)S? ","HOPITA(L|UX)","AP(-)?(^)?HP","HOSPITALIER(S)?","POLICE",
               "GENDARMERIE","FONCTIONNAIRE", "DEPARTEMENT(AL)?(AUX)?","MAIRIE",
               "PREFECT(URE)?(ORAL)?(ORAUX)?","RETRAITE",
               "CLINIQUE","TRIBUNAL","BRIGADE",
               "(L')?(')?INTERIEUR","MUNICIP(AL)?(ALES)?(AUX)?",
               "DEPARTEMENT(AL)?(ALES)?(AUX)?"," NATION(AL)?(ALES)?(AUX)?",
               "ADMINISTRATION","ENSEIGNEMENT", "JUSTICE","SANTE"," COLLECTIVITE(S)?","TERRITORIA(L?)(UX?)(LES?)","EHPAD",
               "RECTORAT","INSPECTION","AMBULANCE","CHU","CHR","CASERNE")


# ON GARDE LES VARIABLES D'INTERET
df_rp2017 <- df_rp2017 %>%  filter(substr(ACTET_C,1,2) %in% c('84', '85', '86', '87', '88') |
                                     grepl(paste(paste0("(",liste_opq,")"), collapse='|'),RS_X) |
                                     grepl(paste(paste0("(",liste_opq,")"), collapse='|'),ACTET_X)
                                   )
```

Données `sirus`
---------------

Elles sont trop lourdes pour être importées directement dans `R`. Nous n'avons cependant pas besoin de celles-ci entières, uniquement des données renvoyant aux secteurs OPQ. Il s'agit donc de les importer pour ensuite les filtrer et les stocker (afin de ne pas alourdir le dossier, le résultat de ceci n'est pas fourni sur github. Dans notre dossier, on a stocké cela sous `sirus_o_p_q.csv` dans le même dossier que `script.R`)

Deux solutions ont été trouvées pour malgré tout importer les données puis les filtrer. Elles sont inscrites dans le dossier `/Python and Spark`:

-   notebook `slice_sirus_PYTHON`: imports successifs de blocs de 100 000 lignes de données via `pandas` qui sont traités et écrits successivements
-   script pyspark `slice_sirus_SPARK`: utilisation d'un langage distribué pour filtrer les données

Le notebook `slice_sirus_PYTHON` a mis environ une heure à tourner. il s'agit du script qui a généré le fichier que nous avons utilisé. Le script `spark` a été écrit *a posteriori*. L'ordre des lignes de nos données n'important pas, la transformation de ce code en permettant le `multithreading` (via `from multiprocessing.dummy import Pool as ThreadPool`) ne devrait pas être compliquée mais le rendre plus rapide.

Le script `spark` a été lancé via `spark-shell` sur un ordinateur disposant de 4 coeurs (sous `linux`). Pour exécuter directement le code (en modifiant la manière d'exporter les données, cf. ci-dessous), il suffit de lancer depuis un terminal de commande, en supposant que `cd` a été initialisé dans le dossier où se situe le document,

``` r
# Si utilise **.write.format('com.databricks.spark.csv') dans code
spark-submit --master local[*] --packages com.databricks:spark-csv_2.10:1.4.0 slice_sirus_SPARK.py

# Sinon
spark-submit --master local[*] slice_sirus_SPARK.py
```

Le script `spark` testé en shell a mis entre 3 et 5 minutes pour importer les données, filtrer et écrire le résultat en `snappy`.

La version de `Spark` utilisée pour le test est ancienne, version `1.6.0`. A partir de la version `2.0`, des méthodes plus simples d'export des données sont disponibles. Le script propose deux manières d'exporter les données (en snappy pour la première, en utilisant package externe pour la seconde). La première méthode est préférable si on dispose d'une infrastructure `HDFS`.

Etape 1: Constitution de raisons sociales cohérentes
====================================================

Etape 1.1. Harmonisation de la raison sociale
=============================================

La première modification automatique possible est l'harmonisation de la raison sociale. De nombreux individus inscrivent le ministère auquel leur fonction les rattache dans la raison sociale: cette erreur est facilement répérable par des expressions régulières

``` r
# MOT CLES
liste.words <- c("MINISTERE DE L'INTERIEUR","MINISTERE DE LA DEFENSE","EDUCATION NATIONALE",
           "GENDARMERIE NATIONALE","MINISTERE DE LA JUSTICE")

# ON CREE UN VECTEUR NETTOYE DE CES MOTS CLES
modif <- tm::removeWords(df_rp2017$RS_X,liste.words)

# ON STOCKE L'ANCIEN NOM
df_rp2017$old_RSX <- df_rp2017$RS_X

# ON MODIFIE
df_rp2017$RS_X <- sapply(1:length(modif), function(i)({
  if (modif[i] == "" & !is.na(modif[i])) df_rp2017$RS_X[i] else modif[i]
}))
```

On fait tout de même attention à ne pas créer un champ vide de raison sociale dans le remplacement. Si la raison sociale se réduit à l'un des mots clés, alors on le laisse (et éventuellement modifie grâce à l'étape 1.2.)

Etape 1.2. Remplacement de la raison sociale quand elle est en fait renseignée dans le nom de voie
--------------------------------------------------------------------------------------------------

Dans de nombreux cas, le nom de voie décrit le lieu de travail. Par exemple, on retrouve dans de nombreux cas le nom d'un collège, lycée... dans le champ du nom de la voie.

La liste de mots qui renvoient à des lieux renseignés dans le mauvais champ est

``` r
listemots <- c("POMPIERS(?)", "MINISTERE",
               "LYCEE", "ECOLE", "CRECHE", "COLLEGE",
               "GARDERIE", "HOPITA(L|UX)",
               "AP(-)?(^)?HP", "HOSPITALIER(S)?", "POLICE",
               "GENDARMERIE", "MAIRIE",
               "PREFECTURE", "TRIBUNAL", "RETRAITE",
               "BRIGADE", "CLINIQUE", "BASE",
               "COLLECTIVITE")
```

Cependant, on désire éviter les erreurs d'interprétation dans le cas où le nom de la voie renvoie bien à un des mots précédents (par exemple *rue des écoles*). Dans ce cas, si les mots suivants sont inclus dans la raison sociale, alors le champ `RS_X` n'est pas substitué par `NOMVOI_X`

``` r
listemots.exclus <- c("RUE","AVENUE","ROUTE","PLACE")
```

La substitution est simplement faite par assignation

``` r
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
```

Etape 2: Appel de l'`API Siret Addok`
=====================================

La première étape de la codification automatique consiste à appeler l'`API Siret Addok` issue de la BAN. Celle-ci a l'avantage d'offrir un moteur de recherche textuel qui nous permet d'envoyer des raisons sociales incomplètes ou imparfaites mais tout de même espérer un *match*.

Pour cela, nous avons créé une fonction `get.banAPI` qui permet de requêter automatiquement l'API Addok (lorsque paramètre `geo.place = T`) pour rechercher une raison sociale dans une commune donnée (code Insee). La fonction `get.banAPI` (cf. document `getGEOAPI.R` dans le dossier `R` de ce projet) permet également, si `geo.place = F` de rechercher des adresses données (cf. étape \*).

Afin de rendre cette méthode utilisable en pratique, les appels à `get.banAPI` ont été parallélisés à l'aide du package `foreach`.

``` r
# INITIALISATION DES CLUSTERS
cl <- parallel::makeCluster(parallel::detectCores()-1, outfile = "")
doParallel::registerDoParallel(cl)

# APPELS PARALLELISES DE LA FONCTION get.banAPI
result <- foreach(i = 1:nrow(df_rp2017), .combine = "list",
                  .multicombine = TRUE,
                  .maxcombine = nrow(df_rp2017),
                  .errorhandling = "pass", .export = 'get.banAPI',
                  .packages = c("jsonlite", "httr")) %dopar% {
                    return(
                      get.banAPI(
                        paste0(df_rp2017$RS_X,"&citycode=",df_rp2017$CLT_C_C)[i], geo.place = T
                      )$features[1,]$properties
                    )
                  }

# STOP LES CLUSTERS
parallel::stopCluster(cl)
```

Les résultats sont stockés sous forme de liste. Le paramètre `.errorhandling = "pass"` nous permet de renvoyer une sortie d'erreur lorsque l'API ne trouve pas de *match*. On arrange et obtient ainsi un `dataframe`:

``` r
result <- lapply(1:length(result), function(i)({
  if (stringr::str_detect(result[[i]], "^Error")){ # Résultat API: erreur --> on renvoie des NA
    result <- rep(NA,5)
  } else{ # On a un résultat: renvoie ce qui nous intéresse
    result <- data.frame(result[[i]]) %>% select_(.dots = c("citycode", "score","id","context","name"))
  }
})
)

result <- tbl_df(do.call(rbind,result))
df <- tbl_df(cbind(result,SIRET_DEC = df.special$SIRET_DEC))
```

Lorsqu'elle trouve un *match*, l'API `Addok` renvoie un score indiquant le degré de confiance du résultat. On ne désire pas utiliser les résultats incertains, pour cette raison on élimine les résultats pour lesquels `score` est trop faible. La fonction `check_validation` permet d'étudier la sensibilité des résultats au choix du seuil d'élimination

``` r
check_validation(df)
```

Nous avons remarqué que prendre le seuil de `score` à 50% offre le meilleur compromis entre performance de prédiction et nombre de `siret` codifiés. Nous ne codifions donc automatiquement que les `siret` pour lesquels le score de confiance est supérieur à 50%:

``` r
# SI SCORE > 50% RESULTAT RECHERCHE SINON NA
df <- df %>% dplyr::mutate(siret = ifelse(result$score > 0.5, id, NA)) %>%
  dplyr::mutate(siret.imput = ifelse(score > 0.5, 1, NA)) %>% select(siret,siret.imput)

df <- tbl_df(cbind(df_rp2017,df))
```

La variable `siret` stocke le `siret` codifiés, la variable `siret.imput` stocke l'étape à laquelle le codage a un lieu

Etape 3: Distance textuelle
===========================

Ecrite mais non implémentée en pratique

`Addok` permettait de codifier les cas les plus évidents (pas besoin de l'adresse, seule une raison sociale approximative suffit). Pour continuer avec les raisons sociales, on peut chercher les raisons sociales proches de celles de `sirus`.

On importe d'abord les données

``` r
# IMPORT DATA
sirus_o_p_q <- readr::read_csv2(paste0(getwd(), "/Dossier fourni/Hackathon-2018/sirus_o_p_q.csv")) %>%
  select_(.dots = c("sirus_id", "nic", "apet",
                    "adr_depcom","denom"))


#On concatene siren et nic dans la base sirus
sirus_o_p_q$nic2<-ifelse(sirus_o_p_q$nic/100<1,as.character(paste0("000",as.character(sirus_o_p_q$nic))),ifelse(sirus_o_p_q$nic/1000<1,paste0("00",as.character(sirus_o_p_q$nic)),
                                                                                                                ifelse(sirus_o_p_q$nic/10000<1,paste0("0",as.character(sirus_o_p_q$nic)),as.character(sirus_o_p_q$nic))))
sirus_o_p_q$siret<-paste0(sirus_o_p_q$sirus_id,sirus_o_p_q$nic2)
```

L'idée est de regarder les raisons sociales les plus proches dans les communes adjacentes (cas où l'individu se trompe de localité mais ne fait qu'une erreur limitée sur la raison sociale)

``` r
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
```

Théoriquement, on peut ensuite faire le bilan:

``` r
siret_imp <- as.character(siret_imp)

#df2 <- df
#df2$siret.imput[is.na(df2$siret) & !is.na(siret_imp)] <- 2
#df2$siret[is.na(df2$siret) & !is.na(siret_imp)] <- paste0(
#  substr(siret_imp[is.na(df2$siret) & !is.na(siret_imp)],1,9), substr(siret_imp[is.na(df2$siret) & !is.na(siret_imp)],11,15)
#)
```

Nous avons remarqué un problème car cette méthode sort bien des SIRET mais le taux d'erreur de cette méthode est de 100%. Il y a probablement un problème dans la sortie des résultats mais nous n'avons pas eu le temps de vérifier la raison.

Etape 4: Appels `API Sirene`
============================

Théoriquement, cette méthode vient se greffer après l'étape de distance textuelle. Cependant, comme avions un problème sur la distance textuelle, nous avons mis cette méthode à la suite de l'appel à `Addok`.

L'API Sirene requiert un certain nombre de champs qui sont fournis par le dataframe `ligne`. L'appel à la fonction `api_siret` est parallélisé

``` r
ligne <- df %>% filter(is.na(siret)) %>%
  select_(.dots = c('CABBI', 'RS_X', 'NOMVOI_X', 'NUMVOI_X', 'TYPEVOI_X',
                                  'BISTER_X', 'CLT_X', 'CLT_C_C'))
ligne[1:3,]
ligne<- apply(ligne, 2, function(x) as.character(x)) 
ligne<- unname(ligne)

# INITIALISATION DU CLUSTER
cl <- parallel::makeCluster(parallel::detectCores()-1, outfile = "")
doParallel::registerDoParallel(cl)

# APPELS PARALLELES DE api_siret
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
```

Comme on ne dispose pas de score de confiance, toutes les valeurs non `NA` sont imputées (si dans le futur un score de confiance est renvoyé, il serait intéressant de discriminer les résultats en fonction de celui-ci).

``` r
df2 <- df
df2$siret.imput[is.na(df2$siret)] <- ifelse(!is.na(result$siren) & !is.na(result$siren),
                                      2, NA)
df2$siret[is.na(df2$siret)] <- ifelse(!is.na(result$siren) & !is.na(result$siren),
                                      paste0(result$siren,result$nic), df2$siret)
```

Etape 5: Appels de l'`API BAN`
==============================

L'`API Sirene` permettait de rechercher les établissements en fonction d'une adresse. S'il n'y a pas de retour de celle-ci, peut-être que l'adresse pose problème. Pour corriger ce problème on peut faire des appels auprès de la BAN qui nous permet de chercher, malgré une adresse vague, une adresse plus propre, que l'on peut alors utiliser via l'API `Sirene` ou `Addok`. Nous avons testé cette méthode indépendamment des autres mais n'avons pas eu le temps de la mettre dans la continuité de l'étape précédente.

``` r
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
```

Etape 6: Plus proches voisins géographiques depuis `sirus`
==========================================================

La dernière étape consiste à imputer les dernières valeurs non encore codifiées. Pour cela, on peut regarder, depuis Sirus, si le plus proche voisin ne peut pas nous aider à obtenir une géolocation précise, i.e. à remplacer les (*x*, *y*) problématiques et ensuite aller voir la base `sirus`.

La fonction `ppv_xy` est construite sur ce modèle. Cependant, nous n'avons pas eu le temps de l'intégrer au flot et ainsi la tester à la suite de nos données (celle-ci a été testée séparemment)

``` r
# =========================
# PLUS PROCHES VOISINS
# =========================

df3 <- df
result = ppv_xy(df3,sirus_o_p_q)

df3$siret.imput[is.na(df3$siret)] <- ifelse(!is.na(result$siren) & !is.na(result$siren),
                                      2, NA)
df3$siret[is.na(df3$siret)] <- ifelse(!is.na(result$siren) & !is.na(result$siren),
                                      paste0(result$siren,result$nic), df2$siret)
```
