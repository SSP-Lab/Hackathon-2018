# ----------------------------
# Import packages

# install.packages("data.table")
# install.packages("sp")
# install.packages("sf")
# install.packages("RColorBrewer")
library(sp)
library(data.table)
library(sf)
library(RColorBrewer)

# ----------------------------
# Paramètres

an <- 2014
dep <- "35"
pal <- brewer.pal(8, "Reds") # Palette de couleurs

path1 <- "D:/Hackathon-2018/données/DonneesTransmisesHackathon" # chemin vers les données exhaustives
path2 <- "C:/Users/HACKATHON/Documents/Geocodeurs" # chemin vers les sous-tables préparées
path3 <- "C:/Users/HACKATHON/Documents/Geocodeurs/Lecteur USB/Fonds_de_carte"  # chemin vers les fonds de carte


# ----------------------------
# Import données

rp <- read.csv2(paste0(path2, "/rp_final_", an, ".csv"), stringsAsFactors = FALSE)
sirus <- read.csv2(paste0(path2, "/sirus", an, "_", dep, ".csv"), stringsAsFactors = FALSE)
lev <- read.csv2(paste0(path2, "/verif_multiple_geo.csv"), stringsAsFactors = FALSE)

# ----------------------------
# Retravail des données originales

# Recodage des variables numériques codées à tort en caractères (cause de bugs)
rp$x <- as.numeric(rp$x)
rp$y <- as.numeric(rp$y)
rp$SourceXYW <- as.numeric(rp$SourceXYW)
rownames(rp) <- rp$CABBI
sirus$x <- as.numeric(sirus$x)
sirus$y <- as.numeric(sirus$y)
sirus$SourceXYW <- as.numeric(sirus$SourceXYW)
sirus$com_et <- substr(sirus$adr_et_loc_geo, 1, 5) # code commune de l'établissement
# On rajoute les zéros pour que le sirus_id soit sur 5 caractères
sirus$sirus_id <- str_pad(sirus$sirus_id, 9, side = "left", pad = "0")
# On rajoute les zéros pour que le nic soit sur 5 caractères
sirus$nic <- str_pad(sirus$nic, 5, side = "left", pad = "0")

# dep_et : département de l'établissement dans le RP
rp$dep_et <- rp$CLT_C_C
rp[rp$CLT_C_C == "", "dep_et"] <- rp[rp$CLT_C_C == "", "DEPCOM_CODE"]
sum(rp$dep_et == "")
rp$com_et <- rp$dep_et
rp$dep_et <- substr(rp$dep_et, 1, 2)

# Filtre sur RP (au département)
rp <- subset(rp, dep_et == dep)
# 550 cas à traiter pour 2014 en Ille et Villaine

# Qqs stats des
table(rp$qual) / nrow(rp)
barplot(table(as.numeric(rp$SourceXYW)))

# Conversion en objets géographiques
sirus$geometry <- sprintf("POINT (%f %f)", sirus$x, sirus$y)
sirus <- st_as_sf(sirus, wkt = "geometry", crs = as.integer("2154"))

# -------------------------
# DEBUT DE LA DEMARCHE
# -------------------------

deb <- Sys.time() # début temps d'exécution

# Deux paquets d'observations dans rp
#  > rp1 avec xy de qualité correcte ou très bonne (au sens de la variable qual)
#  > rp2 avec xy de mauvaise qualité ou manquants
rp1 <- rp[!is.na(rp$x) & rp$qual %in% c("1", "2.1", "2.2", "8"), ]
rp2 <- rp[is.na(rp$x) | !rp$qual %in% c("1", "2.1", "2.2", "8"), ]
nrow(rp1) / nrow(rp)
nrow(rp2) / nrow(rp)


# -------------------------
# Initialisation de l'objet de sortie : RESUL
RESUL <- data.frame(SIRET = rep(NA, nrow(rp)), rs = rep(NA, nrow(rp)), denom = rep(NA, nrow(rp)), etape = rep(NA, nrow(rp)))
rownames(RESUL) <- rp$CABBI
head(RESUL)


# -----------------------
# Etape 1 (rp1) : QUE FAIT-ON DES XY DE BONNE QUALITE ?

print(paste("On travaille sur rp1 de taille", nrow(rp1)))

rp1$geometry <- sprintf("POINT (%f %f)", rp1$x, rp1$y)
rp1 <- st_as_sf(rp1, wkt = "geometry", crs = as.integer("2154"))
plot(rp1$geometry, pch = 20, col = pal[8:1][as.factor(rp1$qual)])

# Etape 1 : on recherche les matchs exacts sur xy
int <- st_intersects(rp1, st_buffer(sirus, 1))
t1 <- sapply(int, length)
t1_ <- table(t1)
paste("On a trouvÃ© un match exact pour", t1_["1"], "/", nrow(rp1))
ok <- rownames(rp1)[t1 == 1]
eli_plusieurs <- rownames(rp1)[t1 > 1]
# ok
RESUL[ok, "etape"] <- "1_match1"
RESUL[eli_plusieurs, "etape"] <- "1_match>1"
table(RESUL$etape) / nrow(RESUL)

# On va mettre le bon sirus_id pour les "1_match1"
lignes_sirus <- sapply(int, function(x){x[1]})
RESUL[rownames(rp1), ][t1 == 1, "SIRET"] <- 
  paste0(sirus[lignes_sirus[t1 == 1], ]$sirus_id, 
         sirus[lignes_sirus[t1 == 1], ]$nic)
RESUL[rownames(rp1), ][t1 == 1, "rs"] <- sirus[lignes_sirus[t1 == 1], ]$enseigne_et1
RESUL[rownames(rp1), ][t1 == 1, "denom"] <- sirus[lignes_sirus[t1 == 1], ]$denom

# ------------------------------------------
# Etape 2 : pour ceux pour qui on a trouvÃ© 0 match, on fait un rayon évolutif

# Sirus : on construit des cercles de rayon SourceXYW
table(sirus$qual)
tapply(sirus$SourceXYW, sirus$qual, function(x){mean(x, na.rm = TRUE)})

# On met des rayons à la main selon valeur de qual
sirus$ray <- sirus$SourceXYW
sirus$ray[sirus$qual == "1"] <- 10
sirus$ray[sirus$qual == "2.1"] <- 50
sirus$ray[sirus$qual == "2.2"] <- 100
sirus$ray[sirus$qual == "8"] <- 1000
sirus$ray[is.na(sirus$ray)] <- 1000

buffersSirus <- st_buffer(sirus, dist = sirus$ray)
# plot(sirus$geometry, pch = '.')
# plot(buffersSirus$geometry, border = "grey", add = TRUE)

int2 <- st_intersects(rp1[t1 == 0, ], buffersSirus)
t2 <- sapply(int2, length)
t2_ <- table(t2)
t1_
# Idem que plus haut
ok <- rownames(rp1[t1 == 0, ])[t2 == 1]
eli_plusieurs <- rownames(rp1[t1 == 0, ])[t2 > 1]
# ok
RESUL[ok, "etape"] <- "2_match1"
RESUL[eli_plusieurs, "etape"] <- "2_match>1"

pas_ok <- rownames(rp1[t1 == 0, ])[t2 == 0]
RESUL[pas_ok, "etape"] <- "12_match0"

table(RESUL$etape) / nrow(rp1)
# 25 % de matchs exacts
# 75 % de matchs pluriels en étape 1 ou 2

# On va mettre le bon sirus_id pour les "1_match1"
lignes_sirus <- sapply(int2, function(x){x[1]})
RESUL[rownames(rp1), ][t2 == 1, "SIRET"] <- 
  paste0(sirus[lignes_sirus[t2 == 1], ]$sirus_id, 
         sirus[lignes_sirus[t2 == 1], ]$nic)
RESUL[rownames(rp2), ][t2 == 1, "rs"] <- sirus[lignes_sirus[t2 == 1], ]$enseigne_et1
RESUL[rownames(rp2), ][t2 == 1, "denom"] <- sirus[lignes_sirus[t2 == 1], ]$denom


# ------------------------------------------
# Etape 3 : pour ceux qui ont 0 ou plusieurs matchs + les RP2, appariement sur la raison sociale ?

rp_pb <- rp[rownames(RESUL)[is.na(RESUL$etape)], ]
nrow(rp_pb) / nrow(rp)
# puisqu'on n'a pas de cas de zéros matchs on retrouve
rp_pb$geometry <- sprintf("POINT (%f %f)", rp_pb$x, rp_pb$y)
rp_pb <- st_as_sf(rp1, wkt = "geometry", crs = as.integer("2154"))
print(paste("On travaille sur rp_pb (cas non rÃ©solus) de taille", nrow(rp_pb)))
  
# Paramètres
lvar_match_rp <- c("RS_X", "RS_X", "RS_X", "com_et")
lvar_match_sirus <- c("enseigne_et1", "denom", "denom_condense", "com_et")
match_ok <- 1:3


# Construction matrice d'entiers qui correspondent aux croisements lvar_match_rp * lvar_match_sirus
mat_match <- matrix(FALSE, ncol = length(lvar_match_rp), nrow = nrow(rp_pb))
for(k in 1:length(lvar_match_rp)){
  # k <- 1
  print(paste0("On Ã©tudie le match : ", lvar_match_rp[k], " * ", lvar_match_sirus[k]))
  lmod <- table(sirus[[lvar_match_sirus[k]]])
  mat_match[, k] <- lmod[rp_pb[[lvar_match_rp[k]]]]
  print(round(100 * sum(pmin(1, mat_match[, k]), na.rm = TRUE) / nrow(rp_pb), 2))
}
colnames(mat_match) <- paste0(lvar_match_rp, "_", lvar_match_sirus)
colSums(mat_match, na.rm = TRUE)
mat_match[is.na(mat_match)] <- FALSE

for(k in rev(match_ok)){
  # k <- 2
  nom <- lvar_match_sirus[k]
  nom
  filtre <- mat_match[, paste0(lvar_match_rp[1], "_", nom)] == 1
  sum(filtre)
  rs <- rp_pb[filtre, ][[lvar_match_rp[1]]]
  rs
  temp <- sirus[sirus[[nom]] %in% rs, c(nom, "sirus_id", "nic")]
  nrow(temp)
  rownames(temp) <- temp[[nom]]
  RESUL[rownames(rp_pb), ][filtre, ]$SIRET <- paste0(temp[rp_pb[filtre, ]$RS_X, ]$sirus_id, 
                                                     temp[rp_pb[filtre, ]$RS_X, ]$nic)
  RESUL[rownames(rp_pb), ][filtre, "rs"] <- rs
  RESUL[rownames(rp_pb), ][filtre, "etape"] <- paste0("3_RS_", k)
}

RESUL$etape[is.na(RESUL$etape)] <- "0"
table(RESUL$etape)
table(RESUL$etape) / nrow(RESUL) # on récupère 

# ----------------------------
# Représentation des cas résolus par simple match
men <- SpatialPoints(cbind(rp$x[!is.na(rp$x)], rp$y[!is.na(rp$y)]))
men <- st_as_sf(men)

col <- rep("grey", nrow(men))
col[RESUL[!is.na(rp$x), ]$etape %in% c("1_match1", "2_match1")] <- "tomato2"
col[RESUL[!is.na(rp$x), ]$etape %in% c("1_match>1", "2_match>1")] <- "yellow"
col[RESUL[!is.na(rp$x), ]$etape %in% c("3_RS_1", "3_RS_2", "3_RS_3")] <- "steelblue"

# plot(sirus$geometry, pch = '.', col = "steelblue")
dep <- st_read(paste0(path3, "/Depf.TAB"))
com <- st_read(paste0(path3, "/Comf.TAB"))

plot(men$geometry, pch = 20, col = col)
nrow(men)
plot(dep$geometry, lty = 3, add = TRUE)
legend("bottomright", 
       fill = c("grey", "tomato2", "yellow", "steelblue"), 
       legend = c("0 match", "Match exact - xy", "Plusieurs matchs - xy", "Match exact - RS"))

RESUL$CABBI <- rownames(RESUL)
# RESUL$SIRET[is.na(RESUL$SIRET)] <- ""
# RESUL$v1 <- NA
# RESUL$v2 <- NA
# RESUL$v3 <- NA

# ----------------------------
# Export du résultat en csv
write.csv2(RESUL[, c("CABBI", "SIRET")], paste0(path2, "/dep35_2014_Geocodeurs_2.csv"), 
           row.names = FALSE, quote = FALSE, na="")

fin <- Sys.time() # fin temps d'exécution
print(fin - deb)


# ----------------------------
# BROUILLONS OU AUTRES TRAITEMENTS DIVERS
# ----------------------------

# ----------------------------
# AJOUT RESULTATS DE JOAQUIM
head(lev)
rownames(RESUL) <- RESUL$CABBI
RESUL[as.character(lev$cabbi), ]$etape <- "4_Lev"
RESUL[as.character(lev$cabbi), ]$etape <- "4_Lev"
RESUL[as.character(lev$cabbi), ]$SIRET <- lev$siretverif

# setdiff(RESUL$CABBI, rp$CABBI)

# ----------------------------
# Carte
col <- rep("grey", nrow(RESUL))
col[RESUL$etape %in% c("1_match1", "2_match1")] <- "tomato2"
col[RESUL$etape %in% c("1_match>1", "2_match>1")] <- "yellow"
col[RESUL$etape %in% c("3_RS_1", "3_RS_2", "3_RS_3", "4_Lev")] <- "steelblue"
plot(rp1$geometry, pch = 20, col = col)
plot(com35$geometry, lty = 3, border = "grey", add = TRUE)
plot(dep$geometry, lwd = 2, add = TRUE)

legend("bottomright", 
       fill = c("grey", "tomato2", "steelblue"), 
       legend = c("0 match", "Match exact - xy", "Levenstein parmi n candidats"))

dev.print(png, paste0(path2, "/Carte.png"), res = 300, unit = "in", width = 5, height = 5)
dev.off()

# ----------------------------
# Carte de buffers : exemples
com35 <- subset(com, Dep == "35")
t <- rp[!is.na(rp$x), ]
t$geometry <- sprintf("POINT (%f %f)", t$x, t$y)
t <- st_as_sf(t, wkt = "geometry", crs = as.integer("2154"))

t <- t[t$DEPCOM_CODE %in% c("35281", "35238", "35047"), ]
nrow(t)
plot(t$geometry, pch = '', col = "black")
plot(com35$geometry, lty = 3, border = "grey", add = TRUE)
plot(dep$geometry, lwd = 2, add = TRUE)
ech <- sample(1:nrow(sirus), size = 100)
plot(buffersSirus[ech, ]$geometry, add = TRUE, col = "grey", border = NA)
plot(sirus[ech, ]$geometry, add = TRUE, pch = 20, col = "tomato3")


# ----------------------------
# Comparaison visuelle (et calcul à la main taux de concordance avant de soumettre)
controle <- rp[!is.na(RESUL$SIRET), ]
trouve <- RESUL[!is.na(RESUL$SIRET), ]
comp <- merge(controle[, c("siret_final", "CABBI", "RS_X")], 
              trouve,
              by.x = "CABBI")
comp$siret_final <- as.character(comp$siret_final)
concorde <- comp$siret_final == comp$SIRET
concorde[is.na(concorde)] <- FALSE
mean(concorde)


tapply(concorde, comp$etape, mean)
f <- RESUL$etape
sum(comp$siret_final == comp$SIRET, na.rm = TRUE) / nrow(comp)

View(comp[comp$siret_final != comp$SIRET, ])

# ----------------------------
# Débug
# table(rp$qual) / nrow(rp)

# id <- "101000008"
# nic <- "06956"
# t <- sirus[sirus$sirus_id == id & sirus$nic == nic, ]
# t <- sirus[sirus$sirus_id == id, ]
# View(t)
# 
# c <- "4302695834"
# View(rp[c, ])
# corr <- rp[c, "siret_final"]
# View(sirus[paste0(sirus$sirus_id, sirus$nic) == corr, ])
# 
# corr2 <- RESUL[c, "SIRET"]
# View(sirus[paste0(sirus$sirus_id, sirus$nic) == corr2, ])
