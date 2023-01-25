dsn_1<- read.delim("C:/Users/KDatsi/OneDrive - EY/Documents/PERSONNEL/base.202208 Mémoire Actuariat Absentéisme/DSN/DSN_SOCLE_M_AFFILIATIONBASE_202205_20220621.txt",sep = "|")
dsn_2<- read.delim("C:/Users/KDatsi/OneDrive - EY/Documents/PERSONNEL/base.202208 Mémoire Actuariat Absentéisme/DSN/DSN_SOCLE_M_AFFILIATIONCOTISATION_202205_20220621.txt",sep = "|")


base.202208 <-read.delim("C:/Users/KDatsi/OneDrive - EY/Documents/PERSONNEL/base.202208 Mémoire Actuariat Absentéisme/202208/EXTRACT_DOSSIER_P3IG_1.txt",sep ="	")

for(k in 2:8) {
  base.inter <- read.delim(paste("C:/Users/KDatsi/OneDrive - EY/Documents/PERSONNEL/base.202208 Mémoire Actuariat Absentéisme/202208/EXTRACT_DOSSIER_P3IG_",k,".txt",sep=""),sep ="	")
  base.202208<-rbind(base.202208,base.inter)
}



library(digest)
base.202208$cle_unique <- ""

base.202208$cle_unique <- paste(gsub("\\s+", "", base.202208$NOM_ASSURE), gsub("\\s+", "", base.202208$PRENOM_ASSURE), base.202208$DAT_NAISSANCE_ASS, sep = "")


# Boucle pour créer la clé unique pour chaque ligne
#for (i in 1:nrow(base.202208)) {
#  # Enlever les espaces
#  base.202208$NOM_ASSURE[i] <- gsub("\\s+", "", base.202208$NOM_ASSURE[i])
#  base.202208$PRENOM_ASSURE[i] <- gsub("\\s+", "", base.202208$PRENOM_ASSURE[i])
#  cle <- paste(base.202208$NOM_ASSURE[i], base.202208$PRENOM_ASSURE[i], base.202208$DAT_NAISSANCE_ASS[i], sep = "")

#  base.202208$cle_unique[i] <- digest(cle, algo = "sha256")
#}

library(stringr)


dsn_1$cle_unique_dsn <- paste(str_to_upper(gsub("\\s+", "", dsn_1$LB_NOM)), str_to_upper(gsub("\\s+", "", dsn_1$LB_PRENOM)), dsn_1$DT_NAISSANCE, sep = "")

# Boucle pour créer la clé unique pour chaque ligne
#for (i in 1:nrow(dsn_1)) {
#  # Enlever les espaces
#  dsn_1$LB_NOM[i] <- gsub("\\s+", "", dsn_1$LB_NOM[i])
#  dsn_1$LB_PRENOM[i] <- gsub("\\s+", "", dsn_1$LB_PRENOM[i])
#  cle <- paste(dsn_1$LB_NOM[i], dsn_1$LB_PRENOM[i], dsn_1$DT_NAISSANCE[i], sep = "")
  
#  dsn_1$cle_unique_dsn[i] <- digest(cle, algo = "sha256")
#}

#$cle_unique <- ""

#bdd$cle_unique <- paste(gsub("\\s+", "", bdd$NOM_ASSURE), gsub("\\s+", "", bdd$PRENOM_ASSURE), bdd$DAT_NAISSANCE_ASS, sep = "")


# Boucle pour créer la clé unique pour chaque ligne
#for (i in 1:nrow(bdd)) {
#  # Enlever les espaces
#  bdd$NOM_ASSURE[i] <- gsub("\\s+", "", bdd$NOM_ASSURE[i])
#  bdd$PRENOM_ASSURE[i] <- gsub("\\s+", "", bdd$PRENOM_ASSURE[i])
# cle <- paste(bdd$NOM_ASSURE[i], bdd$PRENOM_ASSURE[i], bdd$DAT_NAISSANCE_ASS[i], sep = "")
  
#  bdd$cle_unique[i] <- digest(cle, algo = "sha256")
#}


merged_base.202208<-merge(dsn_1,base.202208,by.x="cle_unique_dsn",by.y="cle_unique")

head(merged_base.202208)


sum(nchar(merged_base.202208$NIR)<10)
base.etude<-subset(merged_base.202208,nchar(merged_base.202208$NIR)>10) #uniquement les lignes avec un num ss

base.etude<- subset(base.etude,MNT_TOT_REGLEMENT>0) # uniquement les lignes avec mtn >0







