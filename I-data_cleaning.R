###########################################################################
####Library
library(knitr)
library(ggplot2)



####################Import data########################################################



setwd("C:/Users/KDatsi/OneDrive - EY/Documents/PERSONNEL/BDD Mémoire Actuariat Absentéisme")
bdd <- read.csv("bdd_etude.csv")


#########################Traitement données###################################################


a<-unique(bdd$SITUATION_DOSSIER)[3]
bdd_2<-subset(bdd, MNT_TOT_REGLEMENT > 0)  
variables.selectionnees <- c("cle_unique_dsn","NUM_CONTRAT","NOM_ASSURE","PRENOM_ASSURE","SOC_APERITRICE","GAR_ELEMENTAIRE","MOTIF_SITUATION",
                             "DT_EFFET_SITUATION_DOS","DAT_NAISSANCE_ASS","DAT_SURVENANCE_SIN","DEB_COUVERTURE","FIN_COUVERTURE",
                             "DEB_PER_REGLEMENT","FIN_PER_REGLEMENT","MNT_BASE","MNT_TOT_REGLEMENT","MNT_REVALORISE","NB_J_INDEMNISES",
                             "DT_MISE_INVALIDITE","MNT_BASE_IJ","MNT_REVALORISATION_IJ","MNT_SS_IJ","MNT_TIERS_IJ",
                             "LIB_ASSIETTE_SALAIRE","VALEUR_SALAIRE","LIB_CHOIX_DE_PRESTATION","CD_OPTION","NIR","SITUATION_DOSSIER","SITUATION_CONTR")
bdd_etude <- bdd_2[,variables.selectionnees]
bdd_etude$DAT_NAISSANCE_ASS <- as.Date(bdd_etude$DAT_NAISSANCE_ASS, format = "%d/%m/%Y" )
bdd_etude$DT_EFFET_SITUATION_DOS <- as.Date(bdd_etude$DT_EFFET_SITUATION_DOS, format = "%d/%m/%Y" )
bdd_etude$DAT_SURVENANCE_SIN <- as.Date(bdd_etude$DAT_SURVENANCE_SIN, format = "%d/%m/%Y" )
bdd_etude$DEB_COUVERTURE <- as.Date(bdd_etude$DEB_COUVERTURE, format = "%d/%m/%Y" )
bdd_etude$FIN_COUVERTURE <- as.Date(bdd_etude$FIN_COUVERTURE, format = "%d/%m/%Y" )
bdd_etude$DEB_PER_REGLEMENT <- as.Date(bdd_etude$DEB_PER_REGLEMENT, format = "%d/%m/%Y" )
bdd_etude$FIN_PER_REGLEMENT <- as.Date(bdd_etude$FIN_PER_REGLEMENT, format = "%d/%m/%Y" )
bdd_etude$DT_MISE_INVALIDITE <- as.Date(bdd_etude$DT_MISE_INVALIDITE, format = "%d/%m/%Y" )


gar_IT<-c("ITT     ","ITT_HPRO","ITTM_HPR","INC     ","ITTM    ","ITT_HOSP","ITT_PRO ","ITTA_PRO",
          "ITTA    ","ITT_MAT ","ITT_MO  ","ITTM_PRO","ITT_ATMC","ITTA_HPR","ITT_AT  ","ITT_LO  ",
          "ITT_HOHP","ITMM_HPR","ITMA_PRO","ITMM    ","ITT_LD  ","ITT_HOPR")

gar_IP <- c("IPT_HPRO","ITP     ","IPT     ","ITP_HPRO","IPP_HPRO","ITPM_HPR","IPP     ","INV     ",
            "ITPM    ","ITPA    ","ITPA_PRO","ITP_PRO ","IPP_PRO ","ITPM_PRO","X-IPT   ","IPT_PRO ","INV_CAP ","X-IPP   ","IPT_T_AT")

bdd_etude$Nb_jour_couvert <- as.numeric(bdd_etude$FIN_PER_REGLEMENT - bdd_etude$DAT_SURVENANCE_SIN +1)
bdd_etude<- subset(bdd_etude,Nb_jour_couvert>=0)
bdd_etude$SEXE <-sapply(bdd_etude$NIR,function(x){if(substr(x,start=1,stop = 1)==1) "M" else "F"})

bdd_etude$DEP_NAISS <-substr(bdd_etude$NIR,start=6,stop = 7)
bdd_etude$AGE <- as.integer((bdd_etude$DAT_SURVENANCE_SIN-bdd_etude$DAT_NAISSANCE_ASS)/365.25)
n<-names(sort(table(bdd_etude$CD_OPTION),decreasing=TRUE))
code.option <- c(n[grepl("CAD",n)],n[grepl("NC",n)],n[grepl("ENS",n)])
bdd_etude <- subset(bdd_etude, CD_OPTION %in% code.option)

bdd_etude$CAT_PRO <- as.factor(sapply(bdd_etude$CD_OPTION,function(x){if(grepl("CAD",x)){"Cadre"} else if(grepl("NC",x)){"Non Cadre"} else if(grepl("ENS",x)){"Ensemble du personnel"} else {Divers}}))
bdd_etude$Survenance_Annee <- as.numeric(format(bdd_etude$DAT_SURVENANCE_SIN,"%Y"))
bdd_etude$Survenance_Mois <- format(bdd_etude$DAT_SURVENANCE_SIN,"%B")


#########Doublon#####


bdd_etude$CLE_DOUBLON_2 <-paste(gsub("\\s+", "", bdd_etude$NOM_ASSURE),gsub("\\s+", "", bdd_etude$PRENOM_ASSURE),gsub("\\s+", "", bdd_etude$DAT_NAISSANCE_ASS),gsub("\\s+", "", bdd_etude$DAT_SURVENANCE_SIN),gsub("\\s+", "", bdd_etude$FIN_PER_REGLEMENT))



duplicates <- duplicated(bdd_etude$CLE_DOUBLON_2)
bdd_etude_nd<-bdd_etude[!duplicates,]

bdd_etude_nd_ITT <- subset(bdd_etude_nd, GAR_ELEMENTAIRE %in% gar_IT)
bdd_etude_ITT<-subset(bdd_etude_nd_ITT,Nb_jour_couvert <= 1095) 


bdd_etude_INV <- subset(bdd_etude_nd, GAR_ELEMENTAIRE %in% gar_IP)




#########export de la data#####

write.csv(bdd_etude_ITT,"C:/Users/KDatsi/OneDrive - EY/Documents/PERSONNEL/BDD Mémoire Actuariat Absentéisme/bdd_etude_ITT.csv",row.names=FALSE)
write.csv(bdd_etude_INV,"C:/Users/KDatsi/OneDrive - EY/Documents/PERSONNEL/BDD Mémoire Actuariat Absentéisme/bdd_etude_ITP.csv",row.names=FALSE)



