setwd("D:/Data/b300yno/Desktop/MEMOIRE/Donnée")
load("bdd.RData")


a<-unique(bdd$SITUATION_DOSSIER)[3]
bdd_2<-subset(subset(bdd,SITUATION_DOSSIER == a), MNT_TOT_REGLEMENT > 0)  #On considère uniquement l'ensemble des dossiers classés CLOS
variables.selectionnees <- c("NUM_CONTRAT","NOM_ASSURE","PRENOM_ASSURE","SOC_APERITRICE","GAR_ELEMENTAIRE","MOTIF_SITUATION",
                             "DT_EFFET_SITUATION_DOS","DAT_NAISSANCE_ASS","DAT_SURVENANCE_SIN","DEB_COUVERTURE","FIN_COUVERTURE",
                             "DEB_PER_REGLEMENT","FIN_PER_REGLEMENT","MNT_BASE","MNT_TOT_REGLEMENT","MNT_REVALORISE","NB_J_INDEMNISES",
                             "DT_MISE_INVALIDITE","MNT_BASE_IJ","MNT_REVALORISATION_IJ","MNT_SS_IJ","MNT_TIERS_IJ",
                             "LIB_ASSIETTE_SALAIRE","VALEUR_SALAIRE","LIB_CHOIX_DE_PRESTATION","INVALIDITE_1ERE_CATEG")
bdd_etude <- bdd_2[,variables.selectionnees]
bdd_etude$DAT_NAISSANCE_ASS <- as.Date(bdd_etude$DAT_NAISSANCE_ASS, format = "%d/%m/%Y" )
bdd_etude$DT_EFFET_SITUATION_DOS <- as.Date(bdd_etude$DT_EFFET_SITUATION_DOS, format = "%d/%m/%Y" )
bdd_etude$DAT_SURVENANCE_SIN <- as.Date(bdd_etude$DAT_SURVENANCE_SIN, format = "%d/%m/%Y" )
bdd_etude$DEB_COUVERTURE <- as.Date(bdd_etude$DEB_COUVERTURE, format = "%d/%m/%Y" )
bdd_etude$FIN_COUVERTURE <- as.Date(bdd_etude$FIN_COUVERTURE, format = "%d/%m/%Y" )
bdd_etude$DEB_PER_REGLEMENT <- as.Date(bdd_etude$DEB_PER_REGLEMENT, format = "%d/%m/%Y" )
bdd_etude$FIN_PER_REGLEMENT <- as.Date(bdd_etude$FIN_PER_REGLEMENT, format = "%d/%m/%Y" )
bdd_etude$DT_MISE_INVALIDITE <- as.Date(bdd_etude$DT_MISE_INVALIDITE, format = "%d/%m/%Y" )


library(knitr)

kable(sort(table(bdd_etude$GAR_ELEMENTAIRE)[table(bdd_etude$GAR_ELEMENTAIRE)>100], decreasing = TRUE))

bdd_etude_ITT <- subset(bdd_etude, GAR_ELEMENTAIRE %in% unique(bdd_etude$GAR_ELEMENTAIRE)[grepl("ITT",as.character(unique(bdd_etude$GAR_ELEMENTAIRE)))] )

bdd_etude_ITT$Nb_jour_couvert <- as.numeric(bdd_etude_ITT$FIN_PER_REGLEMENT - bdd_etude_ITT$DEB_PER_REGLEMENT +1)


library(ggplot2)
bdd_etude_ITT<-subset(bdd_etude_ITT,Nb_jour_couvert <= 1095) 
bdd_etude_ITT$Survenance_Annee <- as.numeric(format(bdd_etude_ITT$DAT_SURVENANCE_SIN,"%Y"))
bdd_etude_ITT$Survenance_Mois <- format(bdd_etude_ITT$DAT_SURVENANCE_SIN,"%B")


bdd_etude_ITT2<-subset(bdd_etude_ITT,Survenance_Annee <= 2021 & Survenance_Annee>2007) 
p<-ggplot(bdd_etude_ITT2, aes(x=`Survenance_Annee`)) + 
  geom_histogram(binwidth=1, fill="navy", color="yellow")
p

kable(table(as.factor(bdd_etude_ITT2$Survenance_Annee)),col.names = c("Année de survenance","Nombre de sinistre"))
help(kable)

table(bdd_etude_ITT2$Survenance_Mois)
# library
install.packages("viridis")
install.packages("hrbrthemes")

library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)

bdd_etude_ITT3<-subset(bdd_etude_ITT,Survenance_Annee <= 2021 & Survenance_Annee>2012) 
p<-ggplot(bdd_etude_ITT3, aes(x=`Survenance_Mois`,fill=`Survenance_Annee`)) + 
  geom_histogram(binwidth=1, color="yellow")
p

install.packages("gender")
  library(genderBR)
help("genderBR")
gender("Guillaume")
install.packages("remotes")
remotes::install_github("lmullen/genderdata")
test_sexe = genderBR::get_gender(as.character(bdd_etude_ITT3$PRENOM_ASSURE))
table(test_sexe)
head(test_sexe)

head(bdd_etude_ITT3$NUM_CONTRAT)
bdd_etude_ITT3$NUM_CONTRAT



length(unique(bdd$NUM_DOSSIER))








