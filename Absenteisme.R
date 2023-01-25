setwd("C:/Users/KDatsi/OneDrive - EY/Documents/PERSONNEL/BDD Mémoire Actuariat Absentéisme")
bdd <- read.csv("bdd.csv")


a<-unique(bdd$SITUATION_DOSSIER)[3]
bdd_2<-subset(bdd, MNT_TOT_REGLEMENT > 0)  
variables.selectionnees <- c("NUM_CONTRAT","NOM_ASSURE","PRENOM_ASSURE","SOC_APERITRICE","GAR_ELEMENTAIRE","MOTIF_SITUATION",
                             "DT_EFFET_SITUATION_DOS","DAT_NAISSANCE_ASS","DAT_SURVENANCE_SIN","DEB_COUVERTURE","FIN_COUVERTURE",
                             "DEB_PER_REGLEMENT","FIN_PER_REGLEMENT","MNT_BASE","MNT_TOT_REGLEMENT","MNT_REVALORISE","NB_J_INDEMNISES",
                             "DT_MISE_INVALIDITE","MNT_BASE_IJ","MNT_REVALORISATION_IJ","MNT_SS_IJ","MNT_TIERS_IJ",
                             "LIB_ASSIETTE_SALAIRE","VALEUR_SALAIRE","LIB_CHOIX_DE_PRESTATION","CD_OPTION")
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

gar.inv<-as.factor(unique(bdd_etude$GAR_ELEMENTAIRE)[grepl("INV",as.character(unique(bdd_etude$GAR_ELEMENTAIRE)))])
gar.ipt<-unique(bdd_etude$GAR_ELEMENTAIRE)[grepl("IPT",as.character(unique(bdd_etude$GAR_ELEMENTAIRE)))]

bdd_etude_ITT <- subset(bdd_etude, GAR_ELEMENTAIRE %in% unique(bdd_etude$GAR_ELEMENTAIRE)[grepl("ITT",as.character(unique(bdd_etude$GAR_ELEMENTAIRE)))] )
bdd_etude_INV <- rbind(subset(bdd_etude, GAR_ELEMENTAIRE %in% gar.inv),subset(bdd_etude, GAR_ELEMENTAIRE %in% gar.ipt))

bdd_etude_ITT$Nb_jour_couvert <- as.numeric(bdd_etude_ITT$FIN_PER_REGLEMENT - bdd_etude_ITT$DEB_PER_REGLEMENT +1)


library(ggplot2)
bdd_etude_ITT<-subset(bdd_etude_ITT,Nb_jour_couvert <= 1095) 
bdd_etude_ITT$Survenance_Annee <- as.numeric(format(bdd_etude_ITT$DAT_SURVENANCE_SIN,"%Y"))
bdd_etude_ITT$Survenance_Mois <- format(bdd_etude_ITT$DAT_SURVENANCE_SIN,"%B")

bdd_etude_ITT$Survenance_Annee <- as.numeric(format(bdd_etude_ITT$DAT_SURVENANCE_SIN,"%Y"))
bdd_etude_INV$Survenance_Annee <- as.numeric(format(bdd_etude_INV$DAT_SURVENANCE_SIN,"%Y"))

bdd_etude_ITT$Survenance_Mois <- format(bdd_etude_ITT$DAT_SURVENANCE_SIN,"%B")
bdd_etude_INV$Survenance_Mois <- format(bdd_etude_INV$DAT_SURVENANCE_SIN,"%B")

bdd_etude_ITT2<-subset(bdd_etude_ITT,Survenance_Annee <= 2021 & Survenance_Annee>2012)
bdd_etude_INV2<-subset(bdd_etude_INV,Survenance_Annee <= 2021 & Survenance_Annee>2012)

bdd_etude_ITT2<-subset(bdd_etude_ITT,Survenance_Annee <= 2021 & Survenance_Annee>2007) 
bdd_etude_ITT2$Survenance_Annee=as.factor(bdd_etude_ITT2$Survenance_Annee)
p<-ggplot(bdd_etude_ITT2, aes(x=`Survenance_Annee`)) + 
  geom_histogram(binwidth=1, fill="navy", color="yellow")
p

graph.evol.annee <- ggplot(bdd_etude_ITT2,
                          aes(x=Survenance_Annee)) +
  geom_bar(stat="count",position = "dodge",fill="navy", color="yellow")
graph.evol.annee





kable(table(as.factor(bdd_etude_ITT2$Survenance_Annee)),col.names = c("Année de survenance","Nombre de sinistre"))

#Tableau des variables
kable(cbind(variables.selectionnees,description.variables),col.names = c("Nom des variables","Description") )
description.variables <-c("Numéro de contrat","Nom de l'assuré","Prénom de l'assuré","Nom de la société apéritrice","Nom de la garantie élémentaire",
  "Motif du sinistre","Date d'effet du sinsitre","Date de naissance de l'assuré","Date de survenance du sinistre",
  "Date de début de couverture","Date de fin de couverture", "Date de début de la période de réglement", "Date de fin de la période de réglement",
  "Montant de base ou montant de la rente annualisée", "Montant total réglé", "Montant total revalorisé", "Nombre de jours indemnisés",
  "Date de mise en invalidité dans le cas d'un dossier passé en invalidité", "Montant de l'indemnité journaliere", "Montant de l'IJ revalorisée",
  "Montant l'IJ versé par la Sécurite Sociale","Montant tiers payant de l'IJ", "Libellé de l'assiette de salaire", "Montant du salaire brut annuel de l'assuré",
  "Libellé de la prestation", "Code option")
length(variables.selectionnees)
kable(c(variables.selectionnees),col.names = c("Nom des variables") )




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

###########################################################################
#### Trouver le sexe d'un assuré via son prénom

install.packages("gender")
  library(genderBR)
help("genderBR")
gender("Guillaume")
install.packages("remotes")
remotes::install_github("lmullen/genderdata")
test_sexe = genderBR::get_gender(as.character(bdd_etude_ITT3$PRENOM_ASSURE))
table(test_sexe)
head(test_sexe)

#Pb de notation, les prénoms sont souvent dans la variable NOM
#Solution : Fusionner Nom et Prénom puis séparer ensuite 


###########################################################################
##############Saisonnalité des arrêts de travail ##################
###########################################################################

ggplot(table(bdd_etude_ITT3$Survenance_Mois))
bdd_etude_ITT3$Survenance_Saison <- as.factor(bdd_etude_ITT3$Survenance_Mois)
levels(bdd_etude_ITT3$Survenance_Saison)<-c("ete","printemps",
                                            "automne","hiver",
                                            "hiver",
                                            "ete","printemps",
                                            "printemps","hiver",
                                            "automne","automne",
                                            "ete")

table_mois <- table(bdd_etude_ITT3$Survenance_Mois)[c("janvier",'février',"mars","avril","mai","juin","juillet","août","septembre","octobre","novembre","décembre")]
#barplot(table_mois)
table_saison <- table(bdd_etude_ITT3$Survenance_Saison)[c("printemps","ete","automne","hiver")]
#barplot(table_saison)


bdd_etude_ITT4<-subset(bdd_etude_ITT3,Survenance_Annee != 2020 & Survenance_Annee>2017) 
bdd_etude_ITT4$Survenance_Annee<-as.factor(bdd_etude_ITT4$Survenance_Annee)

graph.sais.mois <- ggplot(bdd_etude_ITT4,
  aes(x=Survenance_Mois,fill=Survenance_Annee)) +
  geom_bar(stat="count",position = "dodge")
graph.sais.mois + scale_x_discrete(limits=c("janvier",'février',"mars","avril","mai","juin","juillet","août","septembre","octobre","novembre","décembre"))

graph.sais.saison <- ggplot(bdd_etude_ITT4,
                          aes(x=Survenance_Saison,fill=Survenance_Annee)) +
  geom_bar(stat="count",position = "dodge")
graph.sais.saison+ scale_x_discrete(limits=c("printemps","ete","automne","hiver"))


table_saison_2018 <- table(subset(bdd_etude_ITT3,Survenance_Annee == 2018)$Survenance_Saison)
table_saison_2019 <- table(subset(bdd_etude_ITT3,Survenance_Annee == 2019)$Survenance_Saison)
table_saison_2018
table_saison_2019

table_saison_2018["hiver"]/table_saison_2018["ete"] #Hausse de 34,7% entre ete et hiver

table_saison_2018["automne"]/table_saison_2018["ete"]

###########################################################################
####################Typologie des arrêts de travail########################
###########################################################################

bdd_etude_ITT3$Nb_jour_couvert

type.arret.travail<-function(x){
  if(is.na(x)){return(as.factor("NA"))}
  else if(x<=10){return(as.factor("court"))}
  else if(x>10 & x<=90){return(as.factor("moyen"))}
  else return(as.factor("long"))
}

bdd_etude_ITT3$Type_Arret = sapply(bdd_etude_ITT3$Nb_jour_couvert,type.arret.travail)

table(bdd_etude_ITT3$Type_Arret)

bdd_etude_ITT4<-subset(bdd_etude_ITT3,Survenance_Annee <= 2021 & Survenance_Annee>2017 & Type_Arret!="NA") 
bdd_etude_ITT4$Survenance_Annee<-as.factor(bdd_etude_ITT4$Survenance_Annee)

graph.type.arret <- ggplot(bdd_etude_ITT4,
                            aes(x=Type_Arret,fill=Survenance_Annee)) +
  geom_bar(stat="count",position = "dodge")
graph.type.arret

length(unique(bdd_etude$GAR_ELEMENTAIRE))


###########################################################################
###############Arrêts de travail en fonction du collègue###################
###########################################################################

sort(table(bdd_etude_ITT3$CD_OPTION),decreasing=TRUE)
n<-names(sort(table(bdd_etude_ITT3$CD_OPTION),decreasing=TRUE))
code.option <- c(n[grepl("CAD",n)],n[grepl("NC",n)],n[grepl("ENS",n)])

bdd_etude_ITT5 <- subset(bdd_etude_ITT3, CD_OPTION %in% code.option)


bdd_etude_ITT5$Cat_Option <- as.factor(sapply(bdd_etude_ITT5$CD_OPTION,function(x){if(grepl("CAD",x)){"Cadre"} else if(grepl("NC",x)){"Non Cadre"} else if(grepl("ENS",x)){"Ensemble du personnel"} else {Divers}}))

bdd_etude_ITT5<-subset(bdd_etude_ITT5,Survenance_Annee <= 2020 & Survenance_Annee>2012) 
bdd_etude_ITT5$Survenance_Annee<-as.factor(bdd_etude_ITT5$Survenance_Annee)


graph.type.coll <- ggplot(bdd_etude_ITT5,
                           aes(x=Cat_Option,fill=Survenance_Annee)) +
  geom_bar(stat="count",position = "dodge")
graph.type.coll


###########################################################################
#################Arrêts de travail en fonction de l'age####################
###########################################################################

bdd_etude_ITT5$Age <- as.integer((bdd_etude_ITT5$DAT_SURVENANCE_SIN-bdd_etude_ITT5$DAT_NAISSANCE_ASS)/365.25)
sum(bdd_etude_ITT5$Age > 100)
bdd_etude_ITT5 <- subset(bdd_etude_ITT5, Age <=62 & Age>15)

quantile(bdd_etude_ITT5$Age, seq(0,1,by=0.25))

newClassAge <- c(-Inf, 26, 37, 48,62, Inf)

regrouprecode <- function(base, listvar, listlevel)
{
  for(i in 1:length(listvar))
  {
    if(is.factor(base[, listvar[i]]))
      levels(base[, listvar[i]]) <- listlevel[[i]]
    else
    {
      base[, listvar[i]] <- cut(base[, listvar[i]], listlevel[[i]])
    }
  }
  
  base
}

baseage<- regrouprecode(bdd_etude_ITT5,c("Age"),list(newClassAge))


graph.Age <- ggplot(baseage,
                          aes(x=Type_Arret,fill=Age)) +
  geom_bar(stat="count",position = "dodge")
graph.Age

###########################################################################
#################Arrêts de travail en fonction de l'age####################
###########################################################################

c(baseage$NOM_ASSURE,baseage$PRENOM_ASSURE)
summary(baseage)

merge(baseage$NOM_ASSURE,baseage$PRENOM_ASSURE)
help("merge")

head(bdd_etude_ITT5$NOM_ASSURE)

bdd_etude_ITT5$Survenance_Annee

#Calculer la durée annuelle moyenne des arrêts de travail
# Duree = date fin couv - date survenance

bdd$LIB_CONTRACTANT





