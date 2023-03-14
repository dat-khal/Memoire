###########################################################################
####Library
library(knitr)
library(ggplot2)
library(readxl)


####################Import data########################################################
setwd("C:/Users/KDatsi/OneDrive - EY/Documents/PERSONNEL/BDD Mémoire Actuariat Absentéisme")
bdd_ITT <- read.csv("bdd_etude_ITT.csv")
bdd_ITP <- read.csv("bdd_etude_ITP.csv")
tables_IT_BCAC <- read_excel("C:/Users/KDatsi/OneDrive - EY/Documents/Travail/Calcul PM AXA/Code R/Tables.xlsx",sheet="BCAC2013_INCAP_66a")

bdd_etude_ITT=bdd_ITT
###################### Graphe evol annee ######
bdd_etude_ITT$Survenance_Annee <- as.numeric(format(bdd_etude_ITT$DAT_SURVENANCE_SIN,"%Y"))
bdd_etude_ITT$Survenance_Mois <- format(bdd_etude_ITT$DAT_SURVENANCE_SIN,"%B")

bdd_etude_ITT$Survenance_Annee <- as.numeric(format(bdd_etude_ITT$DAT_SURVENANCE_SIN,"%Y"))
bdd_etude_INV$Survenance_Annee <- as.numeric(format(bdd_etude_INV$DAT_SURVENANCE_SIN,"%Y"))

bdd_etude_ITT$Survenance_Mois <- format(bdd_etude_ITT$DAT_SURVENANCE_SIN,"%B")
bdd_etude_INV$Survenance_Mois <- format(bdd_etude_INV$DAT_SURVENANCE_SIN,"%B")

bdd_etude_ITT2<-subset(bdd_etude_ITT,Survenance_Annee <= 2021 & Survenance_Annee>2012)
bdd_etude_INV2<-subset(bdd_etude_INV,Survenance_Annee <= 2021 & Survenance_Annee>2012)

bdd_etude_ITT2<-subset(bdd_etude_ITT,Survenance_Annee <= 2022 & Survenance_Annee>2010) 

bdd_etude_ITT2$Survenance_Annee=as.factor(bdd_etude_ITT2$Survenance_Annee)

p<-ggplot(bdd_etude_ITT2, aes(x=`Survenance_Annee`)) + 
  geom_histogram(binwidth=1, fill="navy", color="yellow")
p


graph.evol.annee <- ggplot(bdd_etude_ITT2,
                           aes(x=Survenance_Annee)) +
  geom_bar(stat="count",position = "dodge",fill="navy", color="yellow")
graph.evol.annee



p<-ggplot(bdd_ITT, aes(x=`Survenance_Annee`)) + 
  geom_histogram(binwidth=1, fill="#762123",color="white")+
  scale_x_continuous(limits = c(2010,2023), breaks = seq(2011,2022, 1))+
  theme_classic()+
  labs(x = "Année de survenance",
       y = "Nombre de sinistres") 
p

table(bdd_ITT$Survenance_Annee)

tx_arret_tab=table(subset(bdd_ITT,Survenance_Annee <= 2021 & Survenance_Annee>=2017)$Survenance_Annee)

ef_portefeuill=c(596926,596951,629980,666012,674649)

tx_arret_tab=tx_arret_tab/ef_portefeuill
tx_arret_tab=data.frame(tx_arret_tab)
colnames(tx_arret_tab)=c("Annee", "Taux")

p2<-ggplot(tx_arret_tab, aes(x = Annee, y =Taux )) +
  geom_bar(stat = "identity",binwidth=1, fill="#762123",color="white")+
  theme_classic()+
  labs(x = "Année de survenance",
       y = "Taux de sinistres") 
p2

mean(tx_arret_tab$Taux[2:5]/tx_arret_tab$Taux[1:4])  

nb.2020=table(subset(bdd_etude_ITT,Survenance_Annee==2020)$Survenance_Mois)
nb.2021=table(subset(bdd_etude_ITT,Survenance_Annee==2021)$Survenance_Mois)
nb.2018=table(subset(bdd_etude_ITT,Survenance_Annee==2018)$Survenance_Mois)
nb.2019=table(subset(bdd_etude_ITT,Survenance_Annee==2019)$Survenance_Mois)


nb.2020/nb.2018
nb.2021/nb.2018
mean(nb.2021/nb.2019)
###########################################################################


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

######################## Trouver le sexe d'un assuré via son prénom#################


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



##############Saisonnalité des arrêts de travail ##################

bdd_etude_ITT3<-subset(bdd_etude_ITT,Survenance_Annee <= 2021 & Survenance_Annee>2017) 

bdd_etude_ITT3$Survenance_Saison <- as.factor(bdd_etude_ITT3$Survenance_Mois)
levels(bdd_etude_ITT3$Survenance_Saison)
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
bdd_etude_ITT3$Survenance_Annee<-as.factor(bdd_etude_ITT3$Survenance_Annee)

motifs <- c("22", "23", "24", "25")

graph.sais.mois <- ggplot(bdd_etude_ITT3,
                          aes(x=Survenance_Mois,,fill=Survenance_Annee)) +
  geom_bar(stat="count",position = "dodge")+
  scale_fill_manual(values = c("lightblue", "navy", "#762123", "#E7B800"))+
  theme_classic()+
  labs(x = "Mois de survenance",
       y = "Nombre de sinistres",
       fill = "Année de survenance") 

graph.sais.mois + scale_x_discrete(limits=c("janvier",'février',"mars","avril","mai","juin","juillet","août","septembre","octobre","novembre","décembre"))



graph.sais.saison <- ggplot(bdd_etude_ITT3,
                            aes(x=Survenance_Saison,fill=Survenance_Annee)) +
  geom_bar(stat="count",position = "dodge")+
  scale_fill_manual(values = c("lightblue", "navy", "#762123", "#E7B800"))+
  theme_classic()+
  labs(x = "Saison",
       y = "Nombre de sinistres",
       fill = "Année de survenance") 
graph.sais.saison+ scale_x_discrete(limits=c("printemps","ete","automne","hiver"))


 table_saison_2018 <- table(subset(bdd_etude_ITT3,Survenance_Annee == 2018)$Survenance_Saison)
table_saison_2019 <- table(subset(bdd_etude_ITT3,Survenance_Annee == 2019)$Survenance_Saison)
table_saison_2018
table_saison_2019

table_saison_2018["hiver"]/table_saison_2018["ete"] #Hausse de 34,7% entre ete et hiver

table_saison_2018["automne"]/table_saison_2018["ete"]


####################Typologie des arrêts de travail########################

bdd_etude_ITT3$Nb_jour_couvert

type.arret.travail<-function(x){
  if(is.na(x)){return(as.factor("NA"))}
  else if(x<=10){return(as.factor("court"))}
  else if(x>10 & x<=90){return(as.factor("moyen"))}
  else return(as.factor("long"))
}

bdd_etude_ITT2$Type_Arret = sapply(bdd_etude_ITT2$Nb_jour_couvert,type.arret.travail)

table(bdd_etude_ITT2$Type_Arret)

bdd_etude_ITT4<-subset(bdd_etude_ITT3,Survenance_Annee <= 2021 & Survenance_Annee>2017 & Type_Arret!="NA") 
bdd_etude_ITT2$Survenance_Annee<-as.factor(bdd_etude_ITT2$Survenance_Annee)



bdd_etude_ITT2=subset(bdd_etude_ITT2,Survenance_Annee > 2011)
graph.type.arret <- ggplot(bdd_etude_ITT2,
                           aes(x=Type_Arret,fill=Survenance_Annee)) +
  geom_bar(stat="count",position = "dodge")+scale_fill_brewer(palette="RdGy")+
  theme_classic()+
  labs(x = "Type d'arrêt",
       y = "Nombre de sinistres",
       fill = "Année de survenance") 
graph.type.arret

graph.type.arret.jeune <- ggplot(subset(bdd_etude_ITT2,AGE<=29),
                           aes(x=Type_Arret,fill=Survenance_Annee)) +
  geom_bar(stat="count",position = "dodge")+scale_fill_brewer(palette="RdGy")+
  theme_classic()+
  labs(x = "Type d'arrêt",
       y = "Nombre de sinistres",
       fill = "Année de survenance") 
graph.type.arret.jeune

nb.2020_t=table(subset(bdd_etude_ITT2,Survenance_Annee==2020 & AGE<=29)$Type_Arret)
nb.2021_t=table(subset(bdd_etude_ITT2,Survenance_Annee==2021 & AGE<=29)$Type_Arret)
nb.2018_t=table(subset(bdd_etude_ITT2,Survenance_Annee==2018 & AGE<=29)$Type_Arret)
nb.2019_t=table(subset(bdd_etude_ITT2,Survenance_Annee==2019 & AGE<=29)$Type_Arret)
nb.2017_t=table(subset(bdd_etude_ITT2,Survenance_Annee==2017 & AGE<=29)$Type_Arret)
nb.2022_t=table(subset(bdd_etude_ITT2,Survenance_Annee==2022 & AGE<=29)$Type_Arret)




###############Arrêts de travail en fonction du collègue###################


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



#################Arrêts de travail en fonction de l'age####################


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


#################Arrêts de travail en fonction de l'age####################


c(baseage$NOM_ASSURE,baseage$PRENOM_ASSURE)
summary(baseage)

merge(baseage$NOM_ASSURE,baseage$PRENOM_ASSURE)
help("merge")

head(bdd_etude_ITT5$NOM_ASSURE)

bdd_etude_ITT5$Survenance_Annee

#Calculer la durée annuelle moyenne des arrêts de travail
# Duree = date fin couv - date survenance

bdd$LIB_CONTRACTANT








library(plotly)
anciennete <- seq(0, 36, 1)
age <- seq(20, 75, 1)
z <- outer(anciennete, age, function(x, y) tables_IT_BCAC[y,x+2])
plot_ly(x = anciennete, y = age, z = incapacite, type = "surface")


xlab <- "Age"
ylab <- "Ancienneté"
zlab <- "Taux d'incapacité"

plot_ly(x = age, y = anciennete, z = incapacite, type = "surface", opacity = 1) %>%
  layout(scene = list(xaxis = list(title = xlab),
                      yaxis = list(title = ylab),
                      zaxis = list(title = zlab)))

incapacite<-as.matrix(tables_IT_BCAC[,2:38])
