###########################################################################
####Library
library(knitr)
library(ggplot2)

#install.packages("survminer")
library(fitdistrplus)

library(actuar)
library(survival)
library(gridExtra)
library(survminer)
####################Import data########################################################
setwd("C:/Users/KDatsi/OneDrive - EY/Documents/PERSONNEL/BDD Mémoire Actuariat Absentéisme")
bdd_ITT <- read.csv("bdd_etude_ITT.csv")
bdd_ITP <- read.csv("bdd_etude_ITP.csv")

bdd_ITT <- subset(bdd_ITT, Nb_jour_couvert>0)

#################       Censure à droite des dates     ####################



# Définir l'année pour laquelle vous souhaitez obtenir le nombre total de jours d'arrêt
year <- 2020

# Sélectionner les données pour cette année, incluant les arrêts dont la date de début ou de fin se trouve en dehors de l'année
data_year <- subset(bdd_etude_nd, as.Date(bdd_etude_nd$DAT_SURVENANCE_SIN, "%Y-%m-%d") <= as.Date(paste0(year, "-12-31"), "%Y-%m-%d") & as.Date(bdd_etude_nd$FIN_PER_REGLEMENT, "%Y-%m-%d") >= as.Date(paste0(year, "-01-01"), "%Y-%m-%d"))

# Ajouter une colonne pour le nombre de jours d'arrêt pour chaque arrêt
data_year$jours_arrets <- as.numeric(pmax(0, difftime(pmin(as.Date(data_year$FIN_PER_REGLEMENT, "%Y-%m-%d"), as.Date(paste0(year, "-12-31"), "%Y-%m-%d")), pmax(as.Date(data_year$DAT_SURVENANCE_SIN, "%Y-%m-%d"), as.Date(paste0(year, "-01-01"), "%Y-%m-%d")), units = "days") + 1))

# Calculer le nombre total de jours d'arrêt pour l'année
total_jours_arrets <- sum(data_year$jours_arrets)

# Imprimer le nombre total de jours d'arrêt
print(total_jours_arrets)

data.AT <-data.frame()
agr_dt<-data.frame()
for (year in 2015:2022){
  data_year <- subset(bdd_etude_nd, as.Date(bdd_etude_nd$DAT_SURVENANCE_SIN, "%Y-%m-%d") <= as.Date(paste0(year, "-12-31"), "%Y-%m-%d") & as.Date(bdd_etude_nd$FIN_PER_REGLEMENT, "%Y-%m-%d") >= as.Date(paste0(year, "-01-01"), "%Y-%m-%d"))
  
  # Ajouter une colonne pour le nombre de jours d'arrêt pour chaque arrêt
  data_year$jours_arrets <- as.numeric(pmax(0, difftime(pmin(as.Date(data_year$FIN_PER_REGLEMENT, "%Y-%m-%d"), as.Date(paste0(year, "-12-31"), "%Y-%m-%d")), pmax(as.Date(data_year$DAT_SURVENANCE_SIN, "%Y-%m-%d"), as.Date(paste0(year, "-01-01"), "%Y-%m-%d")), units = "days") + 1))
  
  # Calculer le nombre total de jours d'arrêt pour l'année
  total_jours_arrets <- sum(data_year$jours_arrets)
  df_y<-data.frame(year,total_jours_arrets)
  
  data.AT <-rbind(data.AT,df_y)
  agr_dt_y <- aggregate(data_year$jours_arrets, by= list(data_year$cle_unique_dsn), sum)
  agr_dt_y <- cbind(agr_dt_y,rep(year,dim(agr_dt_y)[1]))
  
  colnames(agr_dt_y)<-c("Clé","Nb_J","Année")
  agr_dt<-rbind(agr_dt,agr_dt_y)
  
}

aggregate(agr_dt$Nb_J, by= list(agr_dt$Année), sum)

head(agr_dt)

agr_dt$Tx_Abs <- agr_dt$Nb_J/365.25

p<-ggplot(agr_dt, aes(x=as.factor(Année),y=Tx_Abs)) +geom_boxplot()
p

###### Fonction de répartion de la durée des arrêts

p<-ggplot(bdd_ITT, aes(x=`Survenance_Annee`)) + 
  geom_histogram(binwidth=1, fill="#762123",color="white")+
  scale_x_continuous(limits = c(2010,2023), breaks = seq(2011,2022, 1))+
  theme_classic()+
  labs(x = "Année de survenance",
       y = "Nombre de sinistres") 
p
ggsave("plot_survenance.jpg",width=20,height = 15,units="cm")

fct_rep <- ecdf(bdd_ITT$Nb_jour_couvert)

plot(fct_rep)

data_repartition <- data.frame(duree = seq(0, max(bdd_ITT$Nb_jour_couvert), length.out = 100),
                               repartition = fct_rep(seq(0, max(bdd_ITT$Nb_jour_couvert), length.out = 100)))

plot_frepartition=ggplot(data_repartition, aes(x = duree, y = repartition)) +
  geom_step(color = "#0072B2") +
  labs(title = "Fonction de répartition empirique de la durée des arrêts",
       x = "Durée d'arrêt de travail (jours)",
       y = "Fréquences") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
geom_vline(xintercept = 183, linetype = "dashed", color = "red")+
  scale_x_continuous(limits = c(0, 1100), breaks = seq(0, 1100, 100))
plot_frepartition
ggsave("Rep.jpg",width=25,height = 10,units="cm")



###Arrêts courte durée (<6 mois)

bdd_ITT_AC <- subset(bdd_ITT,Nb_jour_couvert < 183 & Nb_jour_couvert>0)

arret_court <-na.omit(bdd_ITT$Nb_jour_couvert)

###FIT de distribution

#fit_weibull <- fitdist(arret_court, "weibull")
#fit_poisson <- fitdist(arret_court, "pois")
#fit_logpoisson <- fitdist(arret_court, "logpois", start = list(lambda = mean(log(arret_court))))

fit_lognormal <- fitdist(arret_court, "lnorm")
fit_exp <- fitdist(arret_court, "exp")
fit_gamma <- fitdist(arret_court, "gamma")

#Inverse gaussien
start <- list(mean=0.5,shape= 1)


fit_invgauss<- fitdist(arret_court, "invgauss",start = start)

#plot(fit_invgauss)

#hist(arret_court, prob = TRUE, main = "Histogramme des données et densité ajustée")



#denscomp(list(gamam=fit_gamma, exp=fit_exp,lognorm=fit_lognormal,invgauss = fit_invgauss), legendtext = c("Gamma","Expo","Log-normal","Inverse-gaussienne"))


###Tracé du graphe comparaison des densités####

mu <- as.numeric(fit_lognormal$estimate[1])
sigma <- as.numeric(fit_lognormal$estimate[2])


x <- seq(min(bdd_ITT$Nb_jour_couvert), max(bdd_ITT$Nb_jour_couvert), length.out = 100)
y_ln <- dlnorm(x, mu, sigma)
y_exp<-dexp(x,fit_exp$estimate[1])
y_gamma<-dgamma(x,fit_gamma$estimate[1],fit_gamma$estimate[2])
y_invgauss<-dinvgauss(x,fit_invgauss$estimate[1],fit_invgauss$estimate[2])



# Tracé de l'histogramme et de la courbe de densité
ln_plot<-ggplot(bdd_ITT, aes(x = Nb_jour_couvert)) + 
  geom_histogram(aes(y = ..density..), binwidth = 7.5, fill = "#762123", color = "white", alpha = 1) + 
  geom_line(data = data.frame(x,y_ln), aes(x = x, y = y_ln),  color = "blue",size=1) +
  xlab("Nombre de jours d'arrêts") + 
  ylab("Fréquence") + 
  ggtitle("Log-Normale")+theme(plot.title = element_text(hjust = 0.5,face = "bold"))

exp_plot<-ggplot(bdd_ITT, aes(x = Nb_jour_couvert)) + 
  geom_histogram(aes(y = ..density..), binwidth = 7.5, fill = "#762123", color = "white", alpha = 1)+
  ylab("Fréquence") + 
  ggtitle("Exponentielle")+theme(plot.title = element_text(hjust = 0.5,face = "bold"))

gamma_plot<-ggplot(bdd_ITT, aes(x = Nb_jour_couvert)) + 
  geom_histogram(aes(y = ..density..), binwidth = 7.5, fill = "#762123", color = "white", alpha = 1) + 
  geom_line(data = data.frame(x,y_gamma), aes(x = x, y = y_gamma), color = "blue",size=1) +
  xlab("Nombre de jours d'arrêts") + 
  ylab("Fréquence") + 
  ggtitle("Gamma")+theme(plot.title = element_text(hjust = 0.5,face = "bold"))

invgauss_plot<- ggplot(bdd_ITT, aes(x = Nb_jour_couvert)) + 
  geom_histogram(aes(y = ..density..), binwidth = 7.5, fill = "#762123", color = "white", alpha = 1) + 
  geom_line(data = data.frame(x,y_invgauss), aes(x = x, y = y_invgauss), color = "blue",size=1) + 
  xlab("Nombre de jours d'arrêts") + 
  ylab("Fréquence") + 
  ggtitle("Inverse Gaussienne",)+theme(plot.title = element_text(hjust = 0.5,face = "bold"))


grid.arrange(ln_plot, invgauss_plot,gamma_plot,exp_plot, ncol = 2)

#### Tracer des Q-Q plot ####

#Loi lognormale


qqcomp(list(fit_lognormal,fit_invgauss,fit_gamma,fit_exp),
       legendtext=c("lognormal","inverse-gaussienne","gamma","exponentielle"),
       fitcol=c("red","green","blue","grey"))

qq_gamma<-qqcomp(list(fit_gamma),xlab = "Quantiles théoriques",ylab = "Quantiles empiriques",plotstyle = "ggplot")+ggtitle("Gamma")+theme(plot.title = element_text(face = "bold"))
qq_ln<-qqcomp(list(fit_lognormal),xlab = "Quantiles théoriques",ylab = "Quantiles empiriques",plotstyle = "ggplot")+ggtitle("Log-Normale")+theme(plot.title = element_text(face = "bold"))
qq_invgauss<-qqcomp(list(fit_invgauss),xlab = "Quantiles théoriques",ylab = "Quantiles empiriques",plotstyle = "ggplot")+ggtitle("Inverse Gaussienne")+theme(plot.title = element_text(face = "bold"))
qq_exp<-qqcomp(list(fit_exp),xlab = "Quantiles théoriques",ylab = "Quantiles empiriques",plotstyle = "ggplot")+ggtitle("Exponentielle")+theme(plot.title = element_text(face = "bold"))


grid.arrange(qq_ln, qq_invgauss,qq_gamma,qq_exp, ncol = 2)

#### Tracer des CDF ####

cdfcomp(list(fit_lognormal,fit_invgauss,fit_gamma,fit_exp),
       legendtext=c("lognormal","inverse-gaussienne","gamma","exponentielle"),
       fitcol=c("red","green","blue","grey"))

cdf_gamma<-cdfcomp(list(fit_gamma),xlab = "Quantiles théoriques",ylab = "Quantiles empiriques",plotstyle = "ggplot")+ggtitle("Gamma")+theme_gray()+theme(plot.title = element_text(hjust = 0.5,face = "bold"))
cdf_ln<-cdfcomp(list(fit_lognormal),xlab = "Quantiles théoriques",ylab = "Quantiles empiriques",plotstyle = "ggplot")+ggtitle("Log-Normale")+theme_gray()+theme(plot.title = element_text(hjust = 0.5,face = "bold"))
cdf_invgauss<-cdfcomp(list(fit_invgauss),xlab = "Quantiles théoriques",ylab = "Quantiles empiriques",plotstyle = "ggplot")+ggtitle("Inverse Gaussienne")+theme_gray()+theme(plot.title = element_text(hjust = 0.5,face = "bold"))
cdf_exp<-cdfcomp(list(fit_exp),xlab = "Quantiles théoriques",ylab = "Quantiles empiriques",plotstyle = "ggplot")+ggtitle("Exponentielle")+theme_gray()+theme(plot.title = element_text(hjust = 0.5,face = "bold"))


grid.arrange(cdf_ln, cdf_invgauss,cdf_gamma,cdf_exp, ncol = 2)










bdd_ITT$MOTIF_SITUATION<-as.factor(bdd_ITT$MOTIF_SITUATION)
levels(bdd_ITT$MOTIF_SITUATION)
t<-bdd_ITT$MOTIF_SITUATION
levels(t)<-c("0","0","1","1","1","1","0","1","0","0","1","0","1","1","1","1","1","1","1","0","0","0","1","1","1","1","0","1")
bdd_ITT$status_IT<-t



surv_obj <- Surv(bdd_ITT$Nb_jour_couvert, bdd_ITT$status_IT)
fit <- survfit(surv_obj ~ 1, conf.int = 0.95)
ggsurvplot(fit, data = bdd_ITT, color = "red", legend.title = "Status", legend.labs = c("Survived", "Censored"))

#dist(bdd_ITT$Nb_jour_couvert)




















km_estimateur <- survfit(Surv(Nb_jour_couvert,status_IT)~ AGE,data=bdd_ITT)
km_estimateur$n
max(bdd_ITT$AGE)
plot(km_estimateur, xlab="Age", ylab="Survival Probability")

fit<-survfit(Surv(bdd_ITT$Nb_jour_couvert,as.factor(bdd_ITT$SITUATION_CONTR))~ 1,data =data.frame(bdd_ITT$Nb_jour_couvert,t))

fit$time

help("survfit")

xlab <- "Age"
ylab <- "Ancienneté"
zlab <- "Estimateur de Kaplan-Meier"




plot_ly(x = fit$time, y = incapacity_rate, z = fit$upper, mode = "markers", type = "scatter3d", 
        marker = list(size = 4, color = "red", line = list(color = "black", width = 1))) %>%
  layout(scene = list(xaxis = list(title = xlab),
                      yaxis = list(title = ylab),
                      zaxis = list(title = zlab)))


sum(is.na(t))
table(t)



grepl("ENCOURS",c("ENCOURS    ","ENCOUR","ENCOURS
                  "))
unique(bdd_ITT$SITUATION_DOSSIER
       )
pass_inv<-c("Passage d'I.T.T. en I.T.P","Passage d'I.T.T. en Invalidite")

Motif<-bdd_ITT$MOTIF_SITUATION
Motif[Motif%in%pass_inv]<-"Invalidité"
Motif[Motif != " " & Motif !="Invalidité"]<-"Terminé"
Motif[Motif == " " ]<-"En cours"
unique(Motif)

data_inc <- data.frame(Sexe=bdd_ITT$SEXE,CSP=bdd_ITT$CAT_PRO,DateNaissance=bdd_ITT$DAT_NAISSANCE_ASS,DateSurvenance=bdd_ITT$DAT_SURVENANCE_SIN,DateEntree=bdd_ITT$DEB_PER_REGLEMENT,DateSortie=bdd_ITT$FIN_PER_REGLEMENT,MotifReprise=Motif)
table(subset(bdd_ITT,MOTIF_SITUATION==" ")$SITUATION_DOSSIER)

         
head(bdd_ITT)


pass_inv<-c("Passage d'I.T.T. en I.T.P","Passage d'I.T.T. en Invalidite")

Motif<-bdd$MOTIF_SITUATION
Motif[Motif%in%pass_inv]<-"Invalidité"
Motif[Motif != " " & Motif !="Invalidité"]<-"Terminé"
Motif[Motif == " " ]<-"En cours"
unique(Motif)

data_inc_b <- data.frame(Sexe=bdd$SEXE,CSP=bdd$CAT_PRO,DateNaissance=bdd$DAT_NAISSANCE_ASS,DateSurvenance=bdd$DAT_SURVENANCE_SIN,DateEntree=bdd$DEB_PER_REGLEMENT,DateSortie=bdd$FIN_PER_REGLEMENT,MotifReprise=Motif)

table(subset(bdd,MOTIF_SITUATION==" ")$SITUATION_DOSSIER)
