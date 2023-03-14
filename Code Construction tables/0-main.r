#-------------------------------------------------------------------------------
#########		Risque syst?matique en arr?t de travail                               
#-------------------------------------------------------------------------------
#v1.8 du 27/05/2017
# - correction du calcul de la fonction de survie
#v1.7 du 23/05/2017
# - mise en conformit? avec la v1.7 de la note
# - passage des incapables au-del? de 36 en invalidit?
#v1.5 du 20/05/2017
# - prise en compte d'un d?calage d'un mois entre la transition et la p?riode d'invalidit? (cf. AT v1.5)
#v1.4 du 16/05/2017
# - correction des probabilit?s de sortie d'invalidit?,maintenant mensualis?es 
#v1.3 du 15/05/2017
# - optimisation du code
#v1.2 du 15/05/2017
# - Suppression des doubles-comptages
#v1.1 du 14/05/2017
# - ajout des tables BCAC 2010
# - optimisation du code
# - sauvegarde de la table de maintien
# - correction du calcul de P(Tx=a)
#v1.0 : Mise en place
#-------------------------------------------------------------------------------
rm(list=ls(all=TRUE))

dossier="C:/Users/KDatsi/OneDrive - EY/Documents/PERSONNEL/Memoire/Code Construction tables"
#S?parateurs
sepDecimal=","
# Dossier de stockage
dossierTables=paste(dossier,"/tables",sep="")
dossierFigures=paste(dossier,"/figures",sep="")
dossierResultats=paste(dossier,"/resultats",sep="")

source(paste(dossier,"/2-outilsCalculs.r",sep=""))

#-------------------------------------------------------------------------------
#Initialisation de la gestion des figures
sauveFigure=function(){
  numeroFigure=length(list.files(dossierFigures))+1
  png(file=paste(dossierFigures,"/fig",numeroFigure,".png", sep=""), width  = 2*2100, height = 2100, res=300, pointsize= 12)
  }
#-------------------------------------------------------------------------------
#Lois BCAC
anneeBCAC=2013
fichierBCAC_INC=paste(dossierTables,"/lBCAC",anneeBCAC,"_INC.csv",sep="")
fichierBCAC_INV=paste(dossierTables,"/lBCAC",anneeBCAC,"_INV.csv",sep="")
fichierBCAC_TRA=paste(dossierTables,"/qBCAC",anneeBCAC,"_TRA.csv",sep="")
#-------------------------------------------------------------------------------
#Param?tres des calculs (incapacit?)
ageMin=20
ageMax=65
ancMin=0
ancMax=36
nbColonnes=ageMax-ageMin+1
#-------------------------------------------------------------------------------
#Param?tres des calculs (invalidit?)
ageMinINV=20
ageMaxINV=64
ancMinINV=0
ancMaxINV=45
ageRetraite=67
#-------------------------------------------------------------------------------
#Param?tres des calculs (transitions)
ageMinTRA=ageMin
ageMaxTRA=ageMax
ancMinTRA=ancMin
ancMaxTRA=ancMax
#-------------------------------------------------------------------------------
#Autres param?tres
nbJoursAnnee=365.25
nbJoursMois=365.25/12
#-------------------------------------------------------------------------------
#Chargement des diff?rentes tables du BCAC
#Incapacit?
lBCAC_INC=as.matrix(read.table(fichierBCAC_INC,header=TRUE,dec=",",sep=";"))
qBCAC_INC=1-lBCAC_INC[2:nrow(lBCAC_INC),]/lBCAC_INC[1:(nrow(lBCAC_INC)-1),]
colnames(qBCAC_INC)=c(ageMin:ageMax)
sauveFigure()
persp((ancMin:(ancMax-1)),(ageMin:ageMax),qBCAC_INC[(ancMin+1):ancMax,],theta=-40,phi=15,xlab='anciennet?',ylab='?ge de survenance',zlab='qx',col="lightgreen",expand=0.5,shade=0.8,ticktype="detailed")
title("Incapacit? - Taux de sortie (BCAC)")
dev.off()
#-------------------------------------------------------------------------------
#Invalidit?
getBCAC_INV=function(fichierBCAC_INV,ageDepart){

  res=list()

  lBCAC_INV=as.matrix(read.table(fichierBCAC_INV,header=TRUE,dec=",",sep=";"))
  rownames(lBCAC_INV)=c(ancMinINV:ancMaxINV)
  colnames(lBCAC_INV)=c(ageMinINV:ageMaxINV)
  for (a in (ancMinINV:ancMaxINV)){
    for (x in (ageMinINV:ageMaxINV)){
      if ((x+a)>ageDepart){lBCAC_INV[a-ancMinINV+1,x-ageMinINV+1]=0}
      }
    }
  qBCAC_INV=1-lBCAC_INV[2:(nrow(lBCAC_INV)),]/lBCAC_INV[1:(nrow(lBCAC_INV)-1),]
  qBCAC_INV[is.na(qBCAC_INV)]=1
  colnames(qBCAC_INV)=c(ageMinINV:ageMaxINV)
  res$q=qBCAC_INV
  res$L=lBCAC_INV
  return(res)
  }

BCAC_INV=getBCAC_INV(fichierBCAC_INV,ageRetraite)
lBCAC_INV=BCAC_INV$L  
qBCAC_INV=BCAC_INV$q
z=qBCAC_INV;z[z==1]=0
sauveFigure()
persp(x=0:(nrow(qBCAC_INV)-1),y=ageMinINV:ageMaxINV,z=z,theta=-40,phi=15,xlab='anciennet?',ylab='?ge de d?but',zlab='qx',col="lightgreen",expand=0.5,shade=0.8,ticktype="detailed")
title(main="Invalidit? - Taux de sortie (BCAC)",sub="Les taux ?gaux ? un sur la diagonale ne sont pas affich?s")
dev.off()
#-------------------------------------------------------------------------------
#Transitions
qBCAC_TRA=as.matrix(read.table(fichierBCAC_TRA,header=TRUE,dec=",",sep=";"))
colnames(qBCAC_TRA)=c(ageMinTRA:ageMaxTRA)
sauveFigure()
persp(x=0:(nrow(qBCAC_TRA)-1),y=ageMinTRA:ageMaxTRA,z=qBCAC_TRA,theta=-40,phi=15,xlab='anciennet?',ylab='?ge de d?but',zlab='qx',col="lightgreen",expand=0.5,shade=0.8,ticktype="detailed")
title("Transitions - Probabilit?s de transition (BCAC)")
dev.off()

sauveFigure()
persp(x=0:(nrow(qBCAC_TRA)-2),y=ageMinTRA:ageMaxTRA,z=qBCAC_TRA[1:(nrow(qBCAC_TRA)-1),],theta=-40,phi=15,xlab='anciennet?',ylab='?ge de d?but',zlab='qx',col="lightgreen",expand=0.5,shade=0.8,ticktype="detailed")
title("Transitions - Probabilit?s de transition (BCAC) - Hors dernier mois")
dev.off()
#-------------------------------------------------------------------------------
#Construction de la loi unique
source(paste(dossier,"/1-loiUnique.r",sep=""))
