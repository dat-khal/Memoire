#-------------------------------------------------------------------------------
#Construction d'une loi de maintien en AT
#-------------------------------------------------------------------------------
ageMinAT=ageMin
ageMaxAT=ageMax
ancMinAT=ancMin
ancMaxAT=ancMaxINV*12

lINC=lBCAC_INC
qINC=qBCAC_INC
qTRA=qBCAC_TRA
lINV=lBCAC_INV
qINV=qBCAC_INV

#-------------------------------------------------------------------------------
#calcul de P(Tx=a)
pAT=matrix(nrow=ancMaxAT-ancMinAT+1,ncol=ageMaxAT-ageMinAT+1,data=0)

for (age in ageMinAT:ageMaxAT){

  for (anc in ancMinAT:ancMaxAT){
  
    a=anc-ancMinAT+1
    x=age-ageMinAT+1
    
    if ((age+anc/12)<ageRetraite){
      
      if (anc<ancMax){pINC=lINC[a,x]/lINC[1,x]*max(0,qINC[a,x]-qTRA[a,x])}else{pINC=0}

      pINV=0
      if (anc>0){     
        for (k in (ancMin):min(anc,ancMax)){
          xl=trunc(age+k/12)-ageMinAT+1
          al=trunc((anc-k-ancMinAT)/12)+1
          if ((xl<=ncol(qINV))&(al<=nrow(qINV))){
            qINV_mensuelle=1-(1-qINV[al,xl])^(1/12)
            if (k==ancMax){i=1}else{i=qTRA[k+1,x]}    #S'il reste des incapables apr?s 36 mois on les met en invalidit?
            pINV=pINV+lINC[k+1,x]/lINC[1,x]*i*lINV[al,xl]/lINV[1,xl]*qINV_mensuelle
            }
          }
        }
      pAT[a,x]=pINC+pINV 
      
      }    
    }   
  }
#-------------------------------------------------------------------------------
#Calcul des qx et des Lx
lAT=matrix(nrow=ancMaxAT-ancMinAT+1,ncol=ageMaxAT-ageMinAT+1,data=0)
colnames(lAT)=c(ageMinAT:ageMaxAT)
for (age in ageMinAT:ageMaxAT){
  for (anc in ancMinAT:(ancMaxAT-1)){ 
    a=anc-ancMinAT+1
    x=age-ageMinAT+1
    if ((age+anc/12)<ageRetraite){lAT[a+1,x]=1-sum(pAT[1:a,x])}
    }  
  }
  
lAT[1,]=rep(1,ncol(lAT))
#lAT[2:nrow(lAT),ncol(lAT)]=rep(0,nrow(lAT)-1)  

lAT[lAT==Inf]=0
lAT[is.na(lAT)]=0
qAT=1-lAT[2:nrow(lAT),]/lAT[1:(nrow(lAT)-1),]
qAT[is.na(qAT)]=1
lAT[lAT==-Inf]=1
#-------------------------------------------------------------------------------
#Esp?rances de maintien (en mois)
espAT=esperanceMaintien(qAT,ancMaxAT)
espAT[espAT==0]=NA
espAT[is.na(espAT)]=0
colnames(espAT)=c(ageMinAT:ageMaxAT)
sauveFigure()
persp((ancMinAT:(ancMaxAT-1)),(ageMinAT:ageMaxAT),espAT[(ancMinAT+1):ancMaxAT,],theta=40,phi=15,xlab='anciennet?',ylab='?ge de survenance',zlab='ex (en mois)',col="lightgreen",expand=0.5,shade=0.8,ticktype="detailed")
titre=paste("Arr?t de travail - Dur?es de maintien BCAC - Retraite : ",ageRetraite,sep="")
title(main=titre)
dev.off()

#-------------------------------------------------------------------------------
#Sauvegarde de la table
write.table(lAT,paste(dossierResultats,"/lBCAC_",anneeBCAC,"_",ageRetraite,".csv",sep=""),sep=";",row.names=FALSE, dec=",")  
