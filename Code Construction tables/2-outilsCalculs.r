#-----------------------------------------------------------------------------
#Fonctions utilitaires pour les calculs
#-----------------------------------------------------------------------------
#Espérance de maintien dans l'état limitée à m périodes (mois ou années)
esperanceMaintien=function(Q,m){

  L=matrix(nrow=nrow(Q)+1,ncol=ncol(Q),data=1)
  for (a in 2:nrow(L)){
   L[a,]=L[a-1,]*(1-Q[a-1,])
  }

  e=matrix(nrow=nrow(L)-1,ncol=ncol(L),data=0)
  for (a in 1:(nrow(L)-2)){
     e[a,]=colSums(L[(a+1):min(a+m,nrow(L)),])/L[a,]
    }
  e[is.na(e)]=0
  return(e) 
  }
  
#-----------------------------------------------------------------------------
#volatiité du maintien dans l'état limitée à périodes (mois ou années)
volatiliteMaintien=function(Q,m){

  L=matrix(nrow=nrow(Q)+1,ncol=ncol(Q),data=1)
  for (a in 2:nrow(L)){
   L[a,]=L[a-1,]*(1-Q[a-1,])
  }

  v=matrix(nrow=nrow(L)-1,ncol=ncol(L),data=0)
  
  for (a in 1:(nrow(L)-2)){
    k=a:min(a+m-1,nrow(L)-1)
    v[a,]=colSums(2*(k-a+1/2)*L[(a+1):min(a+m,nrow(L)),])/L[a,]
    }
  s=sqrt(v-esperanceMaintien(Q,m)^2)
  s[is.na(s)]=0
  return(s) 
  }

#-----------------------------------------------------------------------------
#Espérance du nombre de sorties d'incapacité vers l'invalidité pendant m périodes (mois ou années)
esperanceSorties=function(lINC,qTRA,m){

  nc=min(ncol(qTRA),ncol(lINC))
  e=matrix(nrow=nrow(qTRA)-1,ncol=nc,data=0)
  for (a in 1:(nrow(qTRA)-1)){
     e[a,]=colSums(lINC[(a+1):min(a+m,nrow(lINC)),1:nc]*qTRA[a:min(a+m-1,nrow(lINC)-1),1:nc])/lINC[a,1:nc]
    }
  e[is.na(e)]=0
  return(e) 
  }

