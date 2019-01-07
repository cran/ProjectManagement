#' @title Stochastic projects
#' @description This function calculates the average duration time for a stochastic project and the activities criticality index. It also plots in a histogram the duration of the project, as well as the estimate density and the normal density.
#' @param precedence A matrix that indicates the order of precedence between activities. If the value \eqn{(i,j)} is equal to 1 then \eqn{i} precedes \eqn{j}.
#' @param distribution Type of distribution that each activities' duration has. It can be NORMAL, TRIANGLE, EXPONENTIAL, UNIFORM and EMPIRICAL.
#' @param values Matrix with the parameters corresponding to the distribution associated with the duration for each activity. Considering i as an activity we have the following cases. If the distribution is TRIANGLE, then (i, 1) it is the minimum value, (i, 2) the maximum value and (i, 3) the mode. If the distribution is NORMAL, (i, 1) is the mean and (i, 2) the variance. If the distribution is EXPONENTIAL, then (i, 1) is the \eqn{\lambda} parameter. If the distribution is UNIFORM, (i, 1) it is the minimum value and (i, 2) the maximum value. Finally, if the distribution is EMPIRICAL, then (i,j), for all \eqn{j\in \{1,...,M\}} such that \eqn{M>0}, is the sample.
#' @param percentile Percentile used to calculate the maximum time allowed for the duration of the project (Default=0.95).
#' @param compilations Number of compilations that the function will use for average calculations (Default=1000).
#' @export
#' @return Two values, average duration time and the maximum time allowed, a critically index vector and a durations histogram.
#' @examples
#' precedence<-matrix(c(0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#' distribution<-c("TRIANGLE","TRIANGLE","TRIANGLE","TRIANGLE","EXPONENTIAL")
#' values<-matrix(c(1,3,2,1/2,3/2,1,1/4,9/4,1/2,3,5,4,1/2,0,0),nrow=5,byrow=TRUE)
#' percentile<-0.95
#' stochastic.pert(precedence,distribution,values,percentile)
#'
stochastic.pert<-function(precedence,distribution,values,percentile=0.95,compilations=1000){

  or1<-order(organize(precedence)$Order[,2])
  or2<-organize(precedence)$Order[,2]
  precedence<-organize(precedence)$Precedence
  activities<-c(1:dim(precedence)[1])
  n<-length(activities)
  distribution<-distribution[or2]
  values<-values[or2,]

  tiempo.computacion<-numeric(compilations)
  caminos.criticos<-matrix(0,nrow=compilations,ncol=length(activities))
  C<-numeric(length(activities))
  n<-length(activities)
  tiempo.early<-rep(0,n)
  tiempo.last<-rep(0,n)
  duration<-matrix(0,nrow=compilations,ncol=length(activities))

  ii<-as.logical(colSums(precedence))
  iii<-activities[ii]
  nn<-length(iii)
  if(nn>0){
  prec<-matrix(0,nrow=nn,ncol=n-1)
  for(j in 1:length(iii)){
    prec[j,1:length(which(precedence[,iii[j]]==1))]<-which(precedence[,iii[j]]==1)
  }
  prec<-prec[,as.logical(colSums(prec)),drop=FALSE]
  }
  ii1<-as.logical(rowSums(precedence))
  iii1<-activities[ii1]
  nn1<-length(iii1)
  if(nn1>0){
  prec1<-matrix(0,nrow=length(iii1),ncol=n-1)

  for(j in 1:length(iii1)){
    prec1[j,1:length(which(precedence[iii1[j],]==1))]<-which(precedence[iii1[j],]==1)
  }
  prec1<-prec1[,as.logical(colSums(prec1)),drop=FALSE]
  }

  for(j in 1:length(activities)){
    {
    if(distribution[j]=="NORMAL"){
      duration[,j]<-c(rnorm(compilations,values[j,1],values[j,2]))
    }
    else if(distribution[j]=="TRIANGLE"){
      duration[,j]<-c(rtriangle(compilations,values[j,1],values[j,2],values[j,3]))
    }
    else if(distribution[j]=="EXPONENTIAL"){
      duration[,j]<-c(rexp(compilations,values[j,1]))
    }
    else if(distribution[j]=="UNIFORM"){
      duration[,j]<-c(runif(compilations,values[j,1],values[j,2]))
    }
    else if(distribution[j]=="BETA"){
      duration[,j]<-c(rbeta(compilations,values[j,1],values[j,2]))
    }
    else if(distribution[j]=="EMPIRICAL"){
      duration[,j]<-  rnorm(compilations,sample(values[j,],size=compilations,replace=TRUE),density(values[j,])$bw)
    }
  }
}
  for(i in 1:compilations){

  if(nn>0){
    for(j in 1:length(iii)) {
      tiempo.early[iii[j]]=max(tiempo.early[prec[j,]]+duration[i,prec[j,]]);
    }
}

    tiempo.last[activities[ii1==FALSE]]<-max(tiempo.early+duration[i,])
    if(nn1>0){
    for(j in length(iii1):1) {
      tiempo.last[iii1[j]]=min(tiempo.last[prec1[j,]]-duration[i,prec1[j,]]);
    }
}

    tiempo.computacion[i]<-max(tiempo.early+duration[i,])
    caminos.criticos[i,]<-(tiempo.last-tiempo.early-duration[i,])

  }
  delta<-quantile(ecdf(tiempo.computacion),percentile,names=FALSE)

  caminos.criticos<-round(caminos.criticos,4)

  for(i in 1:length(activities)){
    C[i]<-length(which(caminos.criticos[,i]==0))
  }

  cat("Average time of the project = ", mean(tiempo.computacion), "\n")
  cat(" ","\n")
  cat("Maximum duration allowed of the project = ", delta, "\n")
  A<-matrix(c((C[or1]/compilations)*100),ncol=length(activities),byrow=TRUE)
  colnames(A)=c(activities)
  rownames(A)=c("Criticality index by activity  ")
  cat(" ","\n")
  hist(tiempo.computacion,freq=F,xlab="Project duration times",main="histogram",ylim=c(0,max(density(tiempo.computacion)$y)+0.2))
  lines(density(tiempo.computacion),lwd=2,col='red')
  lines(seq(min(tiempo.computacion),max(tiempo.computacion),0.01),dnorm(seq(min(tiempo.computacion),max(tiempo.computacion),0.01),mean(tiempo.computacion),sd(tiempo.computacion)),lwd=2,col="black")
  legend("topleft", legend=c("Estimated density", "Normal density"), lty=c(1,1),lwd=c(2,2),col=c("red", "black"))
  return(round(A,5))
}


