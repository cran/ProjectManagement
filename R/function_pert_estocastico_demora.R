#' @title Problems of distribution of delay in stochastic projects
#' @description This function calculates the delay of a stochastic project, once it has been carried out. In addition, it also calculates the distribution of the delay on the different activities with the Stochastic Shapley rule.
#' @param precedence A matrix that indicates the order of precedence between activities. If the value \eqn{(i,j)} is equal to 1 then \eqn{i} precedes \eqn{j}.
#' @param distribution Type of distribution that each activities' initial duration has. It can be NORMAL, TRIANGLE, EXPONENTIAL, UNIFORM and EMPIRICAL.
#' @param values Matrix with the parameters corresponding to the distribution associated with the duration for each activity. Considering i as an activity we have the following cases. If the distribution is TRIANGLE, then (i, 1) it is the minimum value, (i, 2) the maximum value and (i, 3) the mode. If the distribution is NORMAL, (i, 1) is the mean and (i, 2) the variance. If the distribution is EXPONENTIAL, then (i, 1) is the \eqn{\lambda} parameter. If the distribution is UNIFORM, (i, 1) it is the minimum value and (i, 2) the maximum value. Finally, if the distribution is EMPIRICAL, then (i,j), for all \eqn{j\in \{1,...,M\}} such that \eqn{M>0}, is the sample.
#' @param observed.duration  Vector with the observed duration for each activity.
#' @param percentile Percentile used to calculate the maximum time allowed for the duration of the project (Default=NULL). Only percentile or delta is necessary.
#' @param delta   Maximum time allowed for the duration of the project (Default=NULL). Only delta or pencetile is necessary.
#' @param compilations Number of compilations that the function will use for average calculations (Default=1000).
#' @details Given a problem of sharing delays in a stochastic project \eqn{(N,\prec,\{X_i\}_{i\in N},\{x_i\}_{i\in N})},  such that \eqn{\{X_i\}_{i\in N}} is the random variable of activities' duration and \eqn{\{x_i\}_{i\in N}} the observed value. It is defined as \eqn{E(D(N,\prec,\{X_i\}_{i\in N}))} the expected project time, where \eqn{E} is the mathematical expectation, and \eqn{D(N,\prec,\{x_i\}_{i\in N})} the observed project time, then \eqn{d=D(N,\prec,\{X_i\}_{i\in N})-\delta>0}, with \eqn{\delta>0}, is the delay. The Stochastic Shapley rule is based on the Shapley value for the TU game \eqn{(N,v)} where \eqn{v(S)=E(\max\{D(N,\prec,(\{X_i\}_{i\in N\backslash S},\{x_i\}_{i\in S}))-\delta,0\})}, for all \eqn{S\subseteq N}.  If the number of activities is greater than nine, the Shapley value, of the game \eqn{(N,v)}, is estimated using a unique sampling process for all players, see \cite{Castro et al. (2009)}.
#'
#' @export
#' @references
#' \describe{
#'   \item{}{Castro, J., Gómez, D., & Tejada, J. (2009). Polynomial calculation of the Shapley value based on sampling. Computers & Operations Research, 36(5), 1726-1730.}
#'   \item{}{Gonçalves-Dosantos, J.C., García-Jurado, I., Costa, J. (2018) Sharing delay costs in Stochastic projects.}
#' }
#' @return  A delay value and solution vector.
#' @examples
#'
#' precedence<-matrix(c(0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#' distribution<-c("TRIANGLE","TRIANGLE","TRIANGLE","TRIANGLE","TRIANGLE")
#' values<-matrix(c(1,3,2,1/2,3/2,1,1/4,9/4,1/2,3,5,4,0,4,2),nrow=5,byrow=TRUE)
#' observed.duration<-c(2.5,1.25,2,4.5,3)
#' percentile<-NULL
#' delta<-6.5
#' delay.stochastic.pert(precedence,distribution,values,observed.duration,percentile,delta)
#'

delay.stochastic.pert<-function(precedence,distribution,values,observed.duration,percentile=NULL,delta=NULL,compilations=1000){

  or1<-order(organize(precedence)$Order[,2])
  or2<-organize(precedence)$Order[,2]
  precedence<-organize(precedence)$Precedence
  distribution<-distribution[or2]
  values<-values[or2,]
  observed.duration<-observed.duration[or2]
  activities<-1:length(distribution)
  tiempo.computacion<-numeric(compilations)
  n<-length(activities)
  tiempo.early<-rep(0,n)
  duration<-matrix(0,nrow=compilations,ncol=length(activities))
  v<-numeric(2^n-1)

  ii<-as.logical(colSums(precedence))
  iii<-activities[ii]
  nn<-length(iii)
  if(nn>0){
  prec<-matrix(0,nrow=nn,ncol=n-1)
  for(j in 1:nn){
    prec[j,1:length(which(precedence[,iii[j]]==1))]<-which(precedence[,iii[j]]==1)
  }
  prec<-prec[,as.logical(colSums(prec)),drop=FALSE]


  for(i in 1:nn) {
    tiempo.early[iii[i]]=max(tiempo.early[prec[i,]]+observed.duration[prec[i,]]);
  }
  }
  for(j in 1:n){
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
      duration[,j]<-  rnorm(compilations,sample(values[,j],size=compilations,replace=TRUE),density(values[,j])$bw)
    }
    }
  }

  duracion.total.T<-max(tiempo.early+observed.duration)

  if(is.null(percentile)==TRUE && is.null(delta)==TRUE){
    cat("necessary delta or percentile to continue", "\n")
    cat("Continue with percentile 0.95? (Y = Yes, N = No) \n")
    continue <- scan(what = "character", n = 1)
    {
      if(continue=="N"){
        stop("necessary delta or percentile")
      }
      else{
        percentile<-0.95
      }
    }

  }



  if(is.null(percentile)==FALSE && is.null(delta)==FALSE){
    cat("only one is necessary to continue", "\n")
    cat("Continue with percentile or delta? (P = PERCENTILE, D = DELTA) \n")
    continue <- scan(what = "character", n = 1)

    if(continue=="P"){
      delta<-NULL
    }
    else{
      percentile<-NULL
    }
  }

  if(is.null(percentile)==FALSE){

    for(i in 1:compilations){

      if(nn>0){
      for(j in 1:nn) {
        tiempo.early[iii[j]]=max(tiempo.early[prec[j,]]+duration[i,prec[j,]]);
      }
      }
      tiempo.computacion[i]<-max(tiempo.early+duration[i,])
    }
    delta<-quantile(ecdf(tiempo.computacion),percentile,names=FALSE)
  }


  tiempo.computacion<-numeric(compilations)
  PX.3<-numeric(n)

  for(z in 1:n){
    for(i in 1:compilations){
      duration1<-duration[i,]
      duration1[z]<-observed.duration[z]
      if(nn>0){
      for(j in 1:nn) {
        tiempo.early[iii[j]]=max(tiempo.early[prec[j,]]+duration1[prec[j,]])
      }
      }
      tiempo.computacion[i]<-max(tiempo.early+duration1)
    }
    v[z]<-mean(pmax(tiempo.computacion-delta,0))
  }


  {
    if(length(activities)>9){
      cat("shapley need some time to compute a", n, "player game \n")
      cat("Continue calculation of the Shapley value based on sampling? (Y = Yes, N = No) \n")
      continue <- scan(what = "character", n = 1)

      if(continue=="Y"){
        cont<-1
        while(cont<=1000){
        x<-sample(n)
        for(i in 1:(n-1)){
          for(j in 1:1000){
            duracion1<-c(0,duration[j,])
            duracion1[x[1:i]+1]<-observed.duration[x[1:i]]
            if(nn>0){
            for(t in 1:nn) {
              tiempo.early[iii[t]]=max(tiempo.early[prec[t,]]+duration1[prec[t,]])
            }
            }
            tiempo.computacion[j]<-max(tiempo.early+duracion1)
          }
          tiempo2[i]<-mean(pmax(tiempo.computacion-delta,0))
        }

        if(nn>0){
          for(t in 1:nn){
            tiempo.early[iii[t]]=max(tiempo.early[prec[t,]]+observed.duration[prec[t,]])
          }
        }
        tiempo2[n]<-mean(max(max(tiempo.early+duracion1)-delta,0))
        sh[x[1]]<-tiempo2[1]
        sh[x[2:n]]<-tiempo2[2:n]-tiempo2[1:(n-1)]
        cont<-cont+1
        }
      }
    }

    else{
    PX.3[1:n]<-v[1:n]
    continue<-"Y"
  tiempo.computacion<-numeric(compilations)
  p<-n+1

  for(j in 2:n){
    con<-combn(c(1:n),j)

    for(z in 1:dim(con)[2]){

      for(i in 1:compilations){
        duracion1<-duration[i,]
        duracion1[con[,z]]<-observed.duration[con[,z]]
        if(nn>0){
        for(t in 1:nn) {
          tiempo.early[iii[t]]=max(tiempo.early[prec[t,]]+duracion1[prec[t,]]);
        }
        }
        tiempo.computacion[i]<-max(tiempo.early+duracion1)
      }
      v[p]<-mean(pmax(tiempo.computacion-delta,0))
      p<-p+1
    }
  }

  sh<-c(shapleyValue(n,v=v)$value)
}


  cat("Total delay of the stochastic project = ",max(duracion.total.T-delta,0), "\n")
  cat(" ","\n")

  {
    if(continue=="Y"){
    A<-matrix(c(sh[or1]),ncol=n,byrow=TRUE)
    colnames(A)=c(activities)
    rownames(A)=c("Shapley Value :  ")
    return(round(A,5))
    }
    else{

    }
  }
}
}
