#' @title Last time for a deterministic projects
#' @description This function calculates the last time for one project.
#' @param duration Vector with the duración for each activity.
#' @param precedence A matrix that indicates the order of precedence between activities.  If the value \eqn{(i,j)} is equal to 1 then \eqn{i} precedes \eqn{j}.
#' @param early.times Vector whit the early times for each activities.
#' @export
#' @references
#' \describe{
#'   \item{}{Burke, R. (2013). Project management: planning and control techniques. New Jersey, USA.}
#' }
#' @return Last time vector.
#' @examples
#' precedence<-matrix(c(0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#' duration<-c(3,2,1,1.5,4.2)
#' early.times<-c(0,0,3.5,2,0)
#' last.time(precedence,duration,early.times)
last.time<-function(precedence,duration,early.times){

  or1<-order(organize(precedence)$Order[,2])
  or2<-organize(precedence)$Order[,2]
  precedence<-organize(precedence)$Precedence
  activities<-c(1:dim(precedence)[1])
  n<-length(activities)
  duration<-duration[or2]
  early.times<-early.times[or2]

  tiempo.last<-rep(0,n)

  ii<-as.logical(rowSums(precedence))
  iii<-activities[ii]
  tiempo.last[activities[ii==FALSE]]<-max(early.times+duration)
  nn<-length(iii)
  if(nn>0){
  prec<-matrix(0,nrow=nn,ncol=n-1)
  for(j in 1:nn){
    prec[j,1:length(which(precedence[iii[j],]==1))]<-which(precedence[iii[j],]==1)
  }
  prec<-prec[,as.logical(colSums(prec)),drop=FALSE]


  for(i in length(iii):1) {
    tiempo.last[iii[i]]=min(tiempo.last[prec[i,]]-duration[prec[i,]]);
  }
  }
  return(tiempo.last[or1])
}
