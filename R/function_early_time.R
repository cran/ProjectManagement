#' @title Early time for a deterministic projects
#' @description This function calculates the early time for one project.
#' @param duration vector with the duración for each activities.
#' @param precedence a matrix that indicates the order of precedence between activities.  If the value \eqn{(i,j)} is equal to 1 then \eqn{i} precedes \eqn{j}.
#' @export
#' @references
#' \describe{
#'   \item{}{Burke, R. (2013). Project management: planning and control techniques. New Jersey, USA.}
#' }
#' @return Early time vector.
#' @examples
#' precedence<-matrix(c(0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#' duration<-c(3,2,1,1.5,4.2)
#' early.time(precedence,duration)

early.time<-function(precedence,duration){
  or1<-order(organize(precedence)$Order[,2])
  or2<-organize(precedence)$Order[,2]
  precedence<-organize(precedence)$Precedence
  activities<-c(1:dim(precedence)[1])
  n<-length(activities)
  duration<-duration[or2]
  tiempo.early<-rep(0,n)

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
    tiempo.early[iii[i]]=max(tiempo.early[prec[i,]]+duration[prec[i,]]);
  }
  }

  return(tiempo.early[or1])
}


