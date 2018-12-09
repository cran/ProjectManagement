#' @title Organize project activities
#' @description This function organizes the activities of a project, in such a way that if i precedes j then i is less strict than j.
#' @param precedence A matrix that indicates the order of precedence between activities.  If the value \eqn{(i,j)} is equal to 1 then \eqn{i} precedes \eqn{j}.
#' @export
#' @return A list containing:
#' \itemize{
#' \item{Precedence: }{ ordered precedence matrix.}
#' \item{Order: }{ new activities values.}
#' }
#' @examples
#' precedence<-matrix(c(0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#' organize(precedence)

organize<-function(precedence){
  P1<-precedence
  colnames(P1)<-1:nrow(P1)
  rownames(P1)<-1:nrow(P1)
  indices<-c()
    while(ncol(P1)>0){
     ii<-as.logical(colSums(P1))
     indices<-c(indices,colnames(P1)[!ii])
     P1<-P1[ii,ii,drop=FALSE]
   }
    indices<-as.numeric(indices)
    precedence<-precedence[indices,indices]
    N<-matrix(0,nrow=dim(precedence)[1],ncol=2)
    N[,1]<-1:dim(precedence)[1]
    N[,2]<-indices
    lista<-list(precedence,N)
    names(lista)<-c("Precedence","Order")
  return(lista)
}
