% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_organize.R
\name{organize}
\alias{organize}
\title{Organize project activities}
\usage{
organize(prec1and2 = matrix(0), prec3and4 = matrix(0))
}
\arguments{
\item{prec1and2}{A matrix indicating the order of precedence type 1 and 2 between the activities (Default=matrix(0)). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.}

\item{prec3and4}{A matrix indicating the order of precedence type 3 and 4 between the activities (Default=matrix(0)). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.}
}
\value{
A list containing:
\itemize{
\item{Precedence: }{ ordered precedence matrix.}
\item{Order: }{ new activities values.}
}
}
\description{
This function organizes the activities of a project, in such a way that if i precedes j then i is less strict than j.
}
\examples{
prec1and2<-matrix(c(0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
organize(prec1and2)
}
