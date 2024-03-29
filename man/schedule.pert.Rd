% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_pert_calendario.R
\name{schedule.pert}
\alias{schedule.pert}
\title{Schedule for deterministic projects}
\usage{
schedule.pert(
  duration,
  prec1and2 = matrix(0),
  prec3and4 = matrix(0),
  PRINT = TRUE
)
}
\arguments{
\item{duration}{Vector with the duration for each activity.}

\item{prec1and2}{A matrix indicating the order of precedence type 1 and 2 between the activities (Default=matrix(0)). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.}

\item{prec3and4}{A matrix indicating the order of precedence type 3 and 4 between the activities (Default=matrix(0)). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.}

\item{PRINT}{Logical indicator to show the schedule represented in a graph (Default=TRUE)}
}
\value{
A list of a project schedule and if PRINT=TRUE a plot of schedule.
}
\description{
This function calculates the duration of the project, the slacks for each activity, as well as the schedule of each activity.
}
\examples{
prec1and2<-matrix(c(0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
duration<-c(3,2,1,1.5,4.2)
schedule.pert(duration,prec1and2)
}
\references{
\describe{
  \item{burk}{Burke, R. (2013). Project management: planning and control techniques. New Jersey, USA.}
}
}
