#' @title Schedule for deterministic projects
#' @description This function calculates the duration of the project, the slacks for each activity, as well as the schedule of each activity.
#' @param duration Vector with the duración for each activity.
#' @param precedence A matrix that indicates the order of precedence between activities.  If the value \eqn{(i,j)} is equal to 1 then \eqn{i} precedes \eqn{j}.
#' @param PRINT  Logical indicator to show the schedule represented in a graph (Default=TRUE)
#' @export
#' @references
#' \describe{
#'   \item{}{Burke, R. (2013). Project management: planning and control techniques. New Jersey, USA.}
#' }
#' @return A list of a project schedule and if PRINT=TRUE a plot of schedule.
#' @examples
#' precedence<-matrix(c(0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#' duration<-c(3,2,1,1.5,4.2)
#' schedule.pert(duration,precedence)

schedule.pert<-function(duration,precedence,PRINT=TRUE){
  activities<-1:length(duration)
  n<-length(activities)
  tiempo.early<-numeric(n)
  tiempo.last<-numeric(n)


    tiempo.early<-early.time(precedence,duration)

    tiempo.last<-last.time(precedence,duration,tiempo.early)



  if(PRINT==TRUE){

    duracion.total<-max(tiempo.early+duration)
    holguras.actividades<-tiempo.last-tiempo.early-duration
    comienzo.temprano<-tiempo.early
    comienzo.tardio<-tiempo.early+holguras.actividades
    finalizacion.temprano<-tiempo.last-holguras.actividades
    finalizacion.tardia<-tiempo.last
    data <- data.frame(activities,duration,comienzo.temprano,comienzo.tardio,finalizacion.temprano,finalizacion.tardia,holguras.actividades)
    print<-plot_ly(data) %>%
      add_segments(x = ~comienzo.temprano, xend = ~comienzo.tardio, y = ~activities, yend = ~activities,hoverinfo = "text", color = I("black"),showlegend = FALSE) %>%
      add_segments(x = ~comienzo.tardio, xend = ~finalizacion.temprano, y = ~activities, yend = ~activities, hoverinfo = "text", color = I("black"),showlegend = FALSE) %>%
      add_segments(x = ~finalizacion.temprano, xend = ~finalizacion.tardia, y = ~activities,hoverinfo = "text", yend = ~activities, color = I("black"), showlegend = FALSE) %>%
      add_markers(x = ~comienzo.temprano, y = ~activities,marker = list(size = 10),hoverinfo = "text", name = 'Earliest start date',text = ~paste("Activitye: ", activities,"Early start date: ",comienzo.temprano), mode = 'markers',type = 'scatter') %>%
      add_markers(x = ~comienzo.tardio, y = ~activities, color = I("blue"),hoverinfo = "text",marker = list(size = 10),name = 'Later start date',text = ~paste("Activity: ", activities,"Later start date: ",comienzo.tardio), mode = 'markers',type = 'scatter') %>%
      add_markers(x = ~finalizacion.temprano, y = ~activities,color = I("green"),hoverinfo = "text",marker = list(size = 10), name = 'Earliest completion date',text = ~paste("Activity: ", activities,"Earliest completion date: ",finalizacion.temprano), mode = 'markers',type = 'scatter') %>%
      add_markers(x = ~finalizacion.tardia, y = ~activities,marker = list(size = 10), hoverinfo = "text",name = 'Later completion date',text = ~paste("Activity: ", activities,"Later completion date: ",finalizacion.tardia), mode = 'markers',type = 'scatter') %>%
      layout(title = "Schedule",yaxis=list(title = "Activities"),xaxis = list(title = "Times",zeroline=FALSE))
    colnames(data)=c("Activities"," Duration"," Earliest start date"," Later start date"," Earliest completion date"," Later completion date "," Slacks for each activity")
    lista<-list('Total duration of the project'=duracion.total,data)
    print(print)
    return(lista)
  }
  else{
    duracion.total<-max(tiempo.early+duration)
    holguras.actividades<-tiempo.last-tiempo.early-duration
    comienzo.temprano<-tiempo.early
    comienzo.tardio<-tiempo.early+holguras.actividades
    finalizacion.temprano<-tiempo.last-holguras.actividades
    finalizacion.tardia<-tiempo.last
    data <- data.frame(activities,duration,comienzo.temprano,comienzo.tardio,finalizacion.temprano,finalizacion.tardia,holguras.actividades)
    colnames(data)=c("Activities"," Duration"," Earliest start date"," Later start date"," Earliest completion date"," Later completion date "," Slacks for each activity")
    lista<-list('Total duration of the project'=duracion.total,data)
    return(lista)
  }

}

