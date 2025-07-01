#'Cladogenetic Event table to summary data frame
#'
#'Transforms the standard anagenetic event tables from BioGeoBEARS SBMs into a data.frame that includes the anagenetic event of interest across the total number of calculated maps.
#'@param event_tables A list with anagenetic tables acquired through the stochastic mapping from BioGeoBEARS.
#'@param type Type of anagenetic event, NULL by default. This can be subset dispersal "d" and extinction "e".
#'@returns A data.frame with the type of event, the time at which it happened in MYA, at which node it happen and the area in which it happened.
#'@export
aevent2df<-function(event_tables,type=NULL){
  eventx<-data.frame()
    for(i in 1:length(event_tables)){
      eventn<-event_tables[[i]]
      if(type=="d"){
        eventm<-eventn%>%
        filter(event_type==type) %>%
        select(event_type,time_bp,node,dispersal_to) %>%
        mutate(map=i)}else{
          eventm<-eventn%>%
            filter(event_type==type) %>%
            select(event_type,time_bp,node,extirpation_from) %>%
            mutate(map=i)
        }
      names(eventm)<-c("type","MYA","node","area")
      eventx<-rbind(eventx,eventm)
    }


  return(eventx)
}
