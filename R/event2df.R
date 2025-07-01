#'Cladogenetic Event table to summary data frame
#'
#'Transforms the standard cladogenetic event tables from BioGeoBEARS SBMs into a dataframe that includes the cladogenetic event of interest across the total number of calculated maps.
#'@param event_tables A list with cladogenetic tables acquired through the stochastic mapping from BioGeoBEARS.
#'@param type Type of cladogenetic event, NULL by default. This can be subset cladogenesis "subset (s)", vicariance "vicariance (v)", in situ speciation "sympatry (y)", and founder events "founder (j)".
#'@returns A data.frame with the type of event, the time at which it happened in MYA, at which node it happen and the area in which it happened.
#'@export
event2df<-function(event_tables,type=NULL){
  eventx<-data.frame()
  if(type!="founder (j)"){
  for(i in 1:length(event_tables)){
    eventn<-event_tables[[i]]
    eventm<-eventn%>%
      filter(clado_event_type==type) %>%
      select(clado_event_type,time_bp,node,clado_event_txt) %>%
      mutate(map=i)
    area<-data.frame(area=eventm$clado_event_txt) %>%
      as.tibble ()%>%
      separate(area,c("area",NA),"-")
    eventm2<-data.frame(eventm[,-4],area=area$area)
    names(eventm2)<-c("type","MYA","node","map_n","area")
    eventsm3<-data.frame()
    for(j in 1:length(eventm2$type)){
      ars<-data.frame(areas=strsplit2(eventm2$area[j],""))
      evs<-data.frame(eventm2[j,-5],areas=ars)
      eventsm3<-rbind(eventsm3,evs)
    }
    eventx<-rbind(eventx,eventsm3)

  }
  }else{for(i in 1:length(event_tables)){
    eventn<-event_tables[[i]]
    eventm<-eventn%>%
      filter(clado_event_type==type) %>%
      mutate(map=i) %>%
      select(clado_event_type,time_bp,node,map,clado_dispersal_to)

    #area<-data.frame(area=eventm$clado_event_txt) %>%
      #as.tibble ()%>%
      #separate(area,c("area",NA),"-")
    #eventm2<-data.frame(eventm[,-4],area=area$area)
    names(eventm)<-c("type","MYA","node","map_n","area")
    #eventsm3<-data.frame()
    #for(j in 1:length(eventm2$type)){
      #ars<-data.frame(areas=strsplit2(eventm2$area[j],""))
      #evs<-data.frame(eventm2[j,-5],areas=ars)
      #eventsm3<-rbind(eventsm3,evs)
    }
    eventx<-rbind(eventx,eventm)

  }

  return(eventx)
}

