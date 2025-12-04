#'Clusters to dichotomic regions
#'
#'Transforms a data.frame that contains species and their clusters into data with binary characters (A and B) and a widespread state (AB) for analyses in binary character speciation and extinction models (Particularly GeoHisse).
#'@param data A data.frame with species names and clusters columns in that order.
#'@param reg Combination of the clusters that conform one of the two regions (e.g., "ADG").
#'@param num This parameter indicates if the result data.frame will contain the ranges as numbers (required for SSE analyses), Where state 0 is range A (or reg), state 1 is range B (or clusters-reg), and state 2 is AB or species that live in both ranges.
#'@returns A data.frame with species belonging to Range A, Range B or widespread (Range AB).
#'@export
clus2bi<-function(data,reg=NULL,num=T){
  names(data)<-c("species","cluster")
  databl<-table(data)
  sps<-as.character(unique(data.frame(databl)$species))
  reg1<-unlist(strsplit(reg,""))
  id<-paste(reg1,collapse = "|")
  nclusreg<-length(unlist(strsplit(reg,"")))
  spreg<-data.frame()
  for(i in 1:length(databl[,1])){
  clusts<-as.numeric(databl[i,])
  for(j in 1:length(databl[i,])){
   if(clusts[j]==0){
     clusts[j]<-NA}
    else{
      clusts[j]<-LETTERS[j]
    }
  }
  clusts2<-as.character(na.omit(clusts))
  clusts2<-paste(clusts2,collapse = "")
  spclusts<-data.frame(taxon=sps[i],ranges=clusts2)
  spreg<-rbind(spreg,spclusts)
  }
  if(num==T){
    regb<-spreg %>%
      filter(!grepl(id,ranges)) %>%
      mutate(ranges=2)
    rega1<-spreg %>%
      filter(grepl(id,ranges))
    for(h in 1:length(rega1$taxon)){
      reg2<-unlist(strsplit(rega1$ranges[h],""))
      if(length(reg2)>length(reg1)){rega1$ranges[h]<-0}else{
        rega1$ranges[h]<-1
      }
    }
  }
    else{
  regb<-spreg %>%
    filter(!grepl(id,ranges)) %>%
    mutate(ranges="B")
  rega1<-spreg %>%
    filter(grepl(id,ranges))
  for(h in 1:length(rega1$taxon)){
    reg2<-unlist(strsplit(rega1$ranges[h],""))
    if(length(reg2)>length(reg1)){rega1$ranges[h]<-"AB"}else{
      rega1$ranges[h]<-"A"
    }
  }
  }
  regf<-rbind(regb,rega1)
  return(regf)
  }
