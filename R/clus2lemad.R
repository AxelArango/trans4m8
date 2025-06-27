#'Cluster to Lemad
#'This function transforms a data frame of species with their ranges into a working table ready to use for Lemad analysis. By combining all the individual ranges and sorting the species in the same order as they are in the phylogeny.
#'@param data A data.frame with species and a unique range (e.g., "A", "B"), in that order.
#'@param tree A phylogenetic tree of the species from data.
#'@returns A data frame of species and their combined ranges in the same order as the phylogeny.
#'@export
clus2lemad<-function(data,tree){
  names(data)<-c("species","cluster")
  databl<-table(data)
  sps<-as.character(unique(data.frame(databl)$species))
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
    spclusts<-data.frame(sp=sps[i],ranges=clusts2)
    spreg<-rbind(spreg,spclusts)
  }
  spreg <- spreg[order(match(spreg$sp,tree$tip.label)),]
  return(spreg)
}
