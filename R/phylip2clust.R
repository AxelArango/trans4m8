#'Phylip to data.frame with cluster
#'This function turns a BioGeoBEARS Phylip table into data frame ready for use with LEMAD
#'@param data table object of a Phylip file without the initiation row (Number of species, Number of areas, Names of areas)
#'@export
phylip2clust<-function(data){
  names(data)<-c("sp","regions")
  for(i in 1:length(data$regions)){
    split<-strsplit(data$regions[i],"")
    split2<-split[[1]]
    for(j in 1:length(split2)){
      if(split2[j]==0){split2[j]<-NA}else{split2[j]<-LETTERS[j]}
    }
    split3<-na.omit(split2)
    comsplit<-paste(split3,collapse = "")
    data$regions[i]<-comsplit
  }
  row.names(data)<-NULL
  return(data)
}
