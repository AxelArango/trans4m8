#'Clusters to distance matrix
#'This function calculates the geodesic distance between clusters using a phyloregion object
#'@param phyloreg Phyloregion object, preferably with the optimal K
#'@param BioGeoBEARS If true, it will set up the matrix ready to be exported for BioGeoBears (i.e., distances of 0 are turned into 1, to avoid calculation problems as suggested by Matzke. 2014)
#'@returns A square distance matrix of the geodesic distance between phyloregions, measured in Km.
#'@export
cluster2dist<-function(phyloreg,BioGeoBEARS=T){
  clusters<-LETTERS[1:length(phyloreg$pol$cluster)]
  sfregs<-sf::st_as_sf(phyloreg$pol)
  geodist<-st_distance(sfregs)/1000
  geomat<-data.frame(matrix(ncol = length(clusters), nrow = length(clusters)))
  names(geomat)<-clusters
  for(i in 1:length(clusters)){
    coldist<-data.frame(as.numeric(geodist[,i]))
    if(BioGeoBEARS==T){
      coldist[which(coldist[,1]==0),]<-as.numeric(1)
      geomat[,i]<-coldist
    }else{
    geomat[,i]<-coldist}
  }
  return(geomat)
}


