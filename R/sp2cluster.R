#' Species to clusters
#'
#' This function assigns species to the clusters obtained with phyloregion. Gives which species live in which clusters.
#' @param comm_data A sparse community data from phyloregion.
#' @param cluster_data A phyloregion data.frame result.
#' @param letters This function allows the user to decide if the clusters will be given as numbers (phyloregion output) or as letters (suitable for BioGeoBEARS, Lemad or GeoHiSSE).
#' @export
sp2cluster<-function(comm_data,cluster_data,letters=T){
  longcomm<-sparse2long(comm_data)
  cluster_data<-data.frame(grids=cluster_data$grids,cluster=cluster_data$cluster)
  if(letters==T){
  for(i in 1:length(unique(cluster_data$cluster))){
    cluster_data$cluster[which(cluster_data$cluster==i)]<-LETTERS[i]
  }
    }else{cluster_data<-cluster_data}
  clustsp<-merge(cluster_data,longcomm,by="grids") %>%
    select(species,cluster) %>%
    unique()
  return(clustsp)
}






