#'PAM to points
#'
#'Transforms a Presence Absence Matrix object from letsR into geographic data points for the species.
#'@param PAM a Presence Absence Matrix object from letsR.
#'@param phylo_names option to turn the names of species into names that can match phylogenetic data.
#'@param gridid option to return the id of the grid, useful for validate number of assamblages.
#'@export
pam2points<-function(PAM,phylo_names=F,gridid=F){
  coords<-as.data.frame(PAM$Presence_and_Absence_Matrix[,1:2])
  PAM1<-PAM$Presence_and_Absence_Matrix[,-c(1,2)]
  points<-data.frame()
  for(i in 1:length(coords$`Longitude(x)`)){
    pointa<-data.frame(Presence=PAM1[i,]) %>%
      filter(Presence==1)
    if(gridid==T){points2<-data.frame(x=coords[i,1],y=coords[i,2],sp=row.names(pointa),id=i)}else{
      points2<-data.frame(lon=coords[i,1],lat=coords[i,2],sp=row.names(pointa))}
    points<-rbind(points,points2)
    cat("\rFinished", i, "of", length(coords$`Longitude(x)`))
  }
  if(phylo_names==T){points$sp<-gsub(" ","_",points$sp)}else{points<-points}
  return(points)
}
