#'Dataframe to Phylip
#'Transform a data frame with species and areas into a table almost ready for BioGeoBEARS. save as: write.table(phylip,"phylip.txt",row.names=F,col.names=F,quote=F). It will still require the Phylip inititation (Number of species, Number of areas, names of the areas).
#'@param data a data.frame with a column species that contains the species to use in the biogeographic analysis, genus separated from species with "_"; and a column with the biogeographic area in which the species resides.
#'@return a table ready to be used for Biogeobears
#'@export
data2phylip<-function(data){
  dumtable<-table(data$species,data$cluster)
  dummydf<-data.frame(matrix(ncol = 2,nrow=nrow(dumtable)))
  dummydf[,1]<-row.names(dumtable)
  for (i in 1:length(dummydf[,1])){
    dumpas<-paste(dumtable[i,1:ncol(dumtable)],collapse = "")
    dummydf[i,2]<-dumpas
  }
  matriz<-as.matrix(dummydf)
  as.table(matriz,sep="\t",row.names=F,colnames=F)
  }
