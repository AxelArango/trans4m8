#'BioGeoBEARS result to data.frame
#'This function helps extracting the state with maximum likelihood probability for each branch at the node (i.e., the most probable ancestral area for each node, and tip). This is useful for plotting in ggtree or better visualizing the ancestral states of the node.
#'@param res res object (calc_loglig_sp_results) from BioGeoBEARS
#'@param phylo phylogenetic tree used to calculate res object
#'@param areas number of unique areas used to calculate res object
#'@returns a data.frame with the tip label and node number in the phylogey and the most probable ancestral area fro such label.
#'@export
res2df<-function(res,phylo,areas=NULL){

  nodedtree<-makeNodeLabel(phylo)
  taxlab<-c(nodedtree$tip.label,nodedtree$node.label)
  relativeprobs<-res$ML_marginal_prob_each_state_at_branch_top_AT_node
  relativeprobsbin<-relativeprobs
  for(i in 1:length(relativeprobs[,1])){
    maxval<-max(relativeprobs[i,])
    colx<-which(relativeprobs[i,]==maxval)
    relativeprobsbin[i,]<-0
    relativeprobsbin[i,colx]<-1
  }
  stateslist<-data.frame()
  for(a in 1:length(relativeprobs[1,])){
    slep<-states[[a]]
    slepx<-paste(slep,collapse = "")
    slx<-data.frame(a,slepx)
    stateslist<-rbind(stateslist,slx)
  }

  areax<-LETTERS[1:areas]
  xlox<-stateslist$slepx
  lx<-0
  for (b in 1:length(areax)){
    xlox<-gsub(lx,areax[b],xlox)
    lx<-lx+1}
  xlox
  stateslistx<-stateslist
  stateslistx[,2]<-xlox
  dfx<-data.frame()
  for(x in 1:dim(relativeprobsbin)[1]){
    dfx1<-data.frame(label=taxlab[x],area=stateslistx$slepx[which(relativeprobsbin[x,]==1)])
    dfx<-rbind(dfx,dfx1)}

    return(dfx)
}
