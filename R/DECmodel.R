DECmodel<-function(geogfn,trfn,j=F,adjacency=NULL,stratified=NULL,cores=4){
  tipranges = getranges_from_LagrangePHYLIP(lgdata_fn=geogfn)
  require(ape)
  require(ape)
  require(optimx)   
  require(GenSA)
  require(rexpokit)
  require(cladoRcpp)
  require(snow) 
  require(parallel)
  require(BioGeoBEARS)
  tr <- read.tree(trfn)
  max_range_size<-max(rowSums(dfnums_to_numeric(tipranges@df)))
  BioGeoBEARS_run_object = define_BioGeoBEARS_run()
  BioGeoBEARS_run_object$trfn = trfn
  BioGeoBEARS_run_object$geogfn = geogfn
  BioGeoBEARS_run_object$max_range_size = max_range_size
  BioGeoBEARS_run_object$min_branchlength = 0.000001
  BioGeoBEARS_run_object$include_null_range = F
  BioGeoBEARS_run_object$on_NaN_error = -1e50 
  BioGeoBEARS_run_object$speedup = TRUE 
  BioGeoBEARS_run_object$use_optimx = TRUE 
  BioGeoBEARS_run_object$num_cores_to_use = cores
  BioGeoBEARS_run_object$force_sparse = FALSE 
  BioGeoBEARS_run_object = readfiles_BioGeoBEARS_run(BioGeoBEARS_run_object)
  #
  if(!is.null(adjacency)){BioGeoBEARS_run_object$areas_adjacency_fn = adjacency}
  if(!is.null(stratified)){BioGeoBEARS_run_object$timesfn = stratified}
  BioGeoBEARS_run_object$return_condlikes_table = TRUE
  BioGeoBEARS_run_object$calc_TTL_loglike_from_condlikes_table = TRUE
  BioGeoBEARS_run_object$calc_ancprobs = TRUE
  BioGeoBEARS_run_object = fix_BioGeoBEARS_params_minmax(BioGeoBEARS_run_object=BioGeoBEARS_run_object)
  res = bears_optim_run(BioGeoBEARS_run_object)
  if(j==F){return(res)}else{
  BioGeoBEARS_run_object$return_condlikes_table = TRUE
  BioGeoBEARS_run_object$calc_TTL_loglike_from_condlikes_table = TRUE
  BioGeoBEARS_run_object$calc_ancprobs = TRUE
  dstart = res$outputs@params_table["d","est"]
  estart = res$outputs@params_table["e","est"]
  jstart = 0.0001
BioGeoBEARS_run_object$BioGeoBEARS_model_object@params_table["d","init"] = dstart
  BioGeoBEARS_run_object$BioGeoBEARS_model_object@params_table["d","est"] = dstart
  BioGeoBEARS_run_object$BioGeoBEARS_model_object@params_table["e","init"] = estart
  BioGeoBEARS_run_object$BioGeoBEARS_model_object@params_table["e","est"] = estart
  BioGeoBEARS_run_object$BioGeoBEARS_model_object@params_table["j","type"] = "free"
  BioGeoBEARS_run_object$BioGeoBEARS_model_object@params_table["j","init"] = jstart
  BioGeoBEARS_run_object$BioGeoBEARS_model_object@params_table["j","est"] = jstart
  BioGeoBEARS_run_object = fix_BioGeoBEARS_params_minmax(BioGeoBEARS_run_object=BioGeoBEARS_run_object)
  res = bears_optim_run(BioGeoBEARS_run_object)
  return(res)}
}