New_radio_k_multi_modes_M<-function(Data,K,InitialCenters)
{
  library(parallel)
  cl.cores<-detectCores()
  cl<-makeCluster(cl.cores)
  res<-parLapply(cl=cl,New_ratio_k_multi_modes(Data,K,InitialCenters))
  stopCluster(cl)
}
