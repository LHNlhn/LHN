Fuzzy_K_Mode<-function(data,K,InitialCenters)
{
  {
    n<-nrow(data)
    d<-ncol(data)
  }
  cid<-vector(length=n)
  oldcia<-matrix(1,nrow=n,ncol=K)
  nr<-matrix(0,nrow=n,ncol=K)
  if(is.null(InitialCenters)){
    DDist<-k_initial_center(data,K)
      centers<-data[DDist,]
  }else{
    centers<-data[InitialCenters,]
  }
  row.names(centers)<-c(1:K)
  ni<-centers
  nid<-nrow(ni)
  if(nid!=K)
    print(error)
  #Set up maximum number of iterations
  iter<-1
  iiter<-100
  while(iter<=iiter)
  {
    {
      ptm<-proc.time()
      FW_dist<-vector()
    Dist<-vector()
    for(i in 1:n)
    {
      q<-matrix(c(rep(data[i,],K)),nrow=K,ncol=d,byrow=T)
      dist<-Distance_of_Categorical(q,ni)
      N<-nrow(dist)
      for(j in 1:N)
      {
        Ds<-dist[j,]
        if(Ds==0){
          Dist[j]<-1
        }else{
          W<-matrix(c(Ds),nrow=K,ncol=1)
          Dist[j]<-1/(sum((W/dist)^10))
        }
      }
      Fw_dist<-as.matrix(Dist)
      ind<-which(Fw_dist==max(Fw_dist[,1]),arr.ind = T)[1]
      cid[i]<-ind
    }
    nr<-table(cid)
  nj<-ni
  data1<-vector()
  for(i in 1:K)
  {
    data1<-data[(cid==i),]
    if(length(data1)!=0)
    {
      nj[i,]<-Find_Mode(data1)
    }
  }
  if(all(nj==ni))  break
  }
  ni<-nj
  iter<-iter+1
  }
  cat("iter=",iter,"\n")
  cat("clustering result cid=","\n",cid,"\n")
  cat("time","\n")
  print(proc.time()-ptm)
}


