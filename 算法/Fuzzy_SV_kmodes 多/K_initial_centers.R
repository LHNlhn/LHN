K_initial_centers<-function(Data,K)
{
  m<-ncol(Data)
  udata<-as.matrix(unique(Data[,1]))
  n<-nrow(udata)
  ddens<-Dens(Data)
  rr<-which(ddens==max(ddens))
  dist<-vector()
  dist<-rbind(dist,rr)
  xx<-0
  mmm<-vector()
  for(j in 1:n)
  {
    r<-0
    for(p in 2:m)
  {
    r<-r+New_ratio_distance_between_objects(Data[Data[,1]==j,p],Data[Data[,1]==dist[1],p])%*%ddens[j]
    }
    if(r>xx)
    {
      xx<-r
      m<-j
    }
  }
  dist<-rbind(dist,m)
  for(i in 3:K)
  {
    mindist<-vector()
    for(j in 1:n)
    {
      yy<-99999
      for(n in 1:length(dist))
       {
        t<-0
         for(p in 2:m)
          {
            t<-New_ratio_distance_between_objects(Data[Data[,1]==j,p],Data[Data[,1]==dist[n],p])%*%ddens[j]
          }
         if(t<yy)
            {
              yy<-t
            }
       }
      mindist<-rbind(mindist,yy)
    }
    x<-which(mindist==max(mindist))
    dist<-rbind(dist,x)
  }
  dist<-as.matrix(dist)
}


