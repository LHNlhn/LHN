k_initial_center<-function(data,K)
{
  row<-nrow(data)
  column<-ncol(data)
  da<-frequency(data)
  rr<-which(da==max(da[,1]))
  dist<-vector()
  dist<-rbind(dist,rr)
  xx<-0
  mmm<-vector()
  for(j in 1:row)
  {
    r<-New_Distance_of_Categorical(data[j,],data[dist[1],])%*%da[j,1]
      if(r>xx)
      {
        xx<-r
        m<-j
      }
  }
  dist<-rbind(dist,m)
  dist<-as.matrix(dist)
  for(i in 3:K)
  {
    mindist<-vector()
      for(j in 1:row)
      {
         yy<-99999
         for(n in 1:length(dist))
         {
           t<-New_Distance_of_Categorical(data[j,],data[dist[n],])%*%da[j,1]
           if(t<yy)
           {
             yy<-t
           }
         }
         mindist<-rbind(mindist,yy)
      }
    x<-which(mindist==max(mindist))
    dist<-as.matrix(rbind(dist,x))
  }
  DDist<-as.matrix(dist)
}
    

