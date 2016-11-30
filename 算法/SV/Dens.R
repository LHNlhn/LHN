Dens<-function(Data)
{
  m<-ncol(Data)
  udata<-as.matrix(unique(Data[,1]))
  n<-nrow(udata)
  Dens<-vector()
  for(i in 1:n)
  {
    dens<-0
    for(j in 2:m)
    {
      for(p in 1:n)
      {
        DData1<-Data[Data[,1]==udata[i],j]
        DData2<-Data[Data[,1]==udata[p],j]
        ss<-ratio_distance_between_objects(DData1,DData2)
        dens<-dens+(1/n)%*%ss
      }
    }
        Dens[i]<-dens
      
    }
DDens<-as.matrix(Dens)
}

