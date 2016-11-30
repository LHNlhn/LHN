frequency<-function(data)
{
  row<-nrow(data)
  column<-ncol(data)
  da<-vector()
  daa<-vector()
  DA<-vector()
  dist<-vector()
  for(i in 1:row)
  {
    sum=0
    for(j in 1:column)
    {
      ss<-length(which(data[,j]==data[i,j]))/row
      sum<-sum+ss
    dist<-sum
    }
    daa<-cbind(dist,i)
    DA<-rbind(DA,daa)
  }
  return(DA)
}
      


