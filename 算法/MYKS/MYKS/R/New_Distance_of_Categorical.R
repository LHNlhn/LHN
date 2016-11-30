New_Distance_of_Categorical<-function(Data1,Data2)
{
{
  n<-nrow(Data1)
  d<-ncol(Data1)
}
dataset<-vector()
Distance<-vector()
      Distance=0
        for(j in 1:d)
          {
          if (Data1[1,j]!=Data2[1,j])
          Distance<-Distance+1
          }
            dataset<-Distance
      dist<-matrix(c(dataset),ncol=1)
}

