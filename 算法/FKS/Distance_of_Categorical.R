Distance_of_Categorical<-function(Data1,Data2)
{
{
  n<-nrow(Data1)
  d<-ncol(Data1)
}
dataset<-vector()
Distance<-vector()
{
     for(i in 1:n){
      Distance=0

                for(j in 1:d){
                 if (Data1[i,j]!=Data2[i,j])
                     Distance<-Distance+1
                 }
            dataset[i]<-Distance
          }

  }
      dist<-matrix(c(dataset),ncol=1)
}

