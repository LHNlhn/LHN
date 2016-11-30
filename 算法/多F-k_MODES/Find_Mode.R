Find_Mode<-function(Data)
{
  row<-nrow(Data)
column<-ncol(Data)
CColumnValue<-vector()
New_Mode<-vector()
dataset1<-vector()
NNew_Mode<-vector()
for(i in 1:column)
  {
     CColumnValue<-matrix(c(unique(Data[,i])),ncol=1)
           for(j in 1:length( CColumnValue))
       {m<-length(which(Data[,i]==CColumnValue[j,]))
       dataset1[j]<-m

           }
     q<-min(which((dataset1==max(dataset1)),arr.ind=T))

     New_Mode[i]<-CColumnValue[q,]
}
NNew_Mode<-matrix(c(New_Mode),nrow=1)
}
