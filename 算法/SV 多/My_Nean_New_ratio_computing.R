My_Nean_New_ratio_computing_modes<-function(data)
{
data1<-unique(data[,1])
q<-length(data1)
Vx<-vector()
for(i in 1:q)
{
  x<-data[data[,1]==data1[i],2]
  v<-as.data.frame(t(table(x)))[,c(2,3)]
  xx<-v[,2][match(x,v[,1])]
  m<-length(x)
  px<-xx/m
  Lx<-cbind(x,px)
  Vx<-rbind(Vx,Lx)
}
temp_Vx<-as.matrix(tapply(Vx[,2],Vx[,1],sum))
att_Vx<-sort(unique(data[,2]),decreasing = FALSE)
att_ind<-cbind(att_Vx,temp_Vx)
temp_value<-att_ind[order(att_ind[,2],decreasing = T),]
row.names(temp_value)<-c(1:length(att_Vx))
r<-round((sum(length(data[,2])))/q)


SS<-vector()
TT<-unique(temp_value[,2])
for(i in 1:length(TT))
{
  SS[i]<-length(which(TT[i]==temp_value[,2]))
}
TT_matrix<-matrix(c(TT),ncol=1)
SS_matrix<-matrix(c(SS),ncol=1)
Ttemp_value<-cbind(TT_matrix,SS_matrix)
nT<-nrow(Ttemp_value)


aa<-0
bb<-0
for(m in 1:nT)
{
  aa<-length(which(temp_value[,2]==max(temp_value[,2])))
  bb<-(bb+aa)
  result<-temp_value[(1:aa),]
  if (bb>=r)  break
  else  temp_value<-temp_value[-(1:aa),]
}


tempzz<-vector()
temp_value<-att_ind[order(att_ind[,2],decreasing = T),]
row.names(temp_value)<-c(1:length(att_Vx))
new_mode<-vector()
dd<-which(temp_value[,1]==result[1])[1]
new_mode<-as.matrix(temp_value[1:(dd-1),1])
tempzz<-t(combn(result,r-length(new_mode)))
nt<-nrow(tempzz)

union_value_F<-vector()
for(j in 1:nt)
{
  union_value_frequency<-0
  for(i in 1:length(new_mode))
  {
    union_value<-union(new_mode[i],tempzz[j,])
    for(e in 1:q)
    {
      data2<-data[data[,1]==data1[e],2]
      if(all(is.element(union_value,data2)))
      {
        union_value_frequency<-length(union_value)/length(data2)
        union_value_frequency<-union_value_frequency+1}
    }
  }
  union_value_F[j]<-union_value_frequency
}
cc<-which(union_value_F==max( union_value_F))[1]
NEW_modes<-union(new_mode,tempzz[cc,])
}
