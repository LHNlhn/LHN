New_ratio_distance_between_objects<-function(DData1,DData2)
{
ddateset<-vector()
DDistance<-vector()
  DDistance<-0
  ml<-length(union(DData1, DData2))
  gl<-length(intersect(DData1, DData2))
  DDistance<-(1-gl/ml)
  ddateset<-DDistance
}