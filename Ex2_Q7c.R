set.seed(75)
aMat <- matrix( sample(10, size=60, replace=T), nr=6)
aMat
cTot <- apply(aMat,2,sum)
#  cTot <- colSums(aMat)
#outer(cTot,cTot, "+")
#outer(cTot,cTot, "+")>75
#which(outer(cTot,cTot, "+")>75)
#which(outer(cTot,cTot, "+")>75)%/%10+1
#which(outer(cTot,cTot, "+")>75)%%10
#   which(outer(cTot,cTot, "+")>75,arr.ind=T)
m<-cbind(which(outer(cTot,cTot, "+")>75)%/%10+1)
m<-cbind(m,which(outer(cTot,cTot, "+")>75)%%10)

#Eliminate duplicates
m[which(m[,2]-m[,1]>=0),]

