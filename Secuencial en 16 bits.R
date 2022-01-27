Sec<-function(Xo,n){
Sec16<-matrix(0,nrow=16,ncol = 1);Sec16[16]<-1
c4<-c(0,0,0,1,0,0,0,0,0,0,0,0,1,0,1)
c4<-rbind(diag(15),c4)
Sec16<-cbind(Sec16,c4)
bin<-c(); for (j in 0:15) {
  bin<-c(bin,2^j)
}
vect<-numeric(n)
for (i in 1:n) {
  B<-matrix(strtoi(intToBits(Xo)[1:16]),nrow = 16,ncol = 1)
  TR<-(Sec16%*%(B))%%2
  FIN<-bin%*%TR
  u<-FIN/(2^16)
  vect[i]<-u
  Xo<-FIN  
}
return(vect)
}
Xo<-150
n<-10
Sec(Xo,n)