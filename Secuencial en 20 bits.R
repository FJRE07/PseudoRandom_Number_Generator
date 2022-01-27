Sec<-function(Xo,n){

Sec20<-matrix(0,nrow=20, ncol= 1); Sec20[1]<-1; Sec20[18]<-1
c3<-cbind(numeric(19),diag(19)) 
Sec20<-rbind(c3,t(Sec20))
bin<-c()
for (j in 0:19) {
  bin<-c(bin,2^j)
}
vect<-numeric(n)
for (i in 1:n) {
  B<-matrix(strtoi(intToBits(Xo)[1:20]),nrow = 20,ncol = 1)
  TR<-(Sec20%*%(B))%%2
  FIN<-bin%*%TR
  u<-FIN/(2^20)
  vect[i]<-u
  Xo<-FIN  
}
return(vect)

}

Xo<-150
n<-10
Sec(Xo,n)