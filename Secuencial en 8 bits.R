Sec<-function(Xo,n){

    Sec8<-matrix(0,nrow=8,ncol = 1)
    for (i in 1:8) {
      if(i!=8){
        Sec8[i]<-0
      }
      else{
        Sec8[i]<-1
      }
      c2<-c(0,0,0,1,1,1,0)
    };Sec8<-cbind(Sec8,rbind(diag(7),c2))
    bin<-c(); for (j in 0:7) {
      bin<-c(bin,2^j)
    }
    vect<-numeric(n)
    for (i in 1:n) {
      B<-matrix(strtoi(intToBits(Xo)[1:8]),nrow = 8,ncol = 1)
      TR<-(Sec8%*%(B))%%2
      FIN<-bin%*%TR
      u<-FIN/(2^8)
      vect[i]<-u
      Xo<-FIN  
    }
    return(vect)
}
Xo<-150
n<-10

Sec(Xo,n)