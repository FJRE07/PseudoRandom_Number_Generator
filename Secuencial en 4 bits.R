#SECUENCIAL 4 BITS

Sec<-function(Xo,n){
    cc<-c(0,0,1);c1<-c(0,0,0,1)
    Sec4<-cbind(c1,rbind(diag(3),cc))
    vect<-numeric(n)
    for (i in 1:n) {
      B<-matrix(strtoi(intToBits(Xo)[1:4]),nrow = 4,ncol = 1)
      TR<-(Sec4%*%(B))%%2
      bin<-c(1,2,4,8)
      FIN<-bin%*%TR
      u<-FIN/(2^4)
      vect[i]<-u
      Xo<-FIN
    }
    return(vect)
    }

Xo<-150;n<-10
Sec(Xo,n)