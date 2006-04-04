plot.faces<-function(x,x.pos,y.pos,width=1,height=1,labels,...){
  if(missing(x)) return("no face.list object in call")
  face.list<-x
  if(class(face.list)!="faces") {
      if(!is.list(face.list) || !any(names(face.list[[1]])=="lipso") )
        return("input not of class faces")
  }
  spline<-function(a,y,m=200,plot=FALSE){
      n<-length(a)
    h<-diff(a)
    dy<-diff(y)
    sigma<-dy/h
    lambda<-h[-1]/(hh<-h[-1]+h[-length(h)])
    mu<-1-lambda
    d<-6*diff(sigma)/hh
    tri.mat<-2*diag(n-2)
    tri.mat[2+  (0:(n-4))*(n-1)] <-mu[-1]
    tri.mat[    (1:(n-3))*(n-1)] <-lambda[-(n-2)]
    M<-c(0,solve(tri.mat)%*%d,0)
    x<-seq(from=a[1],to=a[n],length=m)
    anz.kl <- hist(x,breaks=a,plot=FALSE)$counts
    adj<-function(i) i-1
    i<-rep(1:(n-1),anz.kl)+1
    S.x<-  M[i-1]*(a[i]-x          )^3 / (6*h[adj(i)])  +
           M[i]  *(x        -a[i-1])^3 / (6*h[adj(i)])  +
           (y[i-1] - M[i-1]*h[adj(i)]^2 /6) * (a[i]-x)/ h[adj(i)] +
          (y[i]   - M[i]  *h[adj(i)]^2 /6) * (x-a[i-1]) / h[adj(i)]
    if(plot){ plot(x,S.x,type="l"); points(a,y)    }
    return(cbind(x,S.x))
  }

  n<-length(face.list)
  if(missing(x.pos)){
     co<-ro<-ceiling(n^0.5)
     plot(0:(1+ro),0:(1+co),type="n",xlab="",ylab="",axes=FALSE)
     m<-matrix(1,ro,co); x.pos<-col(m); y.pos<-(1+ro)-row(m)
  } 
  if(!missing(labels)) names(face.list)<-labels
  fac.x<-width/1.1/210; fac.y<-height/1.3/210
  for(j in seq(face.list)){
    face.obj<-face.list[[j]]
    for(ind in seq(face.obj)) {
       x <-face.obj[[ind]][,1]; y<-face.obj[[ind]][,2]
       xx<-spline(1:length(x),x,40,FALSE)[,2]
       yy<-spline(1:length(y),y,40,FALSE)[,2]
       xx<-x.pos[j]+fac.x*xx; yy<-y.pos[j]+fac.y*yy
       lines(xx,yy,...)
    }
    lab<-names(face.list)[j]
    text(x.pos[j],y.pos[j]-0.5*height,lab)
  }
}

