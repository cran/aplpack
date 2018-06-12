spin3R <- function(x, alpha=1, delay=.015, na.rm=FALSE){
 #################################################################
 # spin3R: simple spin function to rotate a 3-dim cloud of points#
 # pwolf 070831 / 080520                                             #
 #                                                               #
 # arguments:                                                    #
 #                                                               #
 #  x             (nx3)-matrix of points                         #
 #  alpha         arc of rotation                                #
 #  delay         sleeping time between rotations                #
 #                                                               #
 #################################################################
if(ncol(x)!=3) { print("Error: data matrix must have 3 columns"); return() }
 # require(tcltk) # 180308
 bw <- 4
 topl<-tktoplevel();   tkwm.geometry(topl,"+0+500")
 f1 <- tkframe(topl);f2 <- tkframe(topl);f3 <- tkframe(topl)
 f4 <- tkframe(topl);f5 <- tkframe(topl);tkpack(f1,f2,f3,f4,f5)
 f6 <- tkframe(topl); tkpack(f6)
 b12 <- tkbutton(f1, relief="ridge",  width=bw, text="up")
 b21 <- tkbutton(f2, relief="ridge",  width=bw, text="left")
 b22 <- tklabel(f2,  relief="flat",   width=bw)
 b23 <- tkbutton(f2, relief="ridge",  width=bw, text="right")
 b32 <- tkbutton(f3, relief="ridge",  width=bw, text="down")
 b41 <- tkbutton(f4, relief="ridge",  width=bw, text="clock")
 b42 <- tklabel(f4,  relief="flat",   width=bw)
 b43 <- tkbutton(f4, relief="ridge",  width=bw, text="cclock")
 b51 <- tkbutton(f5, relief="flat", width=bw, text="------")
 b52 <- tklabel(f5,  relief="flat",   width=bw, height=bw)
 b53 <- tkbutton(f5, relief="flat", width=bw, text="------")
 b61 <- tkbutton(f6, relief="raised", width=3*bw, text="EXIT")
 tkpack(b12,b32)
 tkpack(b21,b22,b41,b42,b51,b52,side="left")
 tkpack(b23,b43,b53,side="right")
 tkpack(b61)

 alpha<-alpha/360*2*pi; ca<-cos(alpha); sa<-sin(alpha)
 rot<-matrix(c(ca,-sa,sa,ca),2,2)

 n <- nrow(x)
 if(any(is.na(x))){
   if(na.rm){ x<-x[!apply(is.na(x),1,any),,drop=FALSE]
     print("Warning: NA elements have been removed!!")
   }else{
     xy.means<-colMeans(x,na.rm=TRUE)
     for(j in 1:ncol(x)) x[is.na(x[,j]),j]<-xy.means[j]
     print("Warning: NA elements have been exchanged by mean values!!")
   }  
 }
 x  <- x - matrix(apply(x,2,min),n,3,TRUE)
 x.o<-x<-x / matrix(apply(x,2,max),n,3,TRUE) - 0.5;                
 xa <- x[,2:3]
 A.o<-A<-0.5*matrix(c(1,0,0, 0,0,0, 0,1,0, 0,0,0, 0,0,1),5,3,TRUE)
 Aa <- A[,2:3]
 plot(xa, xlim=.7*c(-1,1), ylim=.7*c(-1,1),bty="n",
           pch=20, xlab="",ylab="",xaxt="n",yaxt="n",type="n")
 lines(Aa)

 i <- 0               ; i.max<-60
 cat("exit by button Exit\n")
 delay<-.01
 if(delay < 0.015) delay <- 0.005 
 colors<-rev(rainbow(ncolors<-30))

 # first view
 ind<-c(1,3); x0 <- x; x0[,ind] <- x[,ind]%*%rot; xb <- x0[,2:3]
 xcol<-colors[ceiling((x0[,1]+1)*0.5*ncolors)]
 points(xb, pch=20, col=xcol,cex=2*(1.5+x0[,1]))
 cex <- .3*2*(1.5+x0[,1])
 points(xb + 0.005 * cex, pch=20, col="white",cex=cex^2);
 xa<-x0[,2:3]
 # info box
 tkmessageBox(title="ready for spinning?", # icon="yesno",
              message=paste("move cursor to with the desired rotation",
                            "move cursor on 'EXIT' to exit",sep="\n"))   
 # loop
 repeat{
   Sys.sleep(delay) ##; if(  ((i<-i+1)>i.max) ){ break }
   cx<-tclvalue(tkwinfo("pointerx",topl))
   cy<-tclvalue(tkwinfo("pointery",topl))
   widget.id<-tclvalue(tkwinfo("containing",cx,cy))
   choice<-sub(paste(sep="",topl$ID,".*([1-6].[1-6]$)"),"\\1",widget.id)
   if(choice=="6.1") break
   switch(choice,
    "1.1" = ind<-c(1,3), "2.1" = ind<-c(2,1), "2.3" = ind<-c(1,2),
    "3.1" = ind<-c(3,1), "4.1" = ind<-c(3,2), "4.3" = ind<-c(2,3), next
   )
   x[,ind] <- x[,ind]%*%rot; A[,ind] <- A[,ind]%*%rot
   xb<-x[,2:3]; Ab<-A[,2:3] #; dcol<-c(rep("black",n),rep("white",n))
   xcol<-colors[ceiling((x[,1]+1)*0.5*ncolors)]
   dcol<-c(xcol,rep("white",n))
   points(rbind(xb,xa), pch=20, col=dcol,cex=2*c(1.5+x[,1],rep(3,n))) # 1*
 #  points(xb, pch=20, col="black",cex=1.5+x[,1]); points(xa, pch=20, cex=3, col="white")
   lines(Aa, col="white",lwd=3)
   points(xb, pch=20, col=xcol,cex=2*(1.5+x[,1])) # 1*
   cex <- .3*2*(1.5+x[,1])
   points(xb + 0.005 * cex, pch=20, col="white",cex=cex^2);
   lines(Ab, col="black")
   xa<-x[,2:3]; Aa<-A[,2:3]
 }
 print(cex)
 tkdestroy(topl)
 "control widget closed"

} # end of spin3R

