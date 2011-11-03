\name{slider}
\alias{slider}
\alias{gslider}
%- Also NEED an '\alias' for EACH other topic documented here.
%-  cp slider2.Rd /home/wiwi/pwolf/work/relax/install.dir/relax/man/slider.Rd
\title{slider / button control widgets}
\description{
  \code{slider} and \code{gslider} construct a Tcl/Tk-widget with sliders and buttons to
  demonstrate the effects of variation of parameters on calculations and plots. 
}
\usage{
slider(sl.functions, sl.names, sl.mins, sl.maxs, sl.deltas, sl.defaults, but.functions,
  but.names, no, set.no.value, obj.name, obj.value, reset.function, title, prompt=FALSE,
  sliders.frame.vertical=TRUE)

gslider(sl.functions, sl.names, sl.mins, sl.maxs, sl.deltas, sl.defaults, but.functions,
  but.names, no, set.no.value, obj.name, obj.value, reset.function, title, prompt=FALSE,
  sliders.frame.vertical=TRUE, hscale=1, vscale=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sl.functions}{set of functions or function connected to the slider(s)}
  \item{sl.names}{labels of the sliders}
  \item{sl.mins}{minimum values of the sliders' ranges}
  \item{sl.maxs}{maximum values of the sliders' ranges}
  \item{sl.deltas}{change of step per click}
  \item{sl.defaults}{default values for the sliders}
  \item{but.functions}{function or list of functions
          that are assigned to the button(s)}
  \item{but.names}{labels of the buttons}
  \item{no}{\code{slider(no=i)} requests slider \code{i}}
  \item{set.no.value}{\code{slider(set.no.value=c(i,val))} sets slider \code{i} to value \code{val}}
  \item{obj.name}{\code{slider(obj.name=name)} requests the value of 
  variable \code{name} from environment \code{slider.env}}
  \item{obj.value}{\code{slider(obj.name=name,obj.value=value)} assigns \code{value}
  to variable \code{name} in environment \code{slider.env}}
  \item{reset.function}{function that induce a \code{reset.button} and 
        contains the commands of it.}
  \item{title}{title of the control window}
  \item{prompt}{ if TRUE slider functions are called by moving a slider,
                 if FALSE slider functions are called after releasing the mouse button}
  \item{sliders.frame.vertical}{ if TRUE the sliders are stacked one above the other;
                 otherwise they are positioned side by side }
  \item{hscale}{horizontal scale factor for image size; compare tkrplot in package tkrplot}
  \item{vscale}{vertical scale factor for image size; compare tkrplot in package tkrplot}  
}
\details{
  \code{slider} constructs a separated panel for controlling the parameters whereas 
  \code{gslider} integrates a graphical device and buttons and sliders within one window. 

  The following actions can be done:
  a) definition of (multiple) sliders and buttons, 
  b) request or specification of slider values, and
  c) request or specification of variables in the environment \code{slider.env}.
  The management takes place in the environment \code{slider.env}. If \code{slider.env}
  is not found it is generated.

  \bold{Definition} ... of sliders: First of all you have to define sliders, buttons and the attributes of them.
  Sliders are established by six arguments:
  \code{sl.functions, sl.names, sl.minima, sl.maxima,sl.deltas}, and \code{sl.defaults}.
  The first argument,
  \code{sl.functions}, is either a list of functions or a single function that 
  contains the commands for the sliders.
  If there are three sliders and slider 2 is moved with the mouse the function stored in
  \code{sl.functions[[2]]} (or in case of one function for all sliders the function \code{sl.functions})
  is called.

  \bold{DEFINITION} ... of buttons: Buttons are defined by a vector of labels \code{but.names} and a list of functions:
  \code{but.functions}. If button \code{i} is pressed the function stored in \code{but.functions[[i]]} is called.

  \bold{REQUESTING} ... a slider: 
  \code{slider(no=1)} returns the actual value of slider 1,
  \code{slider(no=2)} returns the value of slider 2, etc. 
  You are allowed to include expressions of the type
  \code{slider(no=i)} in functions describing the effect of
  sliders or buttons.

  \bold{SETTING} ... a slider: \code{slider(set.no.value=c(2,333))} sets slider \code{2} to value 333.
  \code{slider(set.no.value=c(i,value))} can be included in the functions 
  defining the effects of moving sliders or pushing buttons.

  \bold{VARIABLES} ... of the environment \code{slider.env}: Sometimes information
  has to be trransferred back and forth between functions defining the effects of sliders and buttons.
  Imagine for example two sliders: one to control \code{p} and another one to control \code{q},
  but they should satisfy: \code{p+q=1}. Consequently, you have to correct the value of the first slider
  after the second one was moved. To prevent the creation of  global 
  variables store them in the environment \code{slider.env}.
  Use \code{slider(obj.name="p.save",obj.value=1-slider(no=2))}
  to assign value  \code{1-slider(no=2)} to the variable \code{p.save} .
  \code{slider(obj.name=p.save)} returns the value of variable \code{p.save}.

  \bold{Dependencies} The function \code{gslider} depends on package \code{tkrplot}.
}
\value{
 Using \code{slider} in definition mode \code{slider} returns the value of new created the top level widget. 
  \code{slider(no=i)} returns the actual value of slider \code{i}.
  \code{slider(obj.name=name)} returns the value of variable \code{name} in environment \code{slider.env}.
 \code{gslider} return in definition mode the result of \code{tkrplot} which was called to construct the widget.
}
\author{Hans Peter Wolf}

\examples{

# example 1, sliders only
\dontrun{
## This example cannot be run by examples() but should work in an interactive R session
plot.sample.norm<-function(){
 refresh.code<-function(...){
   mu<-slider(no=1); sd<-slider(no=2); n<-slider(no=3)
   x<-rnorm(n,mu,sd)
   plot(x)
 }
 slider(refresh.code,sl.names=c("value of mu","value of sd","n number of observations"),
       sl.mins=c(-10,.01,5),sl.maxs=c(+10,50,100),sl.deltas=c(.01,.01,1),sl.defaults=c(0,1,20))
}
plot.sample.norm()
}

# example 2, sliders and buttons
\dontrun{
## This example cannot be run by examples() but should work in an interactive R session
plot.sample.norm.2<-function(){
 refresh.code<-function(...){
   mu<-slider(no=1); sd<-slider(no=2); n<-slider(no=3)
   type=  slider(obj.name="type")
   x<-rnorm(n,mu,sd)
   plot(seq(x),x,ylim=c(-20,20),type=type)
 }
 slider(obj.name="type",obj.value="l")
 slider(refresh.code,sl.names=c("value of mu","value of sd","n number of observations"),
       sl.mins=c(-10,.01,5),sl.maxs=c(10,10,100),sl.deltas=c(.01,.01,1),sl.defaults=c(0,1,20),
       but.functions=list(
              function(...){slider(obj.name="type",obj.value="l");refresh.code()},
              function(...){slider(obj.name="type",obj.value="p");refresh.code()},
              function(...){slider(obj.name="type",obj.value="b");refresh.code()}
       ),
       but.names=c("lines","points","both"))
}
plot.sample.norm.2()
}

# example 2a, sliders and buttons and graphics in one window
\dontrun{
## This example cannot be run by examples() but should work in an interactive R session
plot.sample.norm.2<-function(){
 refresh.code<-function(...){
   mu<-slider(no=1); sd<-slider(no=2); n<-slider(no=3)
   type=  slider(obj.name="type")
   x<-rnorm(n,mu,sd)
   plot(seq(x),x,ylim=c(-20,20),type=type)
 }
 slider(obj.name="type",obj.value="l")
 gslider(refresh.code,sl.names=c("value of mu","value of sd","n number of observations"),
       sl.mins=c(-10,.01,5),sl.maxs=c(10,10,100),sl.deltas=c(.01,.01,1),sl.defaults=c(0,1,20),
       but.functions=list(
              function(...){slider(obj.name="type",obj.value="l");refresh.code()},
              function(...){slider(obj.name="type",obj.value="p");refresh.code()},
              function(...){slider(obj.name="type",obj.value="b");refresh.code()}
       ),
       but.names=c("lines","points","both"))
}
plot.sample.norm.2()
}

# example 3, dependent sliders
\dontrun{
## This example cannot be run by examples() but should work in an interactive R session
print.of.p.and.q<-function(){
 refresh.code<-function(...){
   p.old<-slider(obj.name="p.old")
   p<-slider(no=1); if(abs(p-p.old)>0.001) {slider(set.no.value=c(2,1-p))}
   q<-slider(no=2); if(abs(q-(1-p))>0.001) {slider(set.no.value=c(1,1-q))}
   slider(obj.name="p.old",obj.value=p)
   cat("p=",p,"q=",1-p,"\n")
 }
 slider(refresh.code,sl.names=c("value of p","value of q"),
       sl.mins=c(0,0),sl.maxs=c(1,1),sl.deltas=c(.01,.01),sl.defaults=c(.2,.8))
 slider(obj.name="p.old",obj.value=slider(no=1))
}
print.of.p.and.q()
}

# example 4, rotating a surface
\dontrun{
## This example cannot be run by examples() but should work in an interactive R session
R.veil.in.the.wind<-function(){
  # Mark Hempelmann / Peter Wolf
  par(bg="blue4", col="white", col.main="white", 
      col.sub="white", font.sub=2, fg="white") # set colors and fonts
  refresh.code<-function(...){
    samp        <- function(N,D) N*(1/4+D)/(1/4+D*N) 
    z<-outer(seq(1, 800, by=10), seq(.0025, 0.2, .0025)^2/1.96^2, samp) # create 3d matrix
    h<-100 
    z[10:70,20:25]<-z[10:70,20:25]+h; z[65:70,26:45]<-z[65:70,26:45]+h
    z[64:45,43:48]<-z[64:45,43:48]+h; z[44:39,26:45]<-z[44:39,26:45]+h
    x<-26:59; y<-11:38; zz<-outer(x,y,"+"); zz<-zz*(65<zz)*(zz<73)
    cz<-10+col(zz)[zz>0];rz<-25+row(zz)[zz>0]; z[cbind(cz,rz)]<-z[cbind(cz,rz)]+h
    theta<-slider(no=1); phi<-slider(no=2)
    persp(x=seq(1,800,by=10),y=seq(.0025,0.2,.0025),z=z,theta=theta,phi=phi, 
          scale=T, shade=.9, box=F, ltheta = 45,
          lphi = 45, col="aquamarine", border="NA",ticktype="detailed")   
  }
  slider(refresh.code, c("theta", "phi"), c(0, 0),c(360, 360),c(.2, .2),c(85, 270)  )
}
R.veil.in.the.wind()
}

}
\keyword{dynamic}% at least one, from doc/KEYWORDS
\keyword{iplot}% __ONLY ONE__ keyword per line


