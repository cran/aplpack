\name{plotsummary}                                                                               
\alias{plotsummary}         
\title{  graphical summaries of variables of a data set }                                                                      
\description{                                                         
  \code{plotsummary} shows some important characteristics of the variables of a data set.
  For each variable a plot is computed consisting of a barplot, an ecdf, 
  a density trace and a boxplot. }                                                                      
\usage{ 
 plotsummary(data, trim = 0, types = c("stripes", "ecdf", "density", "boxplot"),
             y.sizes = 4:1, design = "chessboard", main, mycols = "RB")
 }                                                              
\arguments{                                                   
   \item{data}{   Data set for computing a graphical summary. }      
   \item{trim}{   \code{trim} defines the fraction of observation for trimming on both  
                  ends of the data.  }
   \item{types}{  vector of types of representation of the data set. 
                  The elements of the vector will induce small plots which are stacked 
                  in vertical order. The first letter of the types is sufficient for 
                  defining a type. } 
   \item{y.sizes}{defines the relative sizes of the small plots. 
                  The values are divided by their sum to get percentages. }                  
   \item{design}{ if \code{design} is \code{chessboard} the graphics device 
                  is fragmented into rows and cols. Otherwise the images of a variable 
                  build vertical stripes.  }                                                  
   \item{main}{   defines a title for the graphics.  }                                           
   \item{mycols}{ allows to define some colors for the showing the regions separated
                  by the quartils. }              
 }                                    
\details{        \code{plotsummary} can be use for a quick and dirty inspection 
                  of a data matrix or a list of variables. 
                  Without further specification some representation of each of the 
                  variables is built and stacked into a plot. 
                  The sizes of the types of representation can be set as well as the 
                  layout design of the graphics device. It is helpful to trim the data 
                  before processing because outliers will often hide 
                  the interesting characteristics. }                   
\author{                                                       
   Peter Wolf, pwolf@wiwi.uni-bielefeld.de
 }                                                               
 \seealso{                                                         
   \code{\link{pairs}},   \code{\link{summary}},    \code{\link{str}}
 }                                                                  
\examples{                                                        
 ##---- Should be DIRECTLY executable !! ----         
 ##-- ==>  Define data, use random,                      
 ##--\tor do  help(data=index)  for the standard data sets.   
 plotsummary(cars)
 plotsummary(cars, types=c("ecdf", "density", "boxplot"), 
             y.sizes = c(1,1,1), design ="stripes")
 plotsummary(c(list(rivers=rivers, co2=co2), cars), y.sizes=c(10,3,3,1), mycols=3)
 plotsummary(cars, design="chessboard")
 # find all matrices in your R
 ds.of.R <- function(type="vector"){
   dat <- ls(pos=grep("datasets",search()))
   dat.type <- unlist(lapply(dat,function(x) {       
      num <- mode(x<-eval(parse(text=x)))
      num <- ifelse(is.array(x),"array",num)
      num <- ifelse(is.list(x),"list",num)
      num <- ifelse(is.matrix(x),"matrix",num)
      num <- ifelse(is.data.frame(x),"matrix",num)
      num <- ifelse(num=="numeric","vector",num)
      num }))
   return(dat[dat.type==type])
 }
 namelist <- ds.of.R("matrix")
 # inspect the matrices one after the other
 for(i in seq(along=namelist)){
   print(i); print(namelist[i])
   xy <- get(namelist[i])
   # plotsummary(xy,y.sizes=4:1,trim=.05,main=namelist[i]) 
   # Sys.sleep(1)
 }
 }
\keyword{ hplot }
\keyword{ manip }% __ONLY ONE__ keyword per line                                                                           
