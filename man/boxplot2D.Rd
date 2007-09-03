\name{boxplot2D}
\alias{boxplot2D}
\title{ Boxplot of projection of two dimensional data }
\description{
  boxplot2D computes summary statistics of a one dimensional 
  projection of a two dimensional data set and plots a sloped
  boxplot of the statistics into the scatterplot of the two 
  dimensional data set.
}
\usage{
    boxplot2D(xy, add.to.plot = TRUE, box.size = 10, box.shift = 0, 
    angle = 0, angle.type = "0", tukey.style = TRUE, coef.out = 1.5, 
    coef.h.out = 3, design = "sl", na.rm=FALSE, ...)
}
\arguments{
  \item{xy}{ \code{(nx2)}-matrix, two dimensional data set  }
  \item{add.to.plot}{ if \code{TRUE} the boxplot is added to
      the actual plot of the graphics device }
  \item{box.size}{ height of the box (of the boxplot) }
  \item{box.shift}{ shift of boxplot perpendicular to the 
  projection direction }
  \item{angle}{ direction of projection in units defined by 
  angle.type }
  \item{angle.type}{ 
    \code{"0"}: angle in (0,2*pi),
    \code{"1"}: clock-like: angle.typ.0==2*pi*angle.typ.1/12,  
    \code{"2"}: degrees: angle.typ.0==2*pi*angle.typ.2/360, 
    \code{"3"}: by fraction: delta.y/delta.x
 }
  \item{tukey.style}{ if \code{TRUE} outliers are defined as described 
    in Tukey (1977)
 }
  \item{coef.out}{ outliers are values that are more than
     \code{coef.out*boxwidth} away from the box, default: 
     \code{coef.out=1.5}  }
  \item{coef.h.out}{ heavy outliers are values that are more
      than \code{coef.h.out*boxwidth} away from the box,
      default: \code{coef.h.out=3}   }
  \item{design}{ if \code{sl} then parallelogram else box  }
  \item{na.rm}{ if TRUE 'NA' values are removed otherwise exchanged by mean}
  \item{\dots}{ additional graphical parameters }
}
\references{
    Tukey, J.
    \emph{Exploratory Data Analysis.}
    Addison-Wesley, 1977.
    }
\author{Peter Wolf }
\note{ version 08/2003 }
\seealso{ \code{\link[graphics]{boxplot}}}
\examples{
 xy<-cbind(1:100, (1:100)+rnorm(100,,5))
 plot(xy,xlim=c(-50,150),ylim=c(-50,150))
 boxplot2D(xy,box.shift=-30,angle=3,angle.typ=1)
 boxplot2D(xy,box.shift=20,angle=1,angle.typ=1)
 boxplot2D(xy,box.shift=50,angle=5,angle.typ=1)
}
\keyword{misc}

