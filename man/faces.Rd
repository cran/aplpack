\name{faces}
\alias{faces}
\alias{plot.faces}
\title{    Chernoff Faces    }
\description{
     \code{faces} represent the rows of a data matrix by faces.
     \code{plot.faces} plots faces into a scatterplot.
}
\usage{
faces(xy, which.row, fill = FALSE, nrow.plot, ncol.plot, scale = TRUE, 
         byrow = FALSE, main, labels, na.rm = FALSE, plot.faces=TRUE)
plot.faces(x, x.pos, y.pos, width = 1, height = 1, labels, ...)
}
\arguments{
  \item{xy}{   \code{xy} data matrix, rows represent individuals and columns variables  }
  \item{which.row}{   defines a permutation of the rows of the input matrix   }
  \item{fill}{   \code{if(fill==TRUE)}, only the first \code{nc} attributes of the faces are
                  transformed, \code{nc} is the number of columns of \code{xy}   }
  \item{nrow.plot}{   number of columns of faces on graphics device   }
  \item{ncol.plot}{   number of rows of faces   }
  \item{scale}{   \code{if(scale==TRUE)}, variables will be normalized   }
  \item{byrow}{   \code{if(byrow==TRUE)}, \code{xy} will be transposed   }
  \item{main}{   title   }
  \item{labels}{   character strings to use as names for the faces   }
  \item{na.rm}{ if TRUE 'NA' values are removed otherwise exchanged by mean of data}
  \item{plot.faces}{   if \code{FALSE} no face is plotted  }
  \item{x}{ an object of class \code{faces} computed by \code{faces}   }
  \item{x.pos}{ x coordinates of positions of faces  }
  \item{y.pos}{ y coordinates of positions of faces  }
  \item{width}{ width of the faces }
  \item{height}{ height of the faces }
  \item{...}{ additional graphical arguments }
}
\details{
Explanation of parameters:
1-height of face,
2-width of face,
3-shape of face,
4-height of mouth,
5-width of mouth,
6-curve of smile,
7-height of eyes,
8-width of eyes,
9-height of hair,
10-width of hair,
11-styling of hair,
12-height of nose,
13-width of nose,
14-width of ears,
15-height of ears. For details look at the literate program of \code{faces}
}
\value{
  list of standardized faces of \code{class faces},
  this object could be plotted by plot.faces;
  a plot of faces is created on the graphics device if 
  \code{plot.faces=TRUE}.
}
\references{  Chernoff, H. (1973): The use of faces to represent statistiscal assoziation,
JASA, 68, pp 361--368.
The smooth curves are computed by an algorithm found in
Ralston, A. and Rabinowitz, P. (1985):
A first course in numerical analysis, McGraw-Hill, pp 76ff.
\url{http://www.wiwi.uni-bielefeld.de/~wolf/} : S/R - functions : faces
   }
\author{   H. P. Wolf    }
\note{ version 04/2006   }

\seealso{   ---    }
\examples{

faces(rbind(1:3,5:3,3:5,5:7))

data(longley)
faces(longley[1:9,])

set.seed(17)
faces(matrix(sample(1:1000,128,),16,8),main="random faces")

a<-faces(rbind(1:3,5:3,3:5,5:7),plot.faces=FALSE)
plot(0:5,0:5,type="n")
plot(a,x.pos=1:4,y.pos=1:4,1.5,0.7)
}
\keyword{misc}



