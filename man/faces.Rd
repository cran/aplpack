\name{faces}
\alias{faces}
\alias{plot.faces}
\title{    Chernoff Faces    }
\description{
     \code{faces} represent the rows of a data matrix by faces.
     \code{plot.faces} plots faces into a scatterplot.
}
\usage{
faces(xy, which.row, fill = FALSE, face.type = 1, nrow.plot, ncol.plot, 
    scale = TRUE, byrow = FALSE, main, labels, print.info = TRUE, 
    na.rm = FALSE, ncolors = 20, col.nose = rainbow(ncolors), 
    col.eyes = rainbow(ncolors, start = 0.6, end = 0.85), 
    col.hair = terrain.colors(ncolors), col.face = heat.colors(ncolors), 
    col.lips = rainbow(ncolors, start = 0, end = 0.2), 
    col.ears = rainbow(ncolors, start = 0, end = 0.2), plot.faces = TRUE) 
\method{plot}{faces}(x, x.pos, y.pos, face.type = 1, width = 1, height = 1, labels, 
        ncolors = 20, col.nose = rainbow(ncolors), col.eyes = rainbow(ncolors, 
        start = 0.6, end = 0.85), col.hair = terrain.colors(ncolors), 
        col.face = heat.colors(ncolors), col.lips = rainbow(ncolors, 
        start = 0, end = 0.2), col.ears = rainbow(ncolors, start = 0, 
        end = 0.2), \ldots)
}
\arguments{
  \item{xy}{   \code{xy} data matrix, rows represent individuals and columns variables  }
  \item{which.row}{   defines a permutation of the rows of the input matrix   }
  \item{fill}{   \code{if(fill==TRUE)}, only the first \code{nc} attributes of the faces are
                  transformed, \code{nc} is the number of columns of \code{xy}   }
  \item{face.type}{  an integer between 0 and 2 with the meanings: 
                     0 = line drawing faces, 
                     1 = the elements of the faces are painted,
                     2 = Santa Claus faces are drawn }
  \item{nrow.plot}{   number of columns of faces on graphics device   }
  \item{ncol.plot}{   number of rows of faces   }
  \item{scale}{   \code{if(scale==TRUE)}, variables will be normalized   }
  \item{byrow}{   \code{if(byrow==TRUE)}, \code{xy} will be transposed   }
  \item{main}{   title   }
  \item{labels}{   character strings to use as names for the faces   }
  \item{print.info}{   if TRUE information about usage of variables for face elements are printed }
  \item{na.rm}{ if TRUE 'NA' values are removed otherwise exchanged by mean of data}
  \item{plot.faces}{   if \code{FALSE} no face is plotted  }
  \item{x}{ an object of class \code{faces} computed by \code{faces}   }
  \item{x.pos}{ x coordinates of positions of faces  }
  \item{y.pos}{ y coordinates of positions of faces  }
  \item{width}{ width of the faces }
  \item{height}{ height of the faces }
  \item{ncolors}{ number of colors in the palettes for painting the elements of the faces }
  \item{col.nose}{ palette of colors for painting the nose }
  \item{col.eyes}{ palette of colors for painting the eyes }
  \item{col.hair}{ palette of colors for painting the hair }
  \item{col.face}{ palette of colors for painting the face }
  \item{col.lips}{ palette of colors for painting the lips }
  \item{col.ears}{ palette of colors for painting the ears }
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
15-height of ears. 

For painting elements of a face the colors of are found by 
averaging of sets of variables: 
(7,8)-eyes:iris, (1,2,3)-lips, 
(14,15)-ears, (12,13)-nose, (9,10,11)-hair, (1,2)-face.

Further details can be found in the literate program of \code{faces}.
}
\value{
  list of two elements: The first element \code{out$faces} 
  is a list of standardized faces of \code{class faces},
  this object could be plotted by plot.faces;
  a plot of faces is created on the graphics device if 
  \code{plot.faces=TRUE}.
  The second list is short description of the effects of the variables.
}
\references{  Chernoff, H. (1973): The use of faces to represent statistiscal assoziation,
JASA, 68, pp 361--368.
The smooth curves are computed by an algorithm found in
Ralston, A. and Rabinowitz, P. (1985):
A first course in numerical analysis, McGraw-Hill, pp 76ff.
\url{http://www.wiwi.uni-bielefeld.de/mitarbeiter/wolf/} : 
S/R - functions : faces
   }
\author{   H. P. Wolf    }
\note{ version 01/2009   }

\seealso{   ---    }
\examples{

faces()
faces(face.type=1)

faces(rbind(1:3,5:3,3:5,5:7))

data(longley)
faces(longley[1:9,],face.type=0)
faces(longley[1:9,],face.type=1)

plot(longley[1:16,2:3],bty="n")
a<-faces(longley[1:16,],plot=FALSE)
plot.faces(a,longley[1:16,2],longley[1:16,3],width=35,height=30)

set.seed(17)
faces(matrix(sample(1:1000,128,),16,8),main="random faces")

a<-faces(rbind(1:3,5:3,3:5,5:7),plot.faces=FALSE)
plot(0:5,0:5,type="n")
plot(a,x.pos=1:4,y.pos=1:4,1.5,0.7)
# during Christmastime
faces(face.type=2)
}
\keyword{misc}

