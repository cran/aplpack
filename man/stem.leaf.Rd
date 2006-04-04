\name{stem.leaf}
\alias{stem.leaf}

\title{Stem-and-Leaf Display}
\description{
  Creates a classical ("Tukey-style") stem-and-leaf display.
}

\usage{
stem.leaf(data, unit, m, Min, Max, rule.line = c("Dixon", "Velleman", "Sturges"),
    style = c("Tukey", "bare"), trim.outliers = TRUE, depths = TRUE,
    reverse.negative.leaves = TRUE)
}

\arguments{
  \item{data}{a numeric vector.}
  \item{unit}{leaf unit, as a power of 10 (e.g., \code{100}, \code{.01});
    if \code{unit} is missing \code{unit} is choosen by \code{stem.leaf}.}
  \item{m}{number of parts (1, 2, or 5) into which each stem will be separated;
  if \code{m} is missing the number of parts/stem 
  (\code{m}) is choosen by \code{stem.leaf}.}
  \item{Min}{smallest non-outlying value; omit for automatic choice.}
  \item{Max}{largest non-outlying value; omit for automatic choice.}
  \item{rule.line}{the rule to use for choosing the desired number of lines
    in the display; \code{"Dixon"} = 10*log10(n); \code{"Velleman"} = 2*sqrt(n);
    \code{"Sturges"} = 1 + log2(n); the default is \code{"Dixon"}.}
  \item{style}{\code{"Tukey"} (the default) for "Tukey-style" divided stems;
    \code{"bare"} for divided stems that simply repeat the stem digits.}
  \item{trim.outliers}{if \code{TRUE} (the default), outliers are placed on \code{LO} and
    \code{HI} stems.}
  \item{depths}{if \code{TRUE} (the default), print a column of "depths" to the left of the
    stems; the depth of the stem containing the median is the stem-count enclosed in
    parentheses.}
  \item{reverse.negative.leaves}{if \code{TRUE} (the default), reverse direction the leaves on negative
    stems (so, e.g., the leaf 9 comes before the leaf 8, etc.).}
}

\details{
  Unlike the \code{stem} function in the \code{base} package, this function produces
  classic stem-and-leaf displays, as described in Tukey's \emph{Exploratory Data Analysis}.
}

\value{
  The computed stem and leaf display is printed out.
  Invisibly \code{stem.leaf} returns the stem and leaf
  display as a list containing the elements
  \code{info} (legend), \code{stem} (display as character vecter), \code{lower} (very small values) , 
  and \code{upper} (very large values).
}
\references{
    Tukey, J.
    \emph{Exploratory Data Analysis.}
    Addison-Wesley, 1977.
    }

\author{Peter Wolf, the code has been slightly modified by John Fox \email{jfox@mcmaster.ca}
    with the original author's permission, help page written by John Fox, the help page has been slightly modified by Peter Wolf.}

\seealso{\code{\link[base]{stem}}}

\examples{
stem.leaf(co2)
}

\keyword{misc}


