\name{MeanDirection3D}
\alias{MeanDirection3D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of the Mean Direction of the angles}
\description{
   This function calculates the Mean Direction of a set of input coordinates.
}
\usage{
MeanDirection3D(coord)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coord}{Matrix containing the values of the coordinates}
}
\details{
   One way to get a set of coordinates  X, Y and Z of the origin position and end position 
   (coordinates X, Y and Z of the vector) or of the colatitude and longitude, it is using the \code{\link{LoadData3D}} function.
}
\value{
   The Value Mean Direction of all input coordinates. The value is expressed in sexagesimal.
}
\references{
  Fisher N.I. , Lewis T. , Embleton, B.J.J. (1987) Statistical analysis of spherical data. Cambridge. Cambridge University Press.

  Website \url{http://vecstat3d.mhostweb.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{AllAngleStatistics}}, \code{\link{MeanModule3D}}, \code{\link{SphericalStandardError3D}}, 
   \code{\link{VonMisesParameter3D}}.
}
\examples{
   FileName<-system.file("data/XYZcoor.txt", package="VecStatGraphs3D")
   dat<-LoadData3D(FileName, Type=3)
   coordinates<-dat[,4:6]
   MeanDirection3D(coordinates)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{angle}
