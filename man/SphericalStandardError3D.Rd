\name{SphericalStandardError3D}
\alias{SphericalStandardError3D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of the Standard Error of the angles}
\description{
   This function calculates the standard error of the arithmetic mean of a set of input coordinates.
}
\usage{
SphericalStandardError3D(coord)
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
  The standard error of all input coordinates.
}
\references{
  Fisher N.I. , Lewis T. , Embleton, B.J.J. (1987) Statistical analysis of spherical data. Cambridge. Cambridge University Press.

  Website \url{http://vecstat3d.mhostweb.com/}
}
\author{Juan Carlos Ruiz Cuetos \email{bilba_t@hotmail.com}, Maria Eugenia Polo Garcia \email{mepolo@unex.es}, Pablo Garcia Rodriguez \email{pablogr@unex.es}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{MeanModule3D}}, \code{\link{MeanDirection3D}}, \code{\link{AllAngleStatistics}}, 
   \code{\link{VonMisesParameter3D}}.
}
\examples{
   FileName<-system.file("data/XYZcoor.txt", package="VecStatGraphs3D")
   dat<-LoadData3D(FileName, Type=3)
   coordinates<-dat[,4:6]
   SphericalStandardError3D(coordinates)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{angle}
