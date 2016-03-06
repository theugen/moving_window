\name{check_parameter}
\alias{check_parameter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Belonging to a given range of values
}
\description{
The function checks if a parameter of class "numeric" belongs to a given range of values. Only suitable for closed intervals, i.e. [a,b].
}
\usage{
check_parameter(parameter, vmin, vmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parameter}{
Object of class "numeric", the object of which is tested the belonging to the interval.
}
  \item{vmin}{
Object of class "numeric", the minimum value of the interval.
}
  \item{vmax}{
Object of class "numeric", the maximum value of the interval.
}
}
\details{
This function doesn't return anything, it only gives an error (entering debug mode) if the parameter is outside the specified range.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Eugenio Thieme, eugen@thieme.it
}
\note{
Observations and bug segnalations are welcome.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

alpha <- 0.05 #A parameter to check, for example the threshold for the pvalue (like the other probabilities, must be in [0,1])
check_parameter(alpha, vmin=0, vmax=1)

******

probability <- 2 #A parameter to check
check_parameter(probability, vmin=0, vmax=1) #This obviously returns an error, because probability must be in [0,1]

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
