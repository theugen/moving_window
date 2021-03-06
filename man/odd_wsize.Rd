\name{odd_wsize}
\alias{odd_wsize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check the oddity of the window
}
\description{
This function checks if the size of the window provided is odd.
If not, it gives a warning and it returns a window size equal to
the first odd integer contained in the provided size.
}
\usage{
odd_wsize(wsize)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wsize}{
%%     ~~Describe \code{wsize} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (wsize) 
{
    if (wsize\%\%2 == 0) {
        warning("Even window size! Size will be coerced to first odd integer contained in the actual size.")
        wsize <- (wsize - 1)
    }
    return(wsize)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
