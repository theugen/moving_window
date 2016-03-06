\name{moving_window}
\alias{moving_window}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Linear Regression on Moving Window
}
\description{
A function for applying a linear interpolation (using lm()) on a moving window on a vector of class "numeric": it is passed the size of the window, computed the interpolation with lm() only on the values in that window, assigning the result to the element in the middle of the window, then the window is moved one element forward for all the length of the vector.
}
\usage{
moving_window(array, time_array, window_size, threshold, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{array}{
%%     ~~Describe \code{array} here~~
}
  \item{time_array}{
%%     ~~Describe \code{time_array} here~~
}
  \item{window_size}{
%%     ~~Describe \code{window_size} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
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
function (array, time_array, window_size, threshold, alpha) 
{
    initial_array <- array
    window_regr <- c()
    window_size <- odd_wsize(window_size)
    check_parameter(threshold, 0, 1)
    check_parameter(alpha, 0, 1)
    i <- 1
    while (((i + window_size) - 1) < length(initial_array)) {
        temp_array <- initial_array
        for (j in 1:length(temp_array)) {
            if (j < i | j > (i + window_size - 1)) 
                temp_array[j] <- NA
            if (j <= floor(window_size/2) | j >= (length(initial_array) - 
                floor(window_size/2))) 
                window_regr[j] <- NA
        }
        if (length(which(!is.na(temp_array))) >= (threshold * 
            window_size)) {
            par_lm <- lm(temp_array ~ time_array)
            bool_p <- pvalue_control(par_lm, alpha)
            if (bool_p == TRUE) 
                window_regr[(ceiling((window_size/2))) + i - 
                  1] <- par_lm$coefficients[2]
            else window_regr[(ceiling((window_size/2))) + i - 
                1] <- 0
        }
        else window_regr[(ceiling((window_size/2))) + i - 1] <- NA
        i <- i + 1
    }
    return(window_regr)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
