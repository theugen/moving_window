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
"array" is the item to be interpolated, should be a vector.
}
  \item{time_array}{
A vector containing the time period in which the "array" argument should be interpolated.
It must be of the same length of "array", because they'll both passed to lm() in the form
of array ~ time_array.

}
  \item{window_size}{
The length of the window size on which the first element, "array" should be interpolated (the temporal scale is the same of the "time_array" object). It must be odd (the result of the interpolation is assigned to the median element), if even it's forced to the first odd integer contained in the size passed.
}
  \item{threshold}{
If the number of non-NA elements in "array" is below this threshold, the result of the linear interpolation on the window considered is forced to NA.
}
  \item{alpha}{
Threshold of significancy for p-value on angular coefficient.
}
}

\value{
The function returns a vector of the same length of "array" and "time_array". The first and the last floor(window_size/2) elements are always NA's (they're used for the estimate of the median element of the first and last iteration of the window), while the others contain the value of the angular coeffincient of the interpolating function (lm$coefficients[2]). 
If pvalue is above the significancy threshold ("alpha"), the corresponding value is set to 0, i.e. no trend.
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
