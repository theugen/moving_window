\name{pvalue_control}
\alias{pvalue_control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checking pvalue threshold
}
\description{
The function takes an object of class "lm" and an object of class "numeric" providing a threshold for pvalue. If pvalue is greater or equal than threshold it returns FALSE, TRUE otherwise.
}
\usage{
pvalue_control(x, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class "lm", from which you want to evaluate the pvalue.
}
  \item{alpha}{
It's an object of class "numeric", representing the maximum threshold for pvalue.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns TRUE if pvalue is smaller than the threshold provided (p<alpha), FALSE otherwise.

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

y <- runif(50,0,50)
x <- c(1:50)
test <- lm(y~x)
bool <- pvalue_control(test, alpha=0.05) #Pass the result of lm and a max value for pvalue
                                         #In this example, alpha=5%

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
