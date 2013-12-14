\name{sb2hts}
\alias{sb2hts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Convert Sleepbot data to daily time series.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Extract from a data frame with Sleepbot data a daily time series with the sleep time. 
}
\usage{
sb2hts(Data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
%%     ~~Describe \code{Data} here~~
A data frame with "day" and "hours" (sleep time). 
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
An object of class \code{\link[xts]{xts}} with the daily sleep time.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Giovanni Bonafe'
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
