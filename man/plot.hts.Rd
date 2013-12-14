\name{plot.hts}
\alias{plot.hts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
plot time series of sleeping time
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
plot sleeping time, as daily time series or as boxplot, categorized on month or on day of the week 
}
\usage{
plot.hts(hts, type = c("ts", "month", "weekday"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hts}{
%%     ~~Describe \code{hts} here~~
An object of class \code{xts}
}
  \item{type}{
%%     ~~Describe \code{type} here~~
If "ts", the time series is plotted. If "month", data are aggregted in box-whiskers on monthly basis. If "weekday", boxplot are based on the day of the week.
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
