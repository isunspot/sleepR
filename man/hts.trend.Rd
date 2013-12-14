\name{hts.trend}
\alias{hts.trend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
calculate trend of sleeping time
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
calculate trend of sleeping time, after removal of weekly cycle, and monthly averaged  
}
\usage{
hts.trend(hts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hts}{
%%     ~~Describe \code{hts} here~~
a time series of daily sleeping time
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{hts.trend} is based on \code{\link[openair]{TheilSen}}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
As well as generating the plot itself, it also returns an object which includes three main components:
\item{call }{the command used to generate the plot}
\item{data }{the data frame of summarised information used to make the plot}
\item{plot }{the plot itself}
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
\code{\link[openair]{TheilSen}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
