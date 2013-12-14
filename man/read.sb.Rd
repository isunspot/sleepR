\name{read.sb}
\alias{read.sb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
read the CSV exported from Sleepbot (Android app)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
read the CSV exported from Sleepbot (Android app)
}
\usage{
read.sb(file, datefmt = "\%d/\%m/\%y")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
The path and name of the CSV file to be read.
}
  \item{datefmt}{
%%     ~~Describe \code{datefmt} here~~
A string describing the format of the date (see \code{\link[base]{format.POSIXct}}).
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
A data frame with the same structure of the CSV.
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
