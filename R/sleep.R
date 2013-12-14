## read sleepbot data from CSV
read.sb <- function(file, datefmt="%d/%m/%y") {
  dat <- read.csv(file)
  day <- strptime(dat$Date,format=datefmt)
  sleeptime <- as.vector(difftime(strptime(dat$Sleep.Time, format="%H:%M"),
                                  strptime("00:00", format="%H:%M"),
                                  units="hours"))
  waketime <- as.vector(difftime(strptime(dat$Wake.Time, format="%H:%M"),
                                 strptime("00:00", format="%H:%M"),
                                 units="hours"))
  hours <- dat$Hours
  Data <- data.frame(day, sleeptime, waketime, hours)
}

## convert sleep hours to timeseries
sb2hts <- function(Data) {
  library(xts)
  hours <- tapply(Data$hours,INDEX=Data$day,FUN=sum)
  Day <- unique(Data$day)
  hts <- xts(hours, Day, frequency=7)
}

## plot timeseries of sleep hours
plot.hts <- function(hts,
                     type=c("ts","month","weekday")) {
  type <- match.arg(type)
  if(type=="ts") {
    library(xts)
    plot(hts,
         ylim=c(0,max(hts)),main="",ylab="sleeping time (hours)")
  }
  if(type=="month") {
    boxplot(as.vector(hts)~format(index(hts),"%Y-%m"), varwidth=T,
            ylim=c(0,max(hts)),main="",ylab="sleeping time (hours)")
  }
  if(type=="weekday") {
    lab <- format(seq.POSIXt(strptime("1970-01-04",
                                      format="%Y-%m-%d"),
                             by="day",length.out=7),
                  format="%a")
    boxplot(as.vector(hts)~format(index(hts),"%w"), varwidth=T,
            names=lab,
            ylim=c(0,max(hts)),main="",ylab="sleeping time (hours)")
  }
}

## calculate weekly cycle
weekly <- function(hts) {
  idx <- format(index(hts),"%w")
  hts.w <- tapply(hts, idx, FUN=mean, na.rm=T)
}

## remove weekly cycle
unweek <- function(hts) {
  nd <- length(hts)
  hts.w <- rep(weekly(hts), nd%/%7+1)[1:nd] 
  hts.u <- hts - hts.w + mean(hts, na.rm=T)
}

## calculate trend, after removal of weekly cycle
hts.trend <- function(hts) {
  library(openair)
  DAT <- data.frame(index(hts), hts)
  colnames(DAT) <- c("date", "sleeping")
  FUN <- TheilSen
  res <- FUN(DAT, pollutant="sleeping", deseason=F, autocor=T)
  return(res)
}
