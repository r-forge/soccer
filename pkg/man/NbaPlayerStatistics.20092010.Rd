\name{NbaPlayerStatistics.20092010}
\alias{NbaPlayerStatistics.20092010}
\docType{data}
\title{NBA player statistics 2009-2010 (regular season) from dougstats.com}
\description{
NBA player statistics 2009-2010 (regular season) from dougstats.com. Screen scraped by \code{scraper/dougstats.com/player-statistics.R} on 2011-08-12.
}
\usage{data(NbaPlayerStatistics.20092010)}
\format{
  A data frame with 9261 observations on the following 5 variables.
  \describe{
    \item{\code{Name}}{a factor with 441 levels}
    \item{\code{Team}}{a factor with 31 levels}
    \item{\code{Role}}{a factor with 6 levels}
    \item{\code{Skill}}{a factor with 21 levels}
    \item{\code{Rating}}{a numeric vector}
  }
}
\source{http://dougstats.com}
\examples{
data(NbaPlayerStatistics.20092010)
## maybe str(NbaPlayerStatistics.20092010)
}
\keyword{datasets}
