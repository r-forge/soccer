\name{NbaPlayerRatings.20102011}
\alias{NbaPlayerRatings.20102011}
\docType{data}
\title{NBA player ratings 2010-2011 (regular season) from BasketballValue.com}
\description{
NBA player ratings 2010-2011 (regular season) from BasketballValue.com. Screen scraped by \code{scraper/BasketballValue.com/player-statistics.R} on 2011-04-27.
}
\usage{data(NbaPlayerRatings.20102011)}
\format{
  A data frame with 7632 observations on the following 4 variables.
  \describe{
    \item{\code{name}}{a factor with 212 levels}
    \item{\code{team}}{a factor with 16 levels}
    \item{\code{skill}}{a factor with 36 levels}
    \item{\code{rating}}{a numeric vector}
  }
}
\source{http://BasketballValue.com}
\examples{
data(NbaPlayerRatings.20102011)
## maybe str(NbaPlayerRatings.20102011)
}
\keyword{datasets}
