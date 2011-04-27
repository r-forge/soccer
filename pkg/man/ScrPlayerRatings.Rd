\name{ScrPlayerRatings}
\alias{ScrPlayerRatings}
\docType{data}
\title{Soccer player ratings from goal.com}
\description{
Soccer player ratings from goal.com. Screen scraped by \code{scraper/goal.com/player-ratings.R} on 2011-04-27.
}
\usage{data(ScrPlayerRatings)}
\format{
  A data frame with 21835 observations on the following 6 variables.
  \describe{
    \item{\code{team}}{a factor with 92 levels}
    \item{\code{competition}}{a factor with 4 levels}
    \item{\code{name}}{a factor with 2135 levels}
    \item{\code{role}}{a factor with 5 levels}
    \item{\code{skill}}{a factor with 37 levels}
    \item{\code{rating}}{a numeric vector}
  }
}
\source{http://www.goal.com}
\examples{
data(ScrPlayerRatings)
## maybe str(ScrPlayerRatings)
}
\keyword{datasets}
