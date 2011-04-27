### Screen scraper for soccer player ratings from goal.com (2011-04-27).
### pkg/data/NbaPlayerRatings.20102011.RData

source("../scraperbase.R")
library("reshape2")


### Fetch file: ######################################################

### Download and unzip file from http://basketballvalue.com/downloads.php:
file <- "playerstats201104262349.txt"

raw <- read.table(file, header = TRUE, sep = "\t", na.strings = "NULL")
str(raw)



### Clean: ###########################################################

raw$PlayerID <- NULL
NbaPlayerRatings.20102011 <- melt(raw)

names(NbaPlayerRatings.20102011) <- c("name", "team", "skill", "rating")

str(NbaPlayerRatings.20102011)



### Save and document: ###############################################

saveit(NbaPlayerRatings.20102011)

documentit(NbaPlayerRatings.20102011,
           title = "NBA player ratings 2010-2011 (regular season) from BasketballValue.com",
           source = "http://BasketballValue.com",
           date = as.character(Sys.Date()),
           scraper = "BasketballValue.com/player-statistics.R")



