### Screen scraper for basketball player statistics from dougstats.com
### (2011-04-27).
### pkg/data/NbaPlayerStatistics.20092010.RData

source("../scraperbase.R")
library("reshape2")



### Fetch file: ######################################################

raw <- read.table("http://dougstats.com/09-10RD.txt", header = TRUE)
str(raw)



### Clean: ###########################################################

NbaPlayerStatistics.20092010 <- melt(raw)
names(NbaPlayerStatistics.20092010) <- c("Name", "Team", "Role", "Skill", "Rating")

str(NbaPlayerStatistics.20092010)


### Team names capitalized:

levels(NbaPlayerStatistics.20092010$Team) <-
  toupper(levels(NbaPlayerStatistics.20092010$Team))


### Player names normalized, i.e., Forename Surname:

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
                     {s <- substring(s,2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


tmp <- strsplit(levels(NbaPlayerStatistics.20092010$Name), ",")
tmp <- sapply(lapply(tmp, capwords),
              function(x) paste(rev(x), collapse = " "))
levels(NbaPlayerStatistics.20092010$Name) <- tmp



### Save and document: ###############################################

saveit(NbaPlayerStatistics.20092010)

documentit(NbaPlayerStatistics.20092010,
           title = "NBA player statistics 2009-2010 (regular season) from dougstats.com",
           source = "http://dougstats.com",
           date = as.character(Sys.Date()),
           scraper = "dougstats.com/player-statistics.R")



