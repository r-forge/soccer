
library("XML")
library("stringr")

options(stringsAsFactors = FALSE)



### Fetch data: ######################################################

fetch_season <- function(season) {
  url <- "http://www.xs4all.nl/~kassiesa/bert/uefa/data/method"
  url <- local({
      if ( season >= 2009 )
        method <- 4
      if ( season >= 2004 && season <= 2008 )
        method <- 3
      if ( season >= 1999 && season <= 2003 )
        method <- 2
      if ( season <= 1998 )
        method <- 1

      sprintf('%s%s/tcoef%s.html', url, method, season)
    })

  raw <- readLines(url, encoding = "latin-1")
  raw <- str_replace_all(raw, "&nbsp;", " ")

  site <- htmlTreeParse(raw, useInternalNodes = TRUE)
  table <- coef_table(site)
  free(site)

  table <- cbind(Season = sprintf("%s-%s", season-1, season),
                 table)

  table
}


coef_table <- function(site) {
  table <- getNodeSet(site, "/html/body/div/div/table[3]//tr",
                      fun = function(x) {
                        if ( xmlName(x[[1]]) == "td" ) {
                          c(xmlValue(x["td"][[2]]),   # Team
                            xmlValue(x["td"][[3]]),   # Country
                            xmlValue(x["td"][[10]]))  # Coef
                        } else {
                          NULL
                        }
                      })
  table <- do.call(rbind, table)
  table <- as.data.frame(table)

  table[, 1] <- str_trim(table[, 1])
  table[, 3] <- as.numeric(table[, 3])

  colnames(table) <- c("Team", "Country", "Coefficient")

  table
}


fetch_seasons <- function(seasons = 2002:2012) {
  do.call(rbind, lapply(seasons, fetch_season))
}


if ( FALSE ) {
  trace(fetch_season, quote(cat(season, "\n")), at = 1)
  coefs <- fetch_seasons()
  untrace(fetch_season)

  saveRDS(coefs, file = "uefacoefs.Rds")
}

coefs <- readRDS("uefacoefs.Rds")



### Clean data: ######################################################

clean_team <- function(x) {
  y <- factor(x)

  ## Clean to match the Lineup data:
  l <- levels(y)
  l[agrep("1.FC Kaiserslautern", l)] <- "1. FC Kaiserslautern"
  l[agrep("Bayern München", l)] <- "Bayern Muenchen"
  l[agrep("1.FC Nürnberg", l)] <- "1. FC Nuernberg"
  l[agrep("FSV Mainz 05", l)] <- "1. FSV Mainz 05"
  l[agrep("Schalke 04", l)] <- "FC Schalke 04"
  levels(y) <- l
  y
}


coefs <- subset(coefs, Country == "Ger")
coefs <- within(coefs, {
  League <- factor("Bundesliga")
  Season <- ordered(Season)
  Team <- clean_team(Team)
  Country <- NULL
})

coefs <- coefs[, c("League", "Season", "Team", "Coefficient")]



### Save data: #######################################################

BundesligaUEFACoefficients <- coefs

load("../../../data/BundesligaLineups.RData")

save(BundesligaMatches, BundesligaLineups,
     BundesligaSubstitutes, BundesligaExchanges,
     BundesligaTrainer, BundesligaUEFACoefficients,
     file = "../../../data/BundesligaLineups.RData")


