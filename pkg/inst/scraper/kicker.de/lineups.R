
library("XML")
library("stringr")

options(stringsAsFactors = FALSE)



### Fetch data: ######################################################

player_position <- function(x) {
  name <- xmlValue(x[["a"]])

  pos <- xmlAttrs(x)["style"]
  pos <- str_split(pos, "; ")[[1]]
  pos <- pos[str_detect(pos, "[left][top]")]
  pos <- str_split(pos, ":")

  pos_value <- sapply(pos, "[[", 2)
  pos_value <- sapply(pos_value, str_replace, "px", "")
  pos_value <- as.numeric(pos_value)
  names(pos_value) <- sapply(pos, "[[", 1)
  names(pos_value) <- paste("Pos",
                            toupper(substring(names(pos_value), 1, 1)),
                            substring(names(pos_value), 2), sep = "")

  cbind(Player = name, as.data.frame(t(pos_value)))
}


game_positions <- function(site) {
  players <- getNodeSet(site, "//table[@class='taktischeaufstellung']/tr/td/div",
                        fun = player_position)
  players <- do.call(rbind, players)
  players
}


game_result <- function(site) {
  info <- getNodeSet(site, "//div[@id='ltmatch']/table")[[1]]

  ## Teams:
  teams <- getNodeSet(info, "//td[@class='lttabver']",
                      fun = function(x) {
                        xmlValue(x[["h1"]][["a"]])
                      })

  ## Result:
  result <- getNodeSet(info, "//td[@class='lttaberg']",
                       fun = function(x) {
                         xmlValue(x[["h1"]])
                       })[[1]]
  result <- str_split(result, " ")[[1]]
  result1 <- str_sub(result[2], start = 2, end = -2)
  result1 <- str_split(result1, ":")[[1]]
  result2 <- str_split(result[1], ":")[[1]]

  ## Date:
  date <- getNodeSet(info, "//div[@class='ltmatdac']",
                     fun = function(x) {
                       xmlValue(x[["a"]])
                     })[[1]]
  date <- str_split(date, ", ")[[1]]
  date[3] <- str_replace(date[3], ". Spieltag", "")

  ## Return:
  data.frame(League = date[1], Season = date[2], Round = date[3],
             TeamHome = teams[[1]], TeamGuest = teams[[2]],
             GoalsHome1 = result1[1], GoalsGuest1 = result1[2],
             GoalsHome2 = result2[1], GoalsGuest2 = result2[2])
}


game_details <- function(site) {
  details <- getNodeSet(site, "//table[@summary='Vereinsliste']")
  details <- details[[1]]["tr"]

  teams <- unname(sapply(details[c(1, 3)], xmlValue))
  teams <- str_trim(teams)

  details <- list(game_details_team(teams[1], details[[2]], "Heim"),
                  game_details_team(teams[2], details[[4]], "Ausw"))

  ret <- list()
  ret$substitutes <- rbind(details[[1]]$bank, details[[2]]$bank)
  ret$exchanges <-  rbind(details[[1]]$wechsel, details[[2]]$wechsel)
  ret$trainer <- rbind(details[[1]]$trainer, details[[2]]$trainer)

  ret
}


game_details_team <- function(team, details, which) {
  aufs <- getNodeSet(details, ".//div[@class='aufstellung_team']")[[1]]

  ## Aufstellung:
  p <- sprintf(".//div[@id='ctl00_PlaceHolderHalf_aufstellunghalf_ausstellung%s']", which)
  aufstellung <- getNodeSet(aufs, p)[[1]]
  aufstellung <- getNodeSet(aufstellung, ".//div[@class='spielerdiv']",
                            fun = function(x) {
                              xmlValue(x[["a"]])
                            })
  aufstellung <- data.frame(Team = team, Player = aufstellung)

  ## Einwechslungen:
  p <- sprintf(".//div[@id='ctl00_PlaceHolderHalf_aufstellunghalf_einwechslungen%s']", which)
  wechsel <- getNodeSet(aufs, p)
  if ( length(wechsel) == 0 ) {
    wechsel <- data.frame(Team = character(0),
                          Minute = character(0),
                          PlayerOn = character(0),
                          PlayerOff = character(0))
  } else {
    wechsel <- wechsel[[1]][["div"]][-1]
    wechsel <- split(wechsel, cumsum(names(wechsel) == "br"))
    wechsel <- t(sapply(wechsel,
                        function(x) {
                          y <- c(str_trim(xmlValue(x[[2]])),
                                 sapply(x[names(x) == "a"], xmlValue))
                          names(y) <- c("Minute", "PlayerOn", "PlayerOff")
                          y
                        }))
    wechsel <- data.frame(Team = team, wechsel)
  }

  ## Reservebank:
  p <- sprintf(".//div[@id='ctl00_PlaceHolderHalf_aufstellunghalf_reservebank%s']", which)
  bank <- getNodeSet(aufs, p)[[1]]
  bank <- sapply(bank[["div"]]["a"], xmlValue)
  bank <- unname(bank)
  bank <- data.frame(Team = team,
                     Player = c(bank, wechsel$PlayerOn))

  ## Trainer:
  p <- sprintf(".//div[@id='ctl00_PlaceHolderHalf_aufstellunghalf_trainer%s']", which)
  trainer <- getNodeSet(aufs, p)[[1]]
  trainer <- xmlValue(trainer[["div"]][["a"]])
  trainer <- data.frame(Team = team, Trainer = trainer)


  list(wechsel = wechsel,
       bank = bank,
       trainer = trainer)
}



fetch_game <- function(url) {
  raw <- readLines(url, encoding = "ISO-8859-1")
  raw <- str_replace_all(raw, "&nbsp;", " ")

  site <- htmlTreeParse(raw, useInternalNodes = TRUE)

  game <- list()
  game$positions <- game_positions(site)
  game$result <- game_result(site)
  game$details <- game_details(site)


  free(site)

  date <- data.frame(League = game$result$League,
                     Season = game$result$Season,
                     Round = game$result$Round)

  game$positions <- cbind(Team = c(rep(game$result$TeamHome, 11),
                                  rep(game$result$TeamGuest, 11)),
                         game$positions)

  ret <- list()
  ret$match <- game$result
  ret$lineup <- cbind(date, game$positions)
  ret$substitutes <- cbind(date, game$details$substitutes)
  ret$exchanges <- cbind(date, game$details$exchanges)
  ret$trainer <- cbind(date, game$details$trainer)

  ret
}


fetch_matchday_games_urls <- function(season, day) {
  url <- "http://www.kicker.de/news/fussball/bundesliga/spieltag/1-bundesliga/%s/%s/0/spieltag.html"
  url <- sprintf(url, season, day)

  raw <- readLines(url)
  raw <- str_replace_all(raw, "&nbsp;", " ")

  site <- htmlTreeParse(raw, useInternalNodes = TRUE)

  games <- getNodeSet(site, "//table[@summary='Begegnungen']/tr[starts-with(@class, 'fest')]",
                      fun = function(x) {
                        if ( !is.null(x[[13]][["a"]]) ) {
                          unname(xmlAttrs(x[[13]][["a"]])["href"])
                        }
                        else {
                          NULL
                        }
                      })

  free(site)

  games <- unlist(games)
  games <- str_replace(games, "spielanalyse", "taktische-austellung")
  games <- sprintf("http://www.kicker.de%s", games)

  games
}


merge_parts <- function(x) {
  ret <- list()

  ret$match <- do.call(rbind, lapply(x, "[[", "match"))
  ret$lineup <- do.call(rbind, lapply(x, "[[", "lineup"))
  ret$substitutes <- do.call(rbind, lapply(x, "[[", "substitutes"))
  ret$exchanges <- do.call(rbind, lapply(x, "[[", "exchanges"))
  ret$trainer <- do.call(rbind, lapply(x, "[[", "trainer"))

  ret
}


fetch_matchday_games <- function(season, day) {
  urls <- fetch_matchday_games_urls(season, day)
  games <- lapply(urls, fetch_game)
  merge_parts(games)
}


fetch_season_games <- function(season) {
  games <- lapply(1:34, function(d) fetch_matchday_games(season, d))
  games <- merge_parts(games)

  saveRDS(games, file = sprintf("games-%s.Rds", season))

  games
}


fetch_seasons <- function(seasons = sprintf("20%02d-%02d", 1:11, 2:12)) {
  games <- lapply(seasons, fetch_season_games)
  merge_parts(games)
}



if ( FALSE ) {
  trace(fetch_matchday_games, quote(cat(season, day, "\n")), at = 1)
  sgames <- fetch_seasons()
  untrace(fetch_matchday_games)

  saveRDS(sgames, file = "sgames.Rds")
}

sgames <- readRDS("sgames.Rds")



### Clean data: ######################################################

clean_player <- function(x) {
  y <- sapply(x, function(y) str_trim(str_replace(y, "\\(.*\\)", "")))
  factor(unname(y))
}

clean_league <- function(x) {
  factor(rep("Bundesliga", length(x)))
}

clean_season <- function(x) {
  y <- strsplit(x, "/")
  y2 <- sprintf("20%s", sapply(y, "[[", 2))
  ordered(sprintf("%s-%s", sapply(y, "[[", 1), y2))
}

clean_round <- function(x) {
  as.numeric(x)
}

clean_team <- function(x) {
  y <- factor(x)

  l <- levels(y)
  l[agrep("1. FC Köln", l)] <- "1. FC Koeln"
  l[agrep("1. FC Nürnberg", l)] <- "1. FC Nuernberg"
  l[agrep("1860 München", l)] <- "1860 Muenchen"
  l[agrep("Bayern München", l)] <- "Bayern Muenchen"
  levels(y) <- l

  y
}

clean_goals <- function(x) {
  as.numeric(x)
}

clean_trainer <- function(x) {
  factor(x)
}

clean_posleft <- function(x) {
  ## Field width: 578 (scaled to 510)
  ## Image width: 44

  (x + (44 / 2)) / 578
}

clean_postop <- function(x) {
  ## Field height: 1050
  ## Image height: 75

  midline <- 1050 / 2

  x[x < midline] <- (x[x < midline] + (75 / 2)) / midline
  x[x > midline] <- (1050 - (x[x > midline] + (75 / 2))) / midline


  ## Correction of the differences between home and guest.
  ## Check, e.g.,
  ##   ggplot(BundesligaLineups, aes(PosX, PosY)) +
  ##     geom_point() + facet_wrap(~ Season) +
  ##     xlim(0, 1) + ylim(0, 1)
  ## to see that there in fact eleven PosY values. Therefore,
  ## we correct the data by using the mean value for each of
  ## the eleven positions. Note that the goalkeeper is already
  ## correct.
  xval <- sort(unique(x))

  xval_mat <- matrix(xval[-1], ncol = 2, byrow = TRUE)
  xval_new <- c(xval[1], apply(xval_mat, 1, mean))
  xval_mat <- rbind(c(0, xval[1]), xval_mat)

  xintval <- sapply(x, function(y) which(apply(xval_mat, 1, function(z) y %in% z)))

  xval_new[xintval]
}



sgames$match <- within(sgames$match, {
  League <- clean_league(League)
  Season <- clean_season(Season)
  Round <- clean_round(Round)
  TeamHome <- clean_team(TeamHome)
  TeamGuest <- clean_team(TeamGuest)
  GoalsHome1 <- clean_goals(GoalsHome1)
  GoalsHome2 <- clean_goals(GoalsHome2)
  GoalsGuest1 <- clean_goals(GoalsGuest1)
  GoalsGuest2 <- clean_goals(GoalsGuest2)
})


sgames$lineup <- within(sgames$lineup, {
  League <- clean_league(League)
  Season <- clean_season(Season)
  Round <- clean_round(Round)
  Team <- clean_team(Team)
  Player <- clean_player(Player)
  PosX <- clean_posleft(PosLeft)
  PosY <- clean_postop(PosTop)
})


sgames$substitutes <- within(sgames$substitutes, {
  League <- clean_league(League)
  Season <- clean_season(Season)
  Round <- clean_round(Round)
  Team <- clean_team(Team)
  Player <- clean_player(Player)
})


sgames$exchanges <- within(sgames$exchanges, {
  League <- clean_league(League)
  Season <- clean_season(Season)
  Round <- clean_round(Round)
  Team <- clean_team(Team)
  PlayerOn <- clean_player(PlayerOn)
  PlayerOff <- clean_player(PlayerOff)
  # TODO: Minute
})


sgames$trainer <- within(sgames$trainer, {
  League <- clean_league(League)
  Season <- clean_season(Season)
  Round <- clean_round(Round)
  Team <- clean_team(Team)
  Trainer <- clean_trainer(Trainer)
})



### Save data: #######################################################

BundesligaMatches <- sgames$match
BundesligaLineups <- sgames$lineup
BundesligaSubstitutes <- sgames$substitutes
BundesligaExchanges <- sgames$exchanges
BundesligaTrainer <- sgames$trainer

save(BundesligaMatches, BundesligaLineups,
     BundesligaSubstitutes, BundesligaExchanges,
     BundesligaTrainer, file = "../../../data/BundesligaLineups.RData")
