
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
                        unname(xmlAttrs(x[[13]][["a"]])["href"])
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


trace(fetch_matchday_games, quote(cat(season, day, "\n")), at = 1)
sgames <- fetch_seasons()
untrace(fetch_matchday_games)


saveRDS(sgames, file = "sgames.Rds")




### Clean data: ######################################################

clean_player_name <- function(x) {
  str_trim(str_replace(x, "\\(.*\\)", ""))
}





### Save data: #######################################################





