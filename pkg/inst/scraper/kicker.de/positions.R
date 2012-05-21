
library("XML")
library("stringr")

options(stringsAsFactors = FALSE)


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

  cbind(name = name, as.data.frame(t(pos_value)))
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

  ret <- list(teams[1], game_details_team(details[[2]]),
              teams[2], game_details_team(details[[4]]))

  ret
}


game_details_team <- function(team, details) {
  aufs <- getNodeSet(details, ".//div[@class='aufstellung_team']")[[1]]

  ## Einwechslungen:
  wechsel <- getNodeSet(aufs,
                        ".//div[@id='ctl00_PlaceHolderHalf_aufstellunghalf_einwechslungenHeim']")[[1]]
  wechsel <- wechsel[["div"]][-1]
  nwechsel <- length(wechsel) / 5
  for ( i in seq(length(wechsel) / 5) ) {

  }
  
  
  
  ## Reservebank:
  bank <- getNodeSet(aufs,
                     ".//div[@id='ctl00_PlaceHolderHalf_aufstellunghalf_reservebankHeim']")[[1]]
  bank <- sapply(bank[["div"]]["a"], xmlValue)
  bank <- unname(bank)
  
  
  ## Trainer:
  trainer <- getNodeSet(aufs,
                        ".//div[@id='ctl00_PlaceHolderHalf_aufstellunghalf_trainerHeim']")[[1]]
  trainer <- xmlValue(trainer[["div"]][["a"]])
  
}




fetch_game <- function(url) {
  raw <- readLines(url)
  raw <- str_replace_all(raw, "&nbsp;", " ")

  site <- htmlTreeParse(raw, useInternalNodes = TRUE)

  ret <- list()
  ret$positions <- game_positions(site)
  ret$result <- game_result(site)
  ret$details <- game_details(site)
  
  free(site)

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


fetch_matchday_games <- function(season, day) {
  urls <- fetch_matchday_games_urls(season, day)
  games <- lapply(urls, fetch_game)
  games
}


fetch_season_games <- function(season) {
  lapply(1:34, function(d) fetch_matchday_games(season, d))
}


fetch_seasons <- function() {

}


#s <- fetch_season_games("2011-12")
