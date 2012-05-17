
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

}


game <- function() {
  url <- "http://www.kicker.de/news/fussball/bundesliga/spieltag/1-bundesliga/2011-12/34/1143587/taktische-austellung_fc-augsburg-91_hamburger-sv-12.html"

  raw <- readLines(url)
  raw <- str_replace_all(raw, "&nbsp;", " ")

  site <- htmlTreeParse(raw, useInternalNodes = TRUE)

  ret <- list()
  ret$positions <- game_positions(site)
  ret$info <- game_result(site)

  free(site)

  ret
}


g <- game()
plot(top ~ left, data = g$positions, col = c(rep(1, 11), rep(2, 11)), pch = 19, cex = 3)


