
library("XML")
library("stringr")

options(stringsAsFactors = FALSE)


fetch_player <- function(url) {
  nice_names <- function(x) {
    x <- str_replace_all(x, " ", "")
    x <- str_replace_all(x, "/", "")
    x
  }

  raw <- readLines(url)
  raw <- str_replace_all(raw, "&nbsp;", " ")

  site <- htmlTreeParse(raw, useInternalNodes = TRUE)
  on.exit(free(site))

  info <- getNodeSet(site, "//div[@id='info']")[[1]]
  info <- xmlValue(info)
  info <- str_split(info, "\\n")[[1]]
  info <- str_split(info, ":")
  info <- lapply(info, str_trim)

  info <- info[sapply(info, length) == 2]

  mat <- data.frame(lapply(info, "[", 2))
  names(mat) <- nice_names(sapply(info, "[", 1))

  mat$Height <- str_replace(mat$Height, " cm", "")
  mat$Weight <- str_replace(mat$Weight, " kg", "")

  age <- mat$Age
  if ( str_detect(age, "\\d+ \\(\\d+/\\d+/\\d+\\)") ) {
    age <- str_split(age, " ")[[1]]
  } else {
    age <- c(NA, NA)
  }

  mat$Age <- age[1]
  mat$Birthday <- age[2]

  #pos <- iconv(mat$Positions, to = "latin1")
  #pos <- str_split(pos, ", ")[[1]]
  #pos <- str_replace(pos[str_detect(pos, "\342~.")], "\342~.", "")

  #mat$Positions <- NULL
  #mat$Position <- pos

  cat(mat$Club, mat$Name, mat$Age, "\n")

  mat
}


fetch_club_players <- function(url) {
  raw <- readLines(url)
  raw <- str_replace_all(raw, "&nbsp;", " ")

  site <- htmlTreeParse(raw, useInternalNodes = TRUE)
  on.exit(free(site))

  table <- getNodeSet(site, "//table[@class='tb_psd'][2]/tr")

  ## Team:
  team <- xmlValue(table[[1]])

  ## Players:
  table <- table[-c(1, 2)]

  trfun <- function(x) {
    class <- xmlAttrs(x[[1]])["class"]

    if ( !is.na(class) & class == "title_pos" ) {
      position <<- xmlValue(x[[1]])
      position <<- str_sub(position, end = str_length(position) - 1)
      return(NULL)
    }

    a <- x[[1]][[1]][[2]]

    player <- xmlValue(a)
    player <- str_replace(player, "\\d+ - ", "")

    href <- unname(xmlAttrs(a)["href"])

    c(Team = team,
      Player = player,
      Site = href,
      Position = position)
  }

  position <- NULL
  table <- lapply(table, trfun)

  as.data.frame(do.call(rbind, table))
}


fetch_championship_teams <- function(url) {
  raw <- readLines(url)
  raw <- str_replace_all(raw, "&nbsp;", " ")

  site <- htmlTreeParse(raw, useInternalNodes = TRUE)
  on.exit(free(site))

  table <- getNodeSet(site, "//table[@class='tb_psd']/tr/td/a/..")

  ## Teams:
  table <- table[-1]

  trfun <- function(x) {
    c(Team = xmlValue(x[[1]]),
      Site = unname(xmlAttrs(x[[1]])["href"]))
  }

  table <- lapply(table, trfun)
  as.data.frame(do.call(rbind, table))
}



#player_url <- "http://pesstatsdatabase.com/PSD/Player.php?Id=1593&Club=69"
#club_players_url <- "http://pesstatsdatabase.com/PSD/Players.php?Club=69"
#championship_teams_url <- "http://pesstatsdatabase.com/PSD/Club.php?Championship=4"
#
#fetch_player(player_url)
#fetch_club_players(club_players_url)
#fetch_championship_teams(championship_teams_url)



fetch_championship_players <- function(url) {
  base_url <- "http://pesstatsdatabase.com/PSD/"

  ## Teams:
  teams <- fetch_championship_teams(url)
  teams$Site <- sprintf("%s%s", base_url, teams$Site)

  ## Players:
  players <- lapply(teams$Site, fetch_club_players)
  players <- do.call(rbind, players)
  players$Site <- sprintf("%s%s", base_url, players$Site)

  ## Player details:
  details <- lapply(players$Site, fetch_player)
  details <- do.call(rbind, details)
  details$Position <- players$Position

  details
}


fetch_all <- function(championships = c(Bundesliga = 4, PremierLeague = 1, LaLiga = 2, SerieA = 3)) {
  details <- list()

  for ( ch in names(championships) ) {
    print(ch)
    url <- sprintf("http://pesstatsdatabase.com/PSD/Club.php?Championship=%s", championships[ch])

    details[[ch]] <- fetch_championship_players(url)
    details[[ch]]$Championship <- ch
    save(details, file = sprintf("details-%s.RData", ch))
  }

  #details <- do.call(rbind, details)

  details
}

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
                     {s <- substring(s,2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


data <- fetch_all()
data <- do.call(rbind, data)

save(data, file = "uncleaned-data.RData")

data_names <- names(data)
data_names_numeric <- setdiff(data_names,
                              c("ShirtName", "Name", "Nationality",
                                "Club", "Championship", "Position",
                                "Foot", "Side", "InjuryTolerance",
                                "Positions", "Birthday"))

data <- within(data, {
  ShirtName <- NULL

  Name <- capwords(Name, strict = TRUE)
  Nationality <- factor(Nationality)

  Club <- factor(Club)
  Championship <- factor(Championship)

  Position <- capwords(Position, strict = TRUE)
  Position <- ordered(Position,
                      levels = c("Goalkeeper", "Defender", "Midfielder", "Forward"))

  Foot <- factor(Foot)
  Side <- factor(Side)

  InjuryTolerance[InjuryTolerance == ""] <- NA
  InjuryTolerance <- ordered(InjuryTolerance, levels = c("A", "B", "C", NA))
})

for ( var in data_names_numeric )
  data[[var]] <- as.numeric(data[[var]])

soccer_psd_20110915 <- data
save(soccer_psd_20110915, file = "../../../data/soccer_psd_20110915.RData")


