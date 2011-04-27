### Screen scraper for soccer player ratings from goal.com (2011-04-27).
### pkg/data/ScrPlayerRatings.RData

source("../scraperbase.R")
library("plyr")


BASE_URL <- "http://www.goal.com"



### Player rating: ###################################################

fetch_playerrating <- function(url) {
  rawSite <- readLines(url)
  site <- htmlTreeParse(rawSite, useInternalNodes = TRUE)

  table <- getNodeSet(site, "//div[@id='playerAbilities']/table")[[1]]

  if ( is.null(table) )
    return(NULL)

  ratings <- xmlApply(table,
                      function(x) {
                        r <- as.numeric(xmlValue(x[[3]][[2]]))
                        names(r) <- xmlValue(x[[1]][[2]])
                        r
                      })

  free(site)

  ret <- unlist(unname(ratings))
  ret <- data.frame(skill = names(ret),
                    rating = unname(ret),
                    stringsAsFactors = FALSE)
}

# url <- "http://www.goal.com/en/people/argentina/8012/lionel-messi/"
# ratings <- fetch_playerrating(url)



### Team squad: ######################################################

fetch_teamsquad <- function(url) {
  rawSite <- readLines(url)
  site <- htmlTreeParse(rawSite, useInternalNodes = TRUE)

  table <- getNodeSet(site, "//div[@id='teamSquadModule']/div/table")[[1]]

  squad <- xmlApply(table[[1]],
                    function(x) {
                      ret <- c(number = xmlValue(x[[1]]),
                               name = xmlValue(x[[3]]),
                               role = xmlValue(x[[5]]),
                               url = xmlAttrs(x[[3]][[1]])[[1]])

                      if ( !is.null(xmlAttrs(x[[9]])) ) {
                        ret <- rbind(ret,
                                     c(number = xmlValue(x[[9]]),
                                       name = xmlValue(x[[11]]),
                                       role = xmlValue(x[[13]]),
                                       url = xmlAttrs(x[[11]][[1]])[[1]]))
                      }

                      ret
                    })

  free(site)

  squad <- do.call(rbind, unname(squad))

  squad
}

# url <- "http://www.goal.com/en/teams/spain/125/barcelona"
# squad <- fetch_teamsquad(url)



fetch_teamratings <- function(url) {
  squad <- fetch_teamsquad(url)
  teamratings <- list()

  for ( i in 1:nrow(squad) ) {
    logger(sprintf("\t%s", squad[i, 2]))

    purl <- sprintf("%s%s", BASE_URL, squad[i, 4])
    ratings <- fetch_playerrating(purl)

    if ( !is.null(ratings) ) {
      player <- data.frame(name = rep(unname(squad[i, 2]), nrow(ratings)),
                           role = rep(unname(squad[i, 3]), nrow(ratings)),
                           stringsAsFactors = FALSE)

      teamratings[[i]] <- cbind(player, ratings)
    }
  }

  do.call(rbind, teamratings)
}



### Teams: ###########################################################

fetch_compteams <- function(url) {
  rawSite <- readLines(url)
  site <- htmlTreeParse(rawSite, useInternalNodes = TRUE)

  teams <- getNodeSet(site, "//td[@class='fcName']",
                      fun = function(x) {
                        c(team = unname(xmlValue(x)),
                          url = unname(xmlAttrs(x[[1]])[1]))
                    })

  free(site)

  teams <- do.call(rbind, teams)

  teams
}

# url <- "http://www.goal.com/en/scores/result-standings/47/1-bundesliga"
# teams <- fetch_teams(url)



fetch_compratings <- function(url, competition = NA) {
  teams <- fetch_compteams(url)
  compratings <- list()

  for ( i in 1:nrow(teams) ) {
    logger(sprintf("Team = %s", teams[i, 1]))

    turl <- sprintf("%s%s", BASE_URL, teams[i, 2])
    ratings <- fetch_teamratings(turl)

    if ( !is.null(ratings) ) {
      team <- data.frame(team = rep(teams[i, 1], nrow(ratings)),
                         competition = rep(competition, nrow(ratings)),
                         stringsAsFactors = FALSE)

      compratings[[i]] <- cbind(team, ratings)
    }
  }

  do.call(rbind, compratings)
}



######################################################################

fetch_all <- function(competitions) {
  for ( comp in names(competitions) ) {
    ratings <- fetch_compratings(competitions[comp], comp)
    save(ratings, file = sprintf("ratings-%s.RData", comp))
  }

  allratings <- list()
  for ( comp in names(competitions) ) {
    load(sprintf("ratings-%s.RData", comp))
    allratings[[comp]] <- ratings
  }

  ratings <- do.call(rbind, allratings)

  ratings <- within(ratings, {
    team <- factor(team)
    name <- factor(name)
    competition <- factor(competition)
    role <- factor(role)
    skill <- factor(skill)
  })

  save(ratings, file = "ratings.RData")
}



### Do it:

competitions <- c(PremierLeague = "http://www.goal.com/en/scores/result-standings/64/premier-league",
                  Bundesliga = "http://www.goal.com/en/scores/result-standings/47/1-bundesliga",
                  LaLiga = "http://www.goal.com/en/scores/result-standings/61/la-liga",
                  Ligue1 = "http://www.goal.com/en/scores/result-standings/60/ligue-1",
                  SerieA = "http://www.goal.com/en/scores/result-standings/69/serie-a")

fetch_all(competitions)



### Clean: ###########################################################

load("ratings.RData")


### Ligue1 has not enough ratings:
ratings <- subset(ratings, competition != "Ligue1")
ratings$competition <- ratings$competition[, drop = TRUE]


### Order roles:
ratings$role <- factor(ratings$role, levels = levels(ratings$role)[c(2, 1, 4, 5, 3)])


### Within a role, there are some "lonely" skill ratings:
subset(ratings, role == "Goalkeeper" & skill == "Diving")


## Number of players within roles:
nr <- table(count(ratings, c("name", "role"))$role)
nr


## Skill ratings per role:
nsr <- ddply(ratings, "role", function(x) table(x$skill))
nsr


## Needed players within roles to define a skill requirement:
NR <- c("Goalkeeper" = 237,
        "Defender" = 645,
        "Midfielder" = 700,
        "Striker" = 400,
        "Manager/Coach" = 49)


## Compute required skills:
required_skills <- list()
for ( i in 1:5 ) {
  w <- nsr[i, -1]
  n <- names(NR)[i]

  required_skills[[n]] <- names(w)[w >= NR[i]]
}

required_skills


## Clean data set:
clean_ratings <- function(clean_role, req_skills) {
  data <- subset(ratings, role == clean_role)

  ## Players which have at minimum the required skills rated:
  w <- dlply(data, "name",
             function(x) {
               length(x$skill) >= length(req_skills) &
               all(x$skill %in% req_skills)
             })
  w <- names(which(unlist(w)))

  data <- subset(data, name %in% w)

  ## Remove skills not in the required skills:
  data <- subset(data, skill %in% req_skills)

  ## Remove duplicates:
  w <- dlply(data, "name",
             function(x) {
               length(x$skill) <= length(req_skills)
             })
  w <- names(which(unlist(w)))

  data <- subset(data, name %in% w)

  data
}

ScrPlayerRatings <- lapply(names(NR),
                           function(x) {
                             clean_ratings(x, required_skills[[x]])
                           })
ScrPlayerRatings <- do.call(rbind, ScrPlayerRatings)



### Save and document: ###############################################

saveit(ScrPlayerRatings)

documentit(ScrPlayerRatings,
           title = "Soccer player ratings from goal.com",
           source = "http://www.goal.com",
           date = as.character(Sys.Date()),
           scraper = "goal.com/player-ratings.R")

