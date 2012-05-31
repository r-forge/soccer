
library("ggplot2")
library("plyr")

load("../../../data/BundesligaLineups.RData")

str(BundesligaMatches)
str(BundesligaLineups)
str(BundesligaSubstitutes)
str(BundesligaExchanges)
str(BundesligaTrainer)



######################################################################

## Borussia Dortmund matches in 2011-2012:
bd_matches <- subset(BundesligaMatches,
                     Season == "2011-2012" &
                     (TeamHome == "Borussia Dortmund" |
                     TeamGuest == "Borussia Dortmund"))


## Borussia Dortmund lineups in 2011-2012:
bd_lineups <- subset(BundesligaLineups,
                     Season == "2011-2012" &
                     Team == "Hertha BSC")
                     #select = -c(PosLeft, PosTop))


ggplot(bd_lineups, aes(PosX, PosY)) +
  geom_point() +
  facet_wrap(~ Round)



######################################################################

ggplot(BundesligaLineups, aes(PosX, PosY)) +
  geom_point() + facet_wrap(~ Season) + xlim(0, 1) + ylim(0, 1)



######################################################################

x <- subset(BundesligaLineups, Season == "2002-2003" &
            PosX < 0.2 & PosY > 0.9)

ab <- subset(BundesligaLineups, Season == "2002-2003" & Round == 7 &
             Team == "Arminia Bielefeld")
ggplot(ab, aes(PosX, PosY)) + geom_point() + xlim(0, 1) + ylim(0, 1)


######################################################################

x <- subset(BundesligaLineups, Season == "2001-2002" & Round == 1 & PosTop == 879)
x <- ddply(x, .(Team),
           function(x) {
             x[which.min(x$PosLeft), ]
           })


hr <- subset(BundesligaLineups,
             Season == "2001-2002" & Round == 1 &
             Team == "Hansa Rostock")
ggplot(hr, aes(PosX, PosY)) + geom_point() + xlim(0, 1) + ylim(0, 1)



######################################################################

sort(unique(BundesligaLineups$PosY))
