library(tidyverse)
library(descr)
library(sqldf)

#chapter 2 exercises

#1
sb <- c(1406,938,897,741,738,689,506,504,474)
cs <- c(335,307,212,195,109,162,136,131,114)
g <- c(3081,2616,3034,2826,2476,2649,2599,2683,2379)

SB.Attempt <- sb + cs
Success.Rate <- sb/SB.Attempt
SB.Game = sb/g
plot(SB.Game, Success.Rate)

#2
outcomes <- c("single","out","out","single","out","double","out","walk","out","single")
table(outcomes)
f.outcomes <- factor(outcomes, levels = c("out", "walk", "single", "double"))
table(f.outcomes)

#3
W <- c(373,354,364,471,355,373,361,363,511)
L <- c(208,184,310,279,227,188,208,245,316)
Name <- c("Alexander", "Clemens", "Galvin", "Johnson", "Maddux", "Mathewson", "Nichols", "Spahn", "Young")
Win.PCT <- W/(W + L)
Wins.350 <- data.frame(Name, W, L, Win.PCT)
Wins.350[order(Wins.350$Win.PCT, decreasing = TRUE),]

#4
SO <- c(2198,4672,1806,3509,3371,2502,1868,2583,2803)
BB <- c(951,1580,745,1363,999,844,1268,1434,1217)
Name <- c("Alexander", "Clemens", "Galvin", "Johnson", "Maddux", "Mathewson", "Nichols", "Spahn", "Young")
SO.BB.Ratio <- SO/BB
SO.BB <- data.frame(Name, SO, BB, SO.BB.Ratio)
subset(SO.BB, SO.BB.Ratio > 2.8)
SO.BB[order(SO.BB$BB, decreasing = TRUE),]

#5
db <- dbConnect(SQLite(), dbname = "~/baseball/databases/Lahman.sqlite")
#pull data i need
Pitching <- dbGetQuery(db, "SELECT *
                       FROM Pitching") 
dbDisconnect(db)
Pitching[c("BAOpp","IBB","HBP","SH","SF","GIDP")] <- lapply(Pitching[c("BAOpp","IBB","HBP","SH","SF","GIDP")], as.numeric)

stats <- function(d) {
  c.SO <- sum(d$SO, na.rm = TRUE)
  c.BB <- sum(d$BB, na.rm = TRUE)
  c.IPouts <- sum(d$IPouts, na.rm = TRUE)
  c.midyear <- median(d$yearID, na.rm = TRUE)
  data.frame(SO = c.SO, BB = c.BB, IPouts = c.IPouts, midYear = c.midyear)
}

#library(plyr)
#career.pitching <- ddply(Pitching, .(playerID), stats)

career.pitching <- Pitching %>% group_by(playerID) %>% 
  summarize(c.SO = sum(SO, na.rm = TRUE), 
            c.BB = sum(BB, na.rm = TRUE), 
            c.IPouts = sum(IPouts, na.rm = TRUE),
            c.midyear = median(yearID, na.rm = TRUE)) %>%
  rename(SO = c.SO, BB = c.BB, IPouts = c.IPouts, midyear = c.midyear)


career.pitching <- Pitching %>% group_by(playerID) %>%
  do(data.frame(stats(.)))

career.pitching2 <- career.pitching %>% left_join(.,Pitching, by = "playerID") %>% filter(IPouts.x >= 10000) %>%
  mutate(midYear = round(midYear,0)) %>% filter(midYear == yearID) %>% mutate(SO_BB_Ratio = SO.y/BB.y) %>% filter(G > 10)

p <- ggplot(career.pitching2, aes(yearID, SO_BB_Ratio)) +
  geom_point(size = 3) +
  geom_text(label = career.pitching2$playerID, size = 3, hjust = 1.1, vjust = 1)
p

