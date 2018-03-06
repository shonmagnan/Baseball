library(tidyverse)
library(descr)
library(DataExplorer)
library(sqldf)

#connect to DB
db <- dbConnect(SQLite(), dbname = "Lahman.sqlite")
#pull data i need
BB1 <- dbGetQuery(db, "SELECT yearID, teamID, HR, SO, G 
                       FROM Teams 
                       Where yearID > 1899") 
#close DB
dbDisconnect(db)

#look at data
plot_histogram(BB1)
#i have data with 0 strikeouts - leaving them for now
#BB1 <- filter(BB1, SO > 0)
#qplot(BB1$SO, geom = "histogram")

#create decade variable
freq(BB1$yearID, plot = FALSE)
YearBins = paste(seq(1900, 2010, 10),"'s",sep = "")
BB1$Decade <- cut(BB1$yearID, seq(1899, 2020, 10), labels = YearBins)
freq(BB1$Decade, plot = FALSE)

#create table
by_decade <- group_by(BB1, Decade)
HR_table <- summarize(by_decade, HRperG = mean(HR/G*2)) 
SO_table <- summarize(by_decade, SOperG = mean(SO/G*2)) 
ggplot(data = HR_table, aes(x = Decade, y = HRperG, group = 1)) +
  geom_line() +
  geom_point()
ggplot(data = SO_table, aes(x = Decade, y = SOperG, group = 1)) +
  geom_line() +
  geom_point()

cor(HR_table$HRperG, SO_table$SOperG)

by_year <- group_by(BB1, yearID)
HR_table2 <- summarize(by_year, HRperG = mean(HR/G*2)) 
SO_table2 <- summarize(by_year, SOperG = mean(SO/G*2)) 
ggplot(data = HR_table2, aes(x = yearID, y = HRperG, group = 1)) +
  geom_line() +
  geom_point()


#need to multiple by 2 as this is a TEAM per game and I need a total game (so 2 teams)

#question 2
db <- dbConnect(SQLite(), dbname = "Lahman.sqlite")
dbListFields(db, "Teams")

#pull data i need for question 2
BB2 <- dbGetQuery(db, "SELECT yearID, lgID, sum(R) as sum_R, sum(G) as sum_G 
                       FROM Teams 
                       Where yearID > 1963
                       GROUP BY yearID, lgID
                       ORDER BY yearID") 
#close DB
dbDisconnect(db)

BB2 <- BB2 %>% mutate(runs_scored = sum_R/sum_G)
ggplot(BB2, aes(x = yearID, y = runs_scored, color = lgID)) + 
  geom_line()

BB3 <- BB2 %>% select(yearID, lgID, runs_scored) %>%
  spread(lgID, runs_scored) %>%
  mutate(RS_diff = AL - NL)
         
ggplot(BB3, aes(yearID)) + 
  geom_line(aes(y = AL, colour = "AL")) + 
  geom_line(aes(y = NL, colour = "NL"))

ggplot(data = BB3, aes(x = yearID, y = RS_diff, group = 1)) +
  geom_line()


#question 3
db <- dbConnect(SQLite(), dbname = "Lahman.sqlite")
dbListFields(db, "Pitching")

#pull data i need for question 2
BB3 <- dbGetQuery(db, "SELECT yearID, CG, GS
                       FROM Pitching 
                       Where (yearID >= 1900 and yearID <= 1909) or (yearID >= 2000 and yearID <= 2010)") 
#close DB
dbDisconnect(db)

table(BB3$yearID)

BB3 %>% filter(yearID <= 1909)  %>%
  summarise(CR_rate = sum(CG)/sum(GS))
BB3 %>% filter(yearID >= 2000)  %>%
  summarise(CR_rate = sum(CG)/sum(GS))

