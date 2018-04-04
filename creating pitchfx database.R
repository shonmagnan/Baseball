library(tidyverse)
library(sqldf)
library(pitchRx)

db <- dbConnect(SQLite(), dbname = "Gameday.sqlite") 
scrape(start = "2008-03-25", end = "2008-09-30", connect = db)
files <- c("inning/inning_hit.xml", "miniscoreboard.xml", "players.xml")
scrape(start = "2008-03-25", end = "2008-09-30", connect = db, suffix = files)

scrape(start = "2009-04-05", end = "2009-10-06", connect = db)
scrape(start = "2009-04-05", end = "2009-10-06", connect = db, suffix = files)

scrape(start = "2010-04-04", end = "2010-10-03", connect = db)
scrape(start = "2010-04-04", end = "2009-10-03", connect = db, suffix = files)

scrape(start = "2011-03-31", end = "2011-09-28", connect = db)
scrape(start = "2011-03-31", end = "2011-09-28", connect = db, suffix = files)


dbListTables(db)
dbListFields(db, "action")
dbListFields(db, "po")
dbListFields(db, "atbat")
dbListFields(db, "pitch")
#this is a test  Tstyilasdfjalsk 

