library(tidyverse)
library(sqldf)
library(pitchRx)

db <- dbConnect(SQLite(), dbname = "~/baseball/databases/Gameday.sqlite") 
scrape(start = "2008-01-01", end = "2008-12-31", connect = db)
files <- c("inning/inning_hit.xml", "miniscoreboard.xml", "players.xml")
scrape(start = "2008-01-01", end = "2008-12-31", connect = db, suffix = files)


scrape(start = "2009-01-01", end = "2009-12-31", connect = db)
scrape(start = "2009-01-01", end = "2009-12-31", connect = db, suffix = files)


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

