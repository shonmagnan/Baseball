library(tidyverse)
library(sqldf)
library(pitchRx)
library(RHelper)
library(XML2R)

db <- dbConnect(SQLite(), dbname = "~/baseball/databases/Gameday.sqlite") 

files <- c("inning/inning_hit.xml", "players.xml","miniscoreboard.xml")
files2 <- c("players.xml","inning/inning_hit.xml")

scrape(start = "2008-01-01", end = "2008-12-31", connect = db)
scrape(start = "2008-01-01", end = "2008-12-31", connect = db, suffix = files)

scrape(start = "2009-01-01", end = "2009-12-31", connect = db)
scrape(start = "2009-01-01", end = "2009-12-31", connect = db, suffix = files)

scrape(start = "2010-01-01", end = "2010-12-31", connect = db)
scrape(start = "2010-01-01", end = "2010-12-31", connect = db, suffix = files)

scrape(start = "2011-01-01", end = "2011-12-31", connect = db)
scrape(start = "2011-01-01", end = "2011-12-31", connect = db, suffix = files)

scrape(start = "2012-01-01", end = "2012-12-31", connect = db)
scrape(start = "2012-01-01", end = "2012-12-31", connect = db, suffix = files)

scrape(start = "2013-01-01", end = "2013-12-31", connect = db)
scrape(start = "2013-01-01", end = "2013-12-31", connect = db, suffix = files)

scrape(start = "2014-01-01", end = "2014-12-31", connect = db)
scrape(start = "2014-01-01", end = "2014-12-31", connect = db, suffix = files)

scrape(start = "2015-01-01", end = "2015-12-31", connect = db)
scrape(start = "2015-01-01", end = "2015-12-31", connect = db, suffix = files)

scrape(start = "2016-01-01", end = "2016-12-31", connect = db)
scrape(start = "2016-01-01", end = "2016-12-31", connect = db, suffix = files)

scrape(start = "2017-01-01", end = "2017-12-31", connect = db)
#had to split them into two - one of his and one of my own
scrape(start = "2017-01-01", end = "2017-12-31", connect = db, suffix = files2)
scrape2(start = "2017-01-01", end = "2017-12-31", connect = db, suffix = "miniscoreboard.xml")

#now start checking and cleaning a bit
dbListTables(db)
dbRemoveTable(db, "Error//Code")
dbRemoveTable(db, "Error//HostId")
dbRemoveTable(db, "Error//Message")
dbRemoveTable(db, "Error//RequestId")
dbRemoveTable(db, "epg")

#innings_all
dbListFields(db, "action")
dbListFields(db, "po")
dbListFields(db, "atbat")
dbListFields(db, "pitch")
dbListFields(db, "runner")

#innings_hit
dbListFields(db, "hip")

#players
dbListFields(db, "coach")
dbListFields(db, "player")
dbListFields(db, "umpire")

#miniscoreboard
dbListFields(db, "media")
dbListFields(db, "game")

#pull data and see if the 2017 works right...
BB1 <- dbGetQuery(db, "SELECT start
                       FROM game") 
tail(BB1)

dbDisconnect(db)


