library(tidyverse)
library(sqldf)
library(pitchRx)
library(XML2R)
source("pitchRx scrape2.R")

db <- dbConnect(SQLite(), dbname = "~/baseball/databases/Gameday.sqlite") 

files2 <- c("players.xml","inning/inning_hit.xml")

scrape(start = "2018-01-01", end = "2018-04-08", connect = db)
scrape(start = "2018-01-01", end = "2018-04-08", connect = db, suffix = files2)
scrape2(start = "2018-01-01", end = "2018-04-08", connect = db, suffix = "miniscoreboard.xml")

dbListTables(db)
dbRemoveTable(db, "epg")

#pull data and see if the 2018 works right...
BB1 <- dbGetQuery(db, "SELECT id
                  FROM game") 
tail(BB1)

dbDisconnect(db)