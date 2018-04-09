library(devtools)
install_github("cpsievert/pitchRx")

library(pitchRx)
library(tidyverse)
library(doParallel)

# Create an empty database instance.
con <- dbConnect(RSQLite::SQLite(), dbname = "mlbgameday.sqlite3")

# Optional: Use parallel processing to populate the database with Spring Training Games.

# Set the number of cores to use as the machine's maximum number of cores minus 1 for background processes.
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)  
registerDoParallel(cl)

get_payload(start = "2018-01-01", end = "2018-03-28", db_con = con)

# Stop and remove cluster.
stopImplicitCluster()
rm(cl)


dat <- scrape(start = "2018-04-01", end = "2018-04-01")
locations <- select(dat$pitch, pitch_type, start_speed, px, pz, des, num, gameday_link)
names <- select(dat$atbat, pitcher, batter, pitcher_name, batter_name, num, gameday_link, event, stand)
data <- names %>% filter(pitcher_name == "Jose Berrios") %>% inner_join(locations, ., by = c("num", "gameday_link"))


#As of XML2R 0.0.5, asyncronous downloads can be performed
dat <- scrape(start = "2017-08-01", end = "2017-08-01", async = TRUE)

# Scrape PITCHf/x from Minnesota Twins 2011 season
data(gids, package = "pitchRx")
twins11 <- gids[grepl("min", gids) & grepl("2011", gids)]
dat <- scrape(game.ids = twins11[1]) #scrapes 1st game only

data(nonMLBgids, package = "pitchRx")
# Grab IDs for triple A games on June 1st, 2011
# This post explains more about obtaining game IDs with regular expressions --
# http://baseballwithr.wordpress.com/2014/06/30/pitchrx-meet-openwar-4/
aaa <- nonMLBgids[grepl("2011_06_01_[a-z]{3}aaa_[a-z]{3}aaa", nonMLBgids)]
dat <- scrape(game.ids = aaa)

# Create SQLite database, then collect and store data in that database
library(dplyr)
my_db <- src_sqlite("Gameday.sqlite3")
scrape(start = "2013-08-01", end = "2013-08-01", connect = my_db$con)

# Collect other data complementary to PITCHf/x and store in database
files <- c("inning/inning_hit.xml", "miniscoreboard.xml", "players.xml")
scrape(start = "2013-08-01", end = "2013-08-01", connect=my_db$con, suffix = files)

# Simple example to demonstrate database query using dplyr
# Note that 'num' and 'gameday_link' together make a key that allows us to join these tables
locations <- select(tbl(my_db, "pitch"), px, pz, des, num, gameday_link)
names <- select(tbl(my_db, "atbat"), pitcher_name, batter_name, num, gameday_link)
que <- inner_join(locations, filter(names, batter_name == "Paul Goldschmidt"),
                  by = c("num", "gameday_link"))
que$query #refine sql query if you'd like
pitchfx <- collect(que) #submit query and bring data into R




## End(Not run)