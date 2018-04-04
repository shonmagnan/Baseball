library(tidyverse)
library(sqldf)

#creating game log database
zippedfiles <- list.files(path = "~/baseball/download.folder/zipped", pattern = "*.zip")
for (i in 1:length(zippedfiles)) {
  unzip(paste("~/baseball/download.folder/zipped/", zippedfiles[i], sep = ""), 
      exdir = "~/baseball/download.folder/unzipped")}


db <- dbConnect(SQLite(), dbname = "~/baseball/databases/retrosheet_game_logs.sqlite")

fields <- read.csv("download.folder/unzipped/game_log_header.csv")

temp <- read.csv(file = "~/baseball/download.folder/unzipped/GL1871.txt", header = FALSE)
colnames(temp) <- colnames(fields) 
dbWriteTable(conn = db, name =  "GameLogs",value = temp, row.names = FALSE)
rm(temp)

unzippedfiles <- list.files(path = "~/baseball/download.folder/unzipped", pattern = "*.TXT")
unzippedfiles <- unzippedfiles[-1]

for (i in 1:length(unzippedfiles)) {
    temp <- read.csv(file = paste("download.folder/unzipped/", unzippedfiles[i],sep = ""), header = FALSE)
    colnames(temp) <- colnames(fields) 
    dbWriteTable(conn = db, name = "GameLogs", 
               value = temp, 
               row.names = FALSE, header = TRUE, append = TRUE) 
    rm(temp)
  
}

dbListTables(db)
dbListFields(db,"GameLogs")

test <- dbGetQuery(db, "SELECT Date FROM GameLogs")
test$Date <- round(test$Date/10000,0)
table(test$Date)
rm(test)

dbDisconnect(db)

#I'm going to add the park ID's and retro_id's to the database

db <- dbConnect(SQLite(), dbname = "~/baseball/databases/retrosheet_game_logs.sqlite")
temp <- read.csv("~/baseball/download.folder/unzipped/parkcode.txt")
dbWriteTable(conn = db, name =  "ParkCode", value = temp, row.names = FALSE)
temp <- read.csv("~/baseball/download.folder/unzipped/Retro_ID.txt")
dbWriteTable(conn = db, name =  "RetroID", value = temp, row.names = FALSE)
temp <- read.csv("~/baseball/download.folder/unzipped/CurrentNames.csv", header = FALSE)
colnames(temp) <- c("Current_franchise_ID", "Franchise_ID",
                    "League",
                    "Division",
                    "Location_name",
                    "Nickname",
                    "Alternate_nicknames",
                    "Date_first_game_w_combination",
                    "Date_last_game_ w_combination",
                    "City",
                    "State")
dbWriteTable(conn = db, name =  "TeamNames", value = temp, row.names = FALSE)

dbListFields(db, "TeamNames")
dbDisconnect(db)