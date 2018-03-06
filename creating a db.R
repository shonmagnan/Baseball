library(tidyverse)
library(sqldf)

#path for Linux '~/Documents/Baseball Data/Lahman Data/baseballdatabank-2017.1/core/'


#pull in names of all files
dbTables <- list.files(path = "~/baseball/baseballdatabank-2017.1/core", pattern = "*.csv")
  
#create/open DB
db <- dbConnect(SQLite(), dbname = "Lahman.sqlite")
#sqldf("attach 'Lahman.sqlite' as new") #also works

for (i in 1:length(dbTables)) {
  if (dbExistsTable(db, dbTables[i])) {
    dbRemoveTable(db, dbTables[i])
  }
  dbWriteTable(conn = db, name =  (gsub(" ","",substr(dbTables[i], 1, which(strsplit(dbTables[i], "")[[1]] == ".") - 1))), 
               value = paste("~/baseball/baseballdatabank-2017.1/core/", dbTables[i], sep = ""), row.names = FALSE, header = TRUE) 
}

# Make sure every thing wrote correctly.
dbListTables(db)

dbListFields(db, "Master")
dbListFields(db, "Batting")
dbListFields(db, "Pitching")
dbListFields(db, "Fielding")
dbListFields(db, "Teams")

test <- dbGetQuery(db, "SELECT * FROM Batting LIMIT 5")
test
rm(test)

# Close the database connection.
dbDisconnect(db)

