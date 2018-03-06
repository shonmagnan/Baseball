library(shiny)
library(sqldf)

db <- dbConnect(SQLite(), dbname = "~/baseball/databases//Lahman.sqlite")
dbListTables(db)
#pull data i need
bb_names <- dbGetQuery(db, "SELECT nameLast, nameFirst, playerID, retroID, bbrefID 
                       FROM Master") 
#close DB
dbDisconnect(db)