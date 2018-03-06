library(sqldf)
library(tidyverse)
library(rvest)

#connect to DB
db <- dbConnect(SQLite(), dbname = "Lahman.sqlite")
#pull data i need
#dbListFields(db, "Batting")

wOBA_98 <- dbGetQuery(db, "SELECT playerID, 
                  yearID, 
                  teamID, 
                  (0.713*(BB-IBB) + 0.742*HBP + 0.898*(H-X2B-X3B-HR) + 1.257*X2B + 1.580*X3B + 2.007*HR) / (AB + BB - IBB + SF + HBP) AS wOBA
                  FROM Batting
                  WHERE yearID = 1998 AND AB > 300
                  ORDER BY wOBA DESC")
#close DB
dbDisconnect(db)

#nice and clean as I know the node to take
Fangraphs <- xml2::read_html("http://www.fangraphs.com/guts.aspx?type=cn") %>% 
    html_node(xpath = "//*[(@id = \"GutsBoard1_dg1_ctl00\")]") %>% 
    html_table() %>% 
    setNames(c("yearID", "lg_woba","woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B", 
                                "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP"))

Fangraphs2 <- xml2::read_html("https://www.fangraphs.com/guts.aspx?type=pf&teamid=0&season=2017") %>% 
  html_node(xpath = "//*[(@id = \"GutsBoard1_dg1_ctl00\")]") %>%
  html_table()

#my node name isn't totally correct so need some cleaning
espn <- xml2::read_html("https://legacy.baseballprospectus.com/sortable/index.php?cid=1819094") %>%
  html_node(xpath = "//*[(@id = \"TTdata\")]") %>%
  html_table()

espn <- xml2::read_html("https://legacy.baseballprospectus.com/sortable/index.php?cid=1920874") %>%
  html_node(xpath = "//*[(@id = \"TTdata\")]") %>%
  html_table()


#this works but needs cleaning
h <- 'http://www.espn.com/mlb/stats/team/_/stat/batting/year/2017/seasontype/2' %>% read_html()

stats <- h %>% 
  html_node('table') %>% 
  html_table(fill = TRUE) %>% 
  set_names(make.names)

h <- 'http://blog.feedspot.com/video_game_news/' %>% read_html()

game_blogs <- h %>% 
  html_node('table') %>%    # select enclosing table node
  html_table() %>%    # turn table into data.frame
  set_names(make.names) %>%    # make names syntactic
  mutate(Blog.Name = sub('\\s?\\+.*', '', Blog.Name)) %>%    # extract title from name info
  mutate_at(3:5, parse_number) %>%    # make numbers actually numbers
  tbl_df()    # for printing

game_blogs