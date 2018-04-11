library(tidyverse)
library(descr)
library(DataExplorer)
library(sqldf)
library(stringr)
library(lubridate)
library(plotly)

#connect to DB
db <- dbConnect(SQLite(), dbname = "~/baseball/databases/Lahman.sqlite")
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
db <- dbConnect(SQLite(), dbname = "~/baseball/databases/Lahman.sqlite")
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
db <- dbConnect(SQLite(), dbname = "~/baseball/databases/Lahman.sqlite")
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

#1998 HR Race

db <- dbConnect(SQLite(), dbname = "~/baseball/databases/Retrosheet1990s.sqlite")
dbListFields(db,"all1998")
HR_Race <- dbGetQuery(db, "SELECT GAME_ID, BAT_ID, EVENT_CD
                           FROM all1998
                           WHERE BAT_ID = 'sosas001' or BAT_ID = 'mcgwm001'") 
dbDisconnect(db)

table(HR_Race$EVENT_CD)

#split apart GAME_ID
HR_Race <- HR_Race %>% mutate(park = str_sub(GAME_ID, 1,3)) %>%
            mutate(date = str_sub(GAME_ID,4,11)) %>%
            mutate(date_yr = str_sub(GAME_ID,4,7)) %>%
            mutate(date_mth = str_sub(GAME_ID,8,9)) %>%
            mutate(date_day = str_sub(GAME_ID,10,11)) %>%
            mutate(game_number = str_sub(GAME_ID,12,12))

#create first table for graph
mth_table <- HR_Race %>% filter(EVENT_CD == 23) %>%   #Event_CD = 23 is HR
  group_by(BAT_ID, date_mth) %>%
  summarize(., count = n()) %>%
  spread(key = 'BAT_ID', value = 'count') %>%
  mutate(sosas001 = ifelse(is.na(sosas001),0,sosas001)) %>%
  mutate(MM_sum = cumsum(mcgwm001)) %>%
  mutate(SS_sum = cumsum(sosas001)) %>%
  select(date_mth, MM_sum, SS_sum)
 
g <- ggplot(mth_table, aes(as.numeric(date_mth))) + 
  geom_line(aes(y = MM_sum, colour = "Mark"), size = 1) + 
  geom_line(aes(y = SS_sum, colour = "Sammy"), size = 1)
g <- g + scale_x_continuous(breaks = seq(3,9,1))
g <- g + ggtitle("1998 HR Battle: Mark vs Sammy") +
  labs(x =  "Month", y = "Cummlative HR's")
g

#gather the data so it is easier to graph
mth_table2 <- HR_Race %>% filter(EVENT_CD == 23) %>%
  group_by(BAT_ID, date_mth) %>%
  summarize(., count = n()) %>%
  spread(key = 'BAT_ID', value = 'count') %>%
  mutate(sosas001 = ifelse(is.na(sosas001),0,sosas001)) %>%
  mutate(MM_sum = cumsum(mcgwm001)) %>%
  mutate(SS_sum = cumsum(sosas001)) %>%
  select(date_mth, MM_sum, SS_sum)  %>%
  gather('MM_sum', 'SS_sum', key = 'hitter', value = "HR" ) %>%
  mutate(hitter = factor(hitter, levels = c("MM_sum", "SS_sum"), labels = c("Mark", "Sammy"))) %>%
  mutate(date_mth = as.numeric(date_mth))
  
g2 <- ggplot() + theme_bw()  +
                geom_line(aes(y = HR, x = date_mth, color = hitter), size = 1.5, 
                           data = mth_table2, stat = 'identity') +
                 theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) 
g2 <- g2 + scale_x_continuous(breaks = seq(3, 9 ,1))
g2 <- g2 + ggtitle("1998 HR Battle: Mark vs Sammy") +
  labs(x =  "Month", y = "Cummlative HR's")
colour <- c("dark red", "dark blue")
g2 <- g2 + scale_colour_manual(values = colour)
g2

#XKCD style graph - couldn't get this to work
library(extrafont)
font_import()

download.file("http://simonsoftware.se/other/xkcd.ttf",
              dest = "xkcd.ttf", mode = "wb")
system("mkdir ~/.fonts")
system("cp xkcd.ttf  ~/.fonts")
font_import(paths = "~/.fonts", pattern = "[X/x]kcd")
fonts()
loadfonts()

fill <- c("#56B4E9", "#ff69b4")

p1 <- ggplot() +
  geom_line(aes(y = HR, x = date_mth, colour = hitter), size = 1.5, data = mth_table2, stat = "identity") +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  scale_x_continuous(breaks = seq(3,9,1)) +
  labs(x = "Months", y = "Cummlative HR's") +
  ggtitle("1998 HR Battle: Mark vs Sammy") +
  scale_color_manual(values = fill) +
  theme(axis.line = element_line(size = 1, colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(plot.title = element_text(family = "xkcd-Regular"), text = element_text(family = "xkcd-Regular"),
        axis.text.x = element_text(colour = "black", size = 10),
        axis.text.y = element_text(colour = "black", size = 10),
        legend.key = element_rect(fill = "white", colour = "white"))
p1

#HR's

db <- dbConnect(SQLite(), dbname = "~/baseball/databases/Retrosheet_game_logs.sqlite")
dbListFields(db,"GameLogs")
HR_mth <- dbGetQuery(db, "SELECT Date, HomeHR, VisitorHR, ParkID
                           FROM GameLogs
                           WHERE Date > 19799999 and Date < 20120000") 
dbDisconnect(db)

#month HR
HR_mth_table <- HR_mth %>%  select(-ParkID) %>%
                            mutate(total_HR = HomeHR + VisitorHR) %>%
                            mutate(Date = ymd(Date)) %>%
                            mutate(Month = month(Date, label = TRUE)) %>%
                            filter(!Month == "Mar"  ) %>%
                            group_by(Month) %>%
                            summarise(., mean(total_HR, na.rm = TRUE))
HR_mth_table

#park HR
HR_park_table <- HR_mth %>% mutate(total_HR = HomeHR + VisitorHR) %>%
                            mutate(Date = ymd(Date)) %>%
                            mutate(Month = month(Date, label = TRUE)) %>%
                            filter(!Month == "Mar"  ) %>%
                            group_by(ParkID) %>%
                            summarise(ParkN = n(),
                                     ParkTotal = mean(total_HR, na.rm = TRUE)) %>%
                            filter(ParkN > 400) %>%
                            arrange(., desc(ParkTotal))
print(HR_park_table, n = nrow(HR_park_table))

#Umpires and runs
db <- dbConnect(SQLite(), dbname = "~/baseball/databases/Retrosheet_game_logs.sqlite")
dbListFields(db,"GameLogs")
R_ump <- dbGetQuery(db, "SELECT Date, HomeRunsScore, VisitorRunsScored, UmpireHID
                     FROM GameLogs
                     WHERE Date > 19799999 and Date < 20120000") 
dbDisconnect(db)

#park HR
R_ump_table <- R_ump %>% mutate(total_R = HomeRunsScore + VisitorRunsScored) %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  filter(!Month == "Mar"  ) %>%
  group_by(UmpireHID) %>%
  summarise(UmpireN = n(),
            UmpireTotal = mean(total_R, na.rm = TRUE)) %>%
  filter(UmpireN > 399) %>%
  arrange(., desc(UmpireTotal))

print(R_ump_table, n = nrow(R_ump_table))

freq(R_ump_table$UmpireN, plot = F)

#day of the week analysis
db <- dbConnect(SQLite(), dbname = "~/baseball/databases/Retrosheet_game_logs.sqlite")
dbListFields(db,"GameLogs")
DOW <- dbGetQuery(db, "SELECT Date, DayOfWeek, Attendence
                    FROM GameLogs
                    WHERE Date > 19799999 and Date < 20120000") 
dbDisconnect(db)

DOW_table <- DOW %>% mutate(Date = ymd(Date)) %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  filter(!Month == "Mar"  ) %>%
  group_by(DayOfWeek) %>%
  filter(Attendence > 0) %>%
  summarise(DOWMean = mean(Attendence, na.rm = TRUE))
DOW_table


db <- dbConnect(SQLite(), dbname = "~/baseball/databases/Retrosheet1990s.sqlite")
dbListFields(db,"all1998")
HR_Race <- dbGetQuery(db, "SELECT GAME_ID, BAT_ID, EVENT_CD, BASE1_RUN_ID, BASE2_RUN_ID, BASE3_RUN_ID, SF_FL
                      FROM all1998
                      WHERE BAT_ID = 'sosas001' or BAT_ID = 'mcgwm001'")  
dbDisconnect(db)

HR_table <- HR_Race %>% mutate(MOB1 = ifelse(BASE1_RUN_ID == "", 0, 1)) %>%
                        mutate(MOB2 = ifelse(BASE2_RUN_ID == "", 0, 1)) %>%
                        mutate(MOB3 = ifelse(BASE3_RUN_ID == "", 0, 1)) %>%
                        mutate(Total_MOB = ifelse(MOB1 + MOB2 + MOB3 > 0,1,0)) %>%
                        mutate(HR = ifelse(EVENT_CD == 23, 1, 0)) %>%
                        filter(!EVENT_CD %in% c(4,6,8,9,10,11)) %>%
                        select(BAT_ID, HR,Total_MOB) %>%
                        group_by(BAT_ID, Total_MOB) %>%
                        summarise(AB = n(), HRs = sum(HR))
                        
HR_table2 <- HR_Race %>% mutate(MOB1 = ifelse(BASE1_RUN_ID == "", 0, 1)) %>%
  mutate(MOB2 = ifelse(BASE2_RUN_ID == "", 0, 1)) %>%
  mutate(MOB3 = ifelse(BASE3_RUN_ID == "", 0, 1)) %>%
  mutate(Total_MOB = ifelse(MOB1 + MOB2 + MOB3 > 0,1,0)) %>%
  mutate(HR = ifelse(EVENT_CD == 23, 1, 0)) %>%
  filter(!EVENT_CD %in% c(4,6,8,9,10,11,14,15,16)) %>%
  filter(SF_FL == 0) %>%
  select(BAT_ID, HR,Total_MOB) %>%
  group_by(BAT_ID, Total_MOB) %>%
  summarise(AB = n(), HRs = sum(HR))

#pitch seq   - there is something wrong in the book
db <- dbConnect(SQLite(), dbname = "~/baseball/databases/Retrosheet2010s.sqlite")
ps <- dbGetQuery(db, "SELECT GAME_ID, EVENT_CD, AB_FL, BALLS_CT, STRIKES_CT, PITCH_SEQ_TX
                      FROM all2011")
dbDisconnect(db)

freq(ps$PITCH_SEQ_TX, plot = F)
table(ps$BALLS_CT[ps$AB_FL == 1], ps$STRIKES_CT[ps$AB_FL == 1])



ps_table <- ps %>% filter(AB_FL == 1)  %>%
  mutate(H = ifelse(EVENT_CD %in% c(20,21,22,23),1,0))  %>%
  filter((BALLS_CT == 2 & STRIKES_CT == 0) | (BALLS_CT == 0 & STRIKES_CT == 2)) %>%
  mutate(pitch_count = ifelse(BALLS_CT == 2 & STRIKES_CT == 0, 1, 0)) %>%
  group_by(pitch_count) %>%
  summarise(BA = mean(H))



PS_table <- ps %>%
  filter(AB_FL == 1) %>% 
  mutate(H = ifelse(EVENT_CD %in% c(14,15, 20,21,22,23),1,0))  %>%
  mutate(PITCH_SEQ_TX2 = str_replace_all(PITCH_SEQ_TX, "[.>123+*N]", "")) %>%
  mutate(PITCH_SEQ_TX2 = str_replace_all(PITCH_SEQ_TX2, "[BIPV]", "B")) %>%
  mutate(PITCH_SEQ_TX2 = str_replace_all(PITCH_SEQ_TX2, "[CFKLMOQRST]","S")) %>%
  mutate(First2 = str_sub(PITCH_SEQ_TX2, 1,2)) %>%
  mutate(PC02 = ifelse(First2 == "SS", 1,0)) %>%
  mutate(PC20 = ifelse(First2 == "BB", 1,0)) 

table(PS_table$PC20)

freq(PS_table$H[PS_table$PC20 == 1], plot = F )
freq(PS_table$H[PS_table$PC02 == 1], plot = F )

ps$sequence <- gsub("[.>123+*N]", "", ps$PITCH_SEQ_TX)
ps$C20 <- grepl("^[BIPV]{2}", ps$sequence)

ps_table <- ps %>% filter(AB_FL == 1)  %>%
  mutate(H = ifelse(EVENT_CD %in% c(20,21,22,23),1,0))  %>%
  filter((BALLS_CT == 2 & STRIKES_CT == 0) | (BALLS_CT == 0 & STRIKES_CT == 2)) %>%
  mutate(pitch_count = ifelse(BALLS_CT == 2 & STRIKES_CT == 0, 1, 0)) %>%
  group_by(pitch_count) %>%
  summarise(BA = mean(H))

freq(PS_table$PC02, plot = F)

##Pitchfx data

db <- dbConnect(SQLite(), dbname = "~/baseball/databases/gameday.sqlite")

pitch <- dbGetQuery(db, "SELECT pitch_type, start_speed, px, pz, des, num, gameday_link
                      FROM pitch
                      where gameday_link = 'gid_2015_05_21_slnmlb_nynmlb_1'")

hit <- dbGetQuery(db, "SELECT pitcher, batter, pitcher_name, batter_name, num, gameday_link, event, stand
                      FROM atbat
                      where gameday_link = 'gid_2015_05_21_slnmlb_nynmlb_1'")

data <- hit %>% filter(pitcher_name == "Jacob DeGrom") %>% inner_join(pitch, ., by = c("num", "gameday_link"))
head(data)

freq(data$des, plot = F)

# subset the data, keeping all rows but only columns number 1 through 5 and 13
deGrom <- data[,c(1:5, 13)]

# filter for swinging strikes
deGrom_swing <- deGrom %>% filter(des %in% c("Swinging Strike", "Called Strike")) 

# plot the pitches, coloring them by velocity
p <- ggplot(deGrom_swing, aes(px, pz, color = start_speed))

# add in customized axis and legend formatting and labels
p <- p + scale_x_continuous(limits = c(-3,3)) + 
  scale_y_continuous(limits = c(0,5)) + 
  annotate("rect", xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0) + 
  labs(title = "Jacob deGrom: Swinging Strikes, 5/21/2015") + 
  ylab("Vertical Location (ft.)") + 
  xlab("Horizontal Location (ft): Catcher's View") + 
  labs(color = "Velocity (mph)")

# format the points
p <- p + geom_point(size = 10, alpha = .65)

# finish formatting
p <- p + theme(axis.title = element_text(size = 15, color = "black", face = "bold")) + 
  theme(plot.title = element_text(size = 30, face = "bold", vjust = 1)) + 
  theme(axis.text = element_text(size = 13, face = "bold", color = "black")) + 
  theme(legend.title = element_text(size = 12)) + 
  theme(legend.text = element_text(size = 12))

# view the plot
p

p <- ggplot(deGrom_swing, aes(px, pz, color = pitch_type))

# add in customized axis and legend formatting and labels
p <- p + scale_x_continuous(limits = c(-3,3)) + 
  scale_y_continuous(limits = c(0,5)) + 
  annotate("rect", xmin = -1, xmax = 1, ymin = 1.5, ymax = 3.5, color = "black", alpha = 0) + 
  labs(title = "Jacob deGrom: Swinging Strikes, 5/21/2015") + 
  ylab("Vertical Location (ft.)") + 
  xlab("Horizontal Location (ft): Catcher's View") + 
  labs(color = "Pitch Type")

# format the points
p <- p + geom_point(size = 10, alpha = .65)

# finish formatting
p <- p + theme(axis.title = element_text(size = 15, color = "black", face = "bold")) + 
  theme(plot.title = element_text(size = 30, face = "bold", vjust = 1)) + 
  theme(axis.text = element_text(size = 13, face = "bold", color = "black")) + 
  theme(legend.title = element_text(size = 12)) + 
  theme(legend.text = element_text(size = 12))

# view the plot
p

#let's recreate graph from page 22
hit <- dbGetQuery(db, "SELECT a.batter, a.batter_name, a.num, a.gameday_link, a.event AS atbat_event, a.date,
                       p.pitch_type, p.start_speed, p.px, p.pz, p.sz_top, p.sz_bot, p.des, p.num, p.gameday_link
                       FROM atbat a
                       JOIN pitch p ON a.gameday_link = p.gameday_link AND a.num = p.num
                       WHERE a.batter_name = 'Jose Bautista' AND (a.date >= 2010 AND a.date <2011) AND a.event = 'Home Run' AND p.des = 'In play, run(s)'")

freq(hit$pitch_type, plot = F)

hit <- data.frame(hit) %>% 
  select(batter:sz_bot) %>%
  mutate(pitch_type_collapsed = ifelse(
  pitch_type %in% c("CH", "CU", "SL"), "breaking", "fastball" 
))

hit2 <- filter(hit, pitch_type_collapsed == "breaking")

p <- ggplot(hit, aes(px, pz, shape = pitch_type_collapsed))

# add in customized axis and legend formatting and labels
p <- p + scale_x_continuous(limits = c(-3,3)) +
  scale_y_continuous(limits = c(0,5)) + 
  annotate("rect", xmin = -1, xmax = 1, ymin = mean(hit$sz_bot), ymax = mean(hit$sz_top), color = "black", alpha = 0) + 
  labs(title = "Jose Bautista HRs, 2010") + 
  ylab("Vertical Location (ft.)") + 
  xlab("Horizontal Location (ft): Catcher's View") + 
  labs(color = "Pitch Type")

# format the points
p <- p +   geom_point(aes(shape = pitch_type_collapsed, color = start_speed), size = 3)

# finish formatting
p <- p + theme(axis.title = element_text(size = 11, color = "black", face = "bold")) + 
  theme(plot.title = element_text(size = 15, face = "bold", vjust = 1)) + 
  theme(axis.text = element_text(size = 10, face = "bold", color = "black")) + 
  theme(legend.title = element_text(size = 9)) + 
  theme(legend.text = element_text(size = 9))

# view the plot
ggplotly(p)

#pulling a particular game and pitcher
data(gids, package = "pitchRx")
ph <- gids[grepl("sea", gids) & grepl("2012_04_21", gids)]

pitch <- dbGetQuery(db, "SELECT pitch_type, start_speed, px, pz, des, num, gameday_link
                         FROM pitch
                          WHERE gameday_link = 'gid_2012_04_21_chamlb_seamlb_1'")

hit <- dbGetQuery(db, "SELECT pitcher, batter, pitcher_name, batter_name, num, gameday_link, event, stand
                       FROM atbat
                       WHERE gameday_link = 'gid_2012_04_21_chamlb_seamlb_1'")

data <- hit %>% filter(pitcher_name == "Philip Humber") %>% inner_join(pitch, ., by = c("num", "gameday_link"))
head(data)


#Q1 from Pitchfx section
Q1 <- dbGetQuery(db, "SELECT a.batter_name, a.num, a.gameday_link, a.date,
                      p.pitch_type
                      FROM atbat a
                      JOIN pitch p ON a.gameday_link = p.gameday_link AND a.num = p.num
                      WHERE a.date >= 2008 AND a.date < 2012")

#to drop non-regular season games I need to download all games and fix the gameday_link
game <- dbGetQuery(db, "SELECT gameday_link, game_type from game")

game$gameday_link <- paste("gid_",game$gameday_link, sep = "")
Q1 <- left_join(Q1, game)
freq(Q1$game_type, plot = F)

Q1_table <- Q1 %>% select(c(game_type, batter_name, pitch_type, date)) %>% 
  filter(game_type == "R") %>%
  filter(!is.na(pitch_type)) %>%
  mutate(fastball = ifelse(pitch_type %in% c("FA", "FF", "FT"), 1, 0)) %>%
  mutate(fastball = ifelse(pitch_type %in% c("PO", "IN", "UN", "AB"), NA, fastball)) %>%
  select(c(batter_name, fastball)) %>%
  group_by(batter_name) %>%
  summarise(n = n(), fastball_per = mean(fastball, na.rm = TRUE)) %>%
  filter(n > 5000) %>%
  arrange(desc(fastball_per))
head(Q1_table)

#Q2
Q2 <- dbGetQuery(db, "SELECT a.pitcher_name, a.num, a.gameday_link, a.date,
                      p.start_speed
                      FROM atbat a
                      JOIN pitch p ON a.gameday_link = p.gameday_link AND a.num = p.num
                      WHERE a.date >= 2008 AND a.date < 2012")

Q2_table <- Q2 %>% left_join(game) %>%
  filter(game_type == "R") %>%
  arrange(desc(start_speed)) %>%
  slice(1:20)
Q2_table

#Q3 - they are doing some cleaning I don't know about
Q3 <- dbGetQuery(db, "SELECT a.pitcher_name, a.num, a.gameday_link, a.date,
                      p.pitch_type, r.event
                      FROM atbat a
                      JOIN pitch p ON a.gameday_link = p.gameday_link AND a.num = p.num
                      JOIN runner r ON a.gameday_link = r.gameday_link AND r.num = p.num
                      WHERE a.date >= 2008 AND a.date < 2012")

Q3_table <- Q3 %>% left_join(game) %>%
  filter(game_type == "R") %>%
  filter(event %in% c("Caught Stealing 2B","Stolen Base 2B", "Picked off stealing 2B")) %>%
  filter(!is.na(pitch_type)) %>%
  mutate(fastball = ifelse(pitch_type %in% c("FA", "FF", "FT"), 1, 0)) %>%
  mutate(fastball = ifelse(pitch_type %in% c("PO", "IN", "UN", "AB"), NA, fastball)) %>%
  mutate(sb = ifelse(event == "Stolen Base 2B",1,0)) %>%
  group_by(fastball) %>%
  summarise(sb_per = mean(sb, na.rm = TRUE))
Q3_table

