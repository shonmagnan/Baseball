dbDisconnect(db)

db <- dbConnect(SQLite(), dbname = "Lahman.sqlite")
batter_stats <- dbGetQuery(db, "SELECT * FROM Teams 
                           WHERE (yearID >= 1955 and yearID <= 2013 and yearID <> 1981 and yearID <> 1994)")
miss_data <- dbGetQuery(db, "SELECT yearID, teamID, SUM(HBP) as HBP_sum, SUM(SF) as SF_sum FROM Batting
                        WHERE (yearID >= 1955 and yearID <= 2013 and yearID <> 1981 and yearID <> 1994)
                        GROUP BY yearID, teamID")

dbDisconnect(db)

str(batter_stats)

#need to turn Y/N into factor
batter_stats[11:14] <-  lapply(batter_stats[11:14], function(x) {x[x == ''] <- NA; return(x)})
batter_stats[11:14] <-  lapply(batter_stats[11:14], factor)

#need to turn some with missing from characters into numeric
batter_stats[25:26] <- lapply(batter_stats[25:26], as.numeric)

#now let's play with dataexplorer
plot_missing((batter_stats))
plot_bar(batter_stats)
plot_histogram(batter_stats$RC)


qplot(batter_stats$RC, geom = "histogram")

freq(batter_stats$yearID[batter_stats$G < 162])

batter_stats <- batter_stats %>% left_join(miss_data)
str(batter_stats)

table(batter_stats$HBP == batter_stats$HBP_sum)
table(batter_stats$SF == batter_stats$SF_sum)
freq(batter_stats$OBP, plot = FALSE)

batter_stats <- mutate(batter_stats,
                       BA = H/AB,
                       OBP = (H + BB + HBP_sum)/(AB + BB + HBP_sum + SF_sum),
                       SLG = (H + X2B + 2 * X3B + 3 * HR)/AB,
                       OPS = OBP + SLG,
                       RC = ((H + BB) * (H + X2B + 2 * X3B + 3 * HR))/(AB + BB))

R2_runs <- function(x) {
  print(round(cor(batter_stats$R, x)^2, 5)) 
}

R2_runs(batter_stats$BA)
R2_runs(batter_stats$HR)
R2_runs(batter_stats$OBP)
R2_runs(batter_stats$H)
R2_runs(batter_stats$SLG)
R2_runs(batter_stats$OPS)
R2_runs(batter_stats$RC)


#try to recreate the graph in ggplot

r2 <- round(cor(batter_stats$R, batter_stats$BA)^2, 5)

#the main part of the graph - data, limits, labels for axis
g <- ggplot(data = batter_stats, 
            aes(x = R, y = BA)) + geom_point(colour = "darkred") +
  scale_x_continuous(name = "Team Runs Scored", breaks = seq(400,1100,100), 
                     label = c("400","500","600","700","800","900","1000","1100")) +
  scale_y_continuous(name = "Batting Average", breaks = seq(.200,.300,.02),
                     label = c(".200", ".220", ".240",".260",".280",".300")) +
  expand_limits(x = c(400,1100), y = c(.20,.3))

#add line, main title, R2
g <- g + geom_smooth(method = "lm", se = 0, color = "white") +
  labs(title = "Hitting Metrics:\nThe Problem With Batting Average",
       subtitle = "Batting Average") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(x = 450, y = .290, label = paste("R^2 == " ,r2)), 
            parse = TRUE, color = "white", fontface = "bold", size = 6)

#adjust theme
g <- g + theme(panel.background = element_rect(fill = "black"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               plot.background = element_rect(fill = "black"),
               axis.title = element_text(color = "white"),
               title = element_text(color = "white"),
               axis.text = element_text(color = "white"),
               axis.line = element_line(color = "white"))

g 


r2 <- round(cor(batter_stats$R, batter_stats$RC)^2, 5)
g <- ggplot(data = batter_stats, 
            aes(x = R, y = RC)) + 
  geom_point(colour = "darkred") +
  scale_x_continuous(name = "Team Runs Scored", breaks = seq(400,1100,100), 
                     label = c("400","500","600","700","800","900","1000","1100")) +
  scale_y_continuous(name = "Runs Created", breaks = seq(400,1100, 100),
                     label = c("400","500","600","700","800","900","1000","1100")) +
  expand_limits(x = c(400,1100), y = c(400,1100)) +
  geom_smooth(method = "lm", se = 0, color = "white") +
  labs(title = "Hitting Metrics:\nThe Problem With Batting Average",
       subtitle = "Runs Created") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(x = 450, y = 1050, label = paste("R^2 == " ,r2)), 
            parse = TRUE, color = "white", fontface = "bold", size = 6) +
  theme(panel.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "black"),
        axis.title = element_text(color = "white"),
        title = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.line = element_line(color = "white"))

plot(g)

g <- ggplot(data = batter_stats, 
            aes(x = R, y = RC)) + 
  geom_point(colour = "darkred") +
  scale_x_continuous(name = "Team Runs Scored", breaks = seq(400,1100,100), 
                     label = c("400","500","600","700","800","900","1000","1100")) +
  scale_y_continuous(name = "Runs Created", breaks = seq(400,1100, 100),
                     label = c("400","500","600","700","800","900","1000","1100")) +
  expand_limits(x = c(400,1100), y = c(400,1100)) +
  geom_smooth(method = "lm", se = 0, color = "white") +
  labs(title = "Hitting Metrics:\nThe Problem With Batting Average",
       subtitle = "Runs Created") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_text(aes(x = 450, y = 1050, label = paste("R^2 == " ,r2)), 
            parse = TRUE, color = "white", fontface = "bold", size = 6)
plot(g)

options(scipen = 999)  # turn-off scientific notation like 1e+48
theme_set(theme_black())  

# Scatterplot
gg <- ggplot(batter_stats, aes(x = R, y = RC)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method = "lm", se = 0, color = "white") + 
  geom_text(aes(x = 450, y = 1050, label = paste("R^2 == " ,r2)), 
            parse = TRUE, color = "white", fontface = "bold", size = 6) +
  xlim(c(400, 1100)) + 
  ylim(c(400, 1100)) + 
  labs(subtitle = "Runs Created", 
       y = "Runs Created", 
       x = "Runs", 
       title = "Hitting Metrics\nThe Problem with Batting Average", 
       caption = "Source: Lahman Data") 


plot(gg)

theme_black <- 
  function(base_size = 11, base_family = "") 
  {
    theme_grey(base_size = base_size, base_family = base_family) %+replace% 
      theme(panel.background = element_rect(fill = "black", colour = NA), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            plot.background = element_rect(fill = "black"),
            axis.title = element_text(color = "white"),
            plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(color = "white", hjust = 0.5),
            axis.text = element_text(color = "white"),
            axis.line = element_line(color = "white"),
            complete = TRUE)
  }