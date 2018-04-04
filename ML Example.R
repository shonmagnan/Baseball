library(tidyverse)
library(ggthemes)
library(caret)
library(rpart)
library(rpart.plot)
library(modelr)
library(gridExtra)
library(lubridate)
library(broom)
library(stringr)
library(forcats)

#reading in the data from the web, separating year and month, changing them into factors,
#aggregating the data by year, month, borough, maj_cat with count as the summary
crime_df <-
  read_csv(
    "https://files.datapress.com/london/dataset/recorded-crime-summary-data-london-borough-level/2017-01-26T18:50:00/MPS_Borough_Level_Crime.csv",
    col_types = "cccci",
    col_names = c("month", "borough", "maj_cat", "min_cat", "count"),
    skip = 1
  ) %>%
  separate(month, c("year", "month"), sep = 4) %>%
  mutate(
    year = factor(year),
    month = factor(month),
    maj_cat = factor(maj_cat),
    borough = factor(borough)
  ) %>%
  group_by(year, month, borough, maj_cat) %>%
  summarise(count = sum(count))

#EDA

#creating the caption
cap <- labs(caption = paste0(
  "\nSource: data.gov.uk (",
  min(str_c(crime_df$year, "/", crime_df$month)),
  " to ",
  max(str_c(crime_df$year, "/", crime_df$month)),
  ")\nData Science Powered by R"
))


#creating the graph
ggplot(crime_df, aes(
  str_c(year, "/", str_pad(month, 2, pad = 0)),
  count,
  colour = maj_cat,
  group = maj_cat
)) +
  geom_line() +
  facet_wrap( ~ borough, scales = "free_y", ncol = 4) +
  theme_economist() +
  scale_colour_economist(name = "Major Category") +
  scale_x_discrete(breaks = c("2013/12", "2014/12", "2015/12", "2016/12", "2017/12")) +
  ggtitle("London Crime by Borough\n") +
  labs(x = NULL, y = NULL, caption = cap) +
  theme(
    rect = element_rect(fill = "#f9f5f1"),
    plot.background = element_rect(fill = "#f9f5f1"),
    text = element_text(size = 7),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, vjust = 0.4)
  )

these <- c("maj_cat", "borough")
crime_df$month <- as.integer(crime_df$month)

EDA_plot <- function(x) {
  ggplot(crime_df, aes_string(x, "count")) +
    theme_economist() +
    theme(
      rect = element_rect(fill = "#f9f5f1"),
      plot.background = element_rect(fill = "#f9f5f1")) +
      {if (x %in% these) geom_boxplot(fill = "#CFB292")} +
      {if (x %in% these) scale_y_log10()} +
      {if (x %in% these) theme(axis.text.x = element_blank())} +
      {if (x %in% these) theme(axis.title.x = element_text(vjust = -2))} +
      {if (x == "month" | x == "borough") theme(axis.title.y = element_blank())} +
      {if (x == "month") scale_x_continuous(breaks = seq(1, 12, by = 1))} +
      {if (x == "month") coord_cartesian(ylim = c(200, 230))} +
      {if (!x %in% these) geom_smooth(colour = "black")} +
      {if (x == "year") geom_col(fill = "#CFB292")} +
      {if (x == "month") geom_jitter(alpha = 0.6, width = 0.2)}
}

p <- map(c("maj_cat", "borough", "year", "month"), EDA_plot)
grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], top = "Exploratory Data Analysis")

crime_df$month <- factor(crime_df$month)

#figure out which vars matter with rpart
temp <- rpart(count ~ ., data = crime_df, cp = 0.002)

text_wrap <- function(x, labs, digits, varlen, faclen) {
  labs <- str_replace_all(labs, ",", " ")
  for (i in 1:length(labs)) {
    labs[i] <- str_c(str_wrap(labs[i], 20), "\n")
  }
  labs
}

prp(
  temp,
  extra = 101,
  box.palette = "#f9f5f1",
  type = 4,
  cex = 0.7,
  yspace = 4,
  nn = TRUE,
  nn.cex = 1.4,
  nn.box.col = "#CFB292",
  space = 2,
  split.font = 0.9,
  branch.lty = 2,
  faclen = 2,
  split.fun = text_wrap,
  main = "Which variables matter?"
)

#caret models

train_control <-
  trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    allowParallel = TRUE,
    seeds = c(1:51)
  )

theme_thinkr <- theme_economist() + theme(
  rect = element_rect(fill = "#f9f5f1"),
  plot.title = element_text(size = 12),
  plot.subtitle = element_text(size = 6),
  strip.text = element_text(size = 9),
  axis.text.x = element_text(size = 7),
  legend.text = element_text(size = 7),
  plot.background = element_rect(fill = "#f9f5f1")
)

model_GLM <-
  train(
    count ~ borough * maj_cat + .,
    data = crime_df,
    method = "glm",
    metric = "RMSE",
    family = "poisson",
    trControl = train_control
  )

model_GLM2 <- 
  train(
    count ~ .,
    data = crime_df,
    method = "glm",
    metric = "Rsquared",
    family = "poisson",
    trControl = train_control
  )

(compare <- data_frame(
  Statistic = c("R-squared", "RMSE"),
  Without_interaction = c(model_GLM2$results$Rsquared, model_GLM2$results$RMSE),
  With_interaction = c(model_GLM$results$Rsquared, model_GLM$results$RMSE)
))

tidy(model_GLM$finalModel) %>% 
  filter(str_detect(term, "month")) %>% 
  select(term, p.value)


gather_residuals(crime_df, model_GLM, .resid = "resid", .model = "model") %>%
  ggplot(aes(count, resid, colour = maj_cat)) +
  geom_point() +
  ggtitle("GLM residuals spread out at higher counts") +
  geom_hline(yintercept = 20, lty = 2, size = 1) +
  geom_abline(intercept = 80, slope = 0.15, colour = "grey80", size = 2, lty = 3) +
  geom_abline(intercept = -80, slope = -0.17, colour = "grey80", size = 2, lty = 3) +
  scale_colour_economist() +
  theme_thinkr +
  cap

#model rpart
tune_grid <-
  expand.grid(
    cp = 0.00001 # tried: 0.001, 0.0001
  )

model_RP <-
  train(
    count ~ ., # tried: interaction (negligible benefit)
    data = crime_df,
    method = "rpart",
    metric = "RMSE",
    parms = list(method = "poisson"),
    tuneGrid = tune_grid,
    trControl = train_control
  )

#random forest
tune_grid <-
  expand.grid(
    mtry = 18, # 54 variables divided by 3 (allowing for dummy variables)
    splitrule = "variance",
    min.node.size = 5
  )

model_RF <-
  train(
    count ~ ., # tried: interaction (negligible benefit)
    data = crime_df,
    method = "ranger",
    num.trees = 500,
    importance = "impurity",
    metric = "RMSE",
    respect.unordered.factors = TRUE,
    tuneGrid = tune_grid,
    trControl = train_control
  )

tune_grid <-
  expand.grid(
    mtry = 2, 
    splitrule = "variance",
    min.node.size = 5
  )

model_RF2 <-
  train(
    count ~ .,
    data = crime_df,
    method = "ranger",
    num.trees = 500,
    metric = "RMSE",
    respect.unordered.factors = TRUE,
    tuneGrid = tune_grid,
    trControl = train_control
  )
crime_df %>% 
  spread_predictions("Random Forest | mtry = 18" = model_RF,
                     "Random Forest | mtry = 02" = model_RF2) %>% 
  gather(key = model,
         value = pred,-year,-month,-borough,-maj_cat,-count) %>%
  rename(act = count) %>%
  ggplot(aes(pred, act, colour = maj_cat)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_abline(colour = "black", lty = 2) +
  facet_wrap( ~ model) +
  scale_colour_economist(name = "Major Category") +
  scale_y_continuous(breaks = seq(500, 3000, by = 500),
                     limits = c(0, 3000)) +
  scale_x_continuous(breaks = seq(500, 3000, by = 500),
                     limits = c(0, 3000)) +
  ggtitle("Accounting for the dummy variables when setting 'mtry'",
          subtitle = "The four categorical variables create 54 independent variables") +
  labs(x = "Predictions", y = "Actual") +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  theme_thinkr +
  cap

tune_grid <-
  expand.grid(
    interaction.depth = 10, # tried: 8, 9
    n.trees = 500,
    shrinkage = 0.1, # tried: 0.001, 0.01, 0.05
    n.minobsinnode = 5 # tried: 4
  )

model_SGB <-
  train(
    count ~ .,
    data = crime_df,
    distribution = "poisson",
    method = "gbm",
    metric = "RMSE",
    tuneGrid = tune_grid,
    verbose = FALSE,
    bag.fraction = 0.5,
    trControl = train_control
  )

tune_grid <-
  expand.grid(
    committees = 80, # tried 10, 40
    neighbors = 9 # tried 0, 5
  )

model_Cub <-
  train(count ~ .,
        data = crime_df,
        method = "cubist",
        metric = "RMSE",
        tuneGrid = tune_grid,
        trControl = train_control
  )

combined_pred <- crime_df %>%
  spread_predictions(
    "Generalized Linear Model" = model_GLM,
    "Random Forest" = model_RF,
    "Stochastic Gradient Boosting" = model_SGB,
    "CART" = model_RP,
    "Cubist" = model_Cub
  ) %>% 
  gather(key = model,
         value = pred,-year,-month,-borough,-maj_cat,-count) %>%
  rename(act = count)

pred_meas <- map_df(list(model_GLM, model_RF, model_SGB, model_RP, model_Cub), function(x) {
  tibble(
    model = x$modelInfo$label,
    method = x$method,
    model_type = x$modelType,
    rmse = rmse(x, crime_df),
    rsquare = rsquare(x, crime_df)
  )
})

ggplot(combined_pred, aes(pred, act, colour = maj_cat)) +
  geom_point(alpha = 0.3, size = 2) +
  geom_abline(colour = "black", lty = 2) +
  facet_wrap(~ model, nrow = 1) +
  geom_text(
    x = 1900,
    y = 250,
    aes(
      label = paste0(
        "Method = ",
        subset(method, model %in% combined_pred$model),
        "\n",
        "Type = ",
        subset(model_type, model %in% combined_pred$model),
        "\n",
        "RMSE = ",
        round(subset(rmse, model %in% combined_pred$model), 3),
        "\n",
        "R squared = ",
        round(subset(rsquare, model %in% combined_pred$model),
              3)
      )
    ),
    data = pred_meas,
    colour = "black",
    size = 3
  ) +
  scale_colour_economist(name = "Major Category") +
  scale_y_continuous(breaks = seq(500, 3000, by = 500),
                     limits = c(0, 3000)) +
  scale_x_continuous(breaks = seq(500, 3000, by = 500),
                     limits = c(0, 3000)) +
  ggtitle("Comparison of 5 Regression Models", subtitle = "Crime by category for each of London's 32 boroughs") +
  labs(x = "Predictions", y = "Actual") +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  theme_thinkr +
  cap

var_imp <- varImp(model_RF)$importance %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  top_n(10) %>%
  mutate(rowname = str_replace(rowname, "borough", "borough | "),
         rowname = str_replace(rowname, "maj_cat", "category | "),
         rowname = fct_inorder(rowname),
         hjust = if_else(Overall > 40, 1.1, -0.3),
         labcol = if_else(Overall > 40, "white", "black"))

ggplot(var_imp, aes(rowname, Overall)) +
  geom_col(fill = economist_pal()(1), width = 0.8) +
  geom_text(aes(label = paste0(rowname, "  ", round(Overall, 2))),
            hjust = var_imp$hjust, colour = var_imp$labcol, size = 3) +
  ggtitle("Top 10 Variables by Importance", subtitle = "Scaled to 100 -- Random Forest") +
  scale_y_log10() +
  coord_flip() +
  theme_economist() +
  theme(
    rect = element_rect(fill = "#f9f5f1"),
    plot.caption = element_text(colour = "grey60"),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#f9f5f1")
  ) +
  cap