library(tidyverse)

glimpse(msleep)


###########################
#SELECTNG COLUMNS
###########################

#selecting columns 
msleep %>%
  select(name, genus, sleep_total, awake) %>%
  glimpse()

#selecting a chunk of columns
msleep %>%
  select(name:order, sleep_total:sleep_cycle) %>%
  glimpse()

#deselect a chunk of columns
msleep %>% 
  select(-conservation, -(sleep_total:awake)) %>%
  glimpse()

#deselect a chunk and then add part back in
msleep %>%
  select(-(name:awake), conservation) %>%
  glimpse()

#create vector of names and refer to it in select statement
classification_info <- c("name", "genus", "vore", "order", "conservation")
sleep_cols <- c("sleep_total", "sleep_rem", "sleep_cycle")
weight_cols <- c("brainwt", "bodywt")

msleep %>%
  select(one_of(sleep_cols))

#select columns that "start with" or "ends with" or "contains" characters - easy grep 
msleep %>%
  select(name, starts_with("sleep")) %>%
  glimpse()

msleep %>%
  select(contains("eep"), ends_with("wt")) %>%
  glimpse()

#selecting based on regex
msleep %>%
  select(matches("o.+er")) %>%
  glimpse()

#selecting columns by their data type
msleep %>%
  select_if(is.numeric) %>%
  glimpse()

#select by negation but you need ~
msleep %>%
  select_if(~!is.numeric(.)) %>%
  glimpse()

#using select_if based on logical function
msleep %>%
  select_if(is.numeric) %>%
  select_if(~mean(., na.rm=TRUE) > 10)
msleep %>%
  select_if(~is.numeric(.) & mean(., na.rm=TRUE) > 10)

#is.distinct allows you to pick cols with a certain number of unique values 
msleep %>%
  select_if(~n_distinct(.) < 10) %>%
  glimpse()

#you can reorder variables by the order you select them
msleep %>%
  select(conservation, sleep_total, name) %>%
  glimpse()

#everything basically means everything else
msleep %>%
  select(conservation, sleep_total, everything()) %>%
  glimpse()

#can rename variables right in the select statement
msleep %>%
  select(animal = name, sleep_total, extinction_threat = conservation) %>%
  glimpse()

#if you want all columns but rename, just don't use the select statement
msleep %>% 
  rename(animal = name, extinction_threat = conservation) %>%
  glimpse()

#select_all allows changes to all columns - toupper or tolower for example
msleep %>%
  select_all(toupper)

#can quickly clean names from excel for example

#making an unclean database:
msleep2 <- select(msleep, name, sleep_total, brainwt)
colnames(msleep2) <- c("name", "sleep total", "brain weight")

msleep2 %>%
  select_all(~str_replace(., " ", "_"))

#can change row names to an actual column
mtcars %>%
  rownames_to_column("car_model") %>%
  head()

###########################
#Mutating 
###########################

#create new variable out of old
msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_min = sleep_total * 60)

msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_vs_AVG = sleep_total - round(mean(sleep_total), 1),
         sleep_total_vs_MIN = sleep_total - min(sleep_total))

#if_else is super handy - if function like
msleep %>%
  select(name, brainwt) %>%
  mutate(brainwt2 = ifelse(brainwt > 4, NA, brainwt)) %>%
  arrange(desc(brainwt))

#return last word of text and make it lower case
msleep %>%
  select(name) %>%
  mutate(name_last_word = tolower(str_extract(name, pattern = "\\w+$")))

#mutate_all does something to your entire database
msleep %>%
  mutate_all(tolower)

#first screw up the data
msleep_ohno <- msleep %>%
  mutate_all(~paste(., "  /n  "))
msleep_ohno[,1:4]

#removes all white space and /n
msleep_corr <- msleep_ohno %>%
  mutate_all(~str_replace_all(., "/n", "")) %>%
  mutate_all(str_trim)
msleep_corr[,1:4]

#regex replacing all vowels
msleep %>%
  select(name:sleep_total) %>%
  mutate_all(~str_replace_all(., "[aeiou]", ""))

#mutate_if 
msleep %>%
  select(name, sleep_total:bodywt) %>%
  mutate_if(is.numeric, round)

msleep %>%
  select(name, sleep_total:bodywt) %>%
  mutate_if(is.numeric, ~round(. ,2))

#mutate_at - mutate just certain vars - can use any of the select helpers
msleep %>%
  select(name, sleep_total:awake) %>%
  mutate_at(vars(contains("sleep")), ~(.*60)) 

#recode character variable and then count the results
msleep %>%
  mutate(conservation2 = recode(conservation,
                                "en" = "Endangered",
                                "lc" = "Least_Concern",
                                "domesticated" = "Least_Concern",
                                .default = "other")) %>%
  count(conservation2)

#recoding a factor variable
msleep %>%
  mutate(conservation2 = recode_factor(conservation,
                                       "en" = "Endangered",
                                       "lc" = "Least_Concern",
                                       "domesticated" = "Least_Concern",
                                       .default = "other",
                                       .missing = "no data",
                                       .ordered = TRUE)) %>%
  count(conservation2)

#creating a dummy variable with character values 
msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_time = ifelse(sleep_total > 10, "long", "short")) 

#creating new var with multiple character levels 
#it evaluates n order so only the falses move on to the next level
msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_discr = case_when(
    sleep_total > 13 ~ "very long",
    sleep_total > 10 ~ "long",
    sleep_total > 7 ~ "limited",
    TRUE ~ "short")) %>%
  mutate(sleep_total_discr = factor(sleep_total_discr, 
                                    levels = c("short", "limited", 
                                               "long", "very long")))
msleep %>%
  mutate(silly_groups = case_when(
    brainwt < 0.001 ~ "light_headed",
    sleep_total > 10 ~ "lazy_sleeper",
    is.na(sleep_rem) ~ "absent_rem",
    TRUE ~ "other")) %>%
  count(silly_groups)

#splitting and merging columns #these examples don't work as I don't have this data
#this separates a variable that she pulled in that looks like:
#  EX = Extinct
#  EW = Extinct in the wild

#splits a variable
(conservation_table <- conservation_expl %>%
    separate(`conservation abbreviation`, 
             into = c("abbreviation", "description"), sep = " = "))

#unites two variables
conservation_table %>%
  unite(united_col, abbreviation, description, sep=": ")

#merging variables from another data table
msleep %>%
  select(name, conservation) %>%
  mutate(conservation = toupper(conservation)) %>%
  left_join(conservation_table, by = c("conservation" = "abbreviation")) %>%
  mutate(description = ifelse(is.na(description), conservation, description))

#spreading and gathering variables
msleep %>%
  select(name, contains("sleep")) %>%
  gather(key = "sleep_measure", value = "time", -name)

#same as above but key is an ordered factor
(msleep_g <- msleep %>%
    select(name, contains("sleep")) %>%
    gather(key = "sleep_measure", value = "time", -name, factor_key = TRUE))

#spread is the flip of gather
msleep_g %>%
  spread(sleep_measure, time)

#turning value into NA
msleep %>%
  select(name:order) %>%
  na_if("omni")

###########################
#filtering
###########################

#basic filter
msleep %>% 
  select(name, sleep_total) %>% 
  filter(sleep_total > 18)

#between
msleep %>% 
  select(name, sleep_total) %>% 
  filter(between(sleep_total, 16, 18))

#filter within 1 SD
msleep %>% 
  select(name, sleep_total) %>% 
  filter(near(sleep_total, 17, tol = sd(sleep_total)))

#exact character matches
msleep %>% 
  select(order, name, sleep_total) %>% 
  filter(order == "Didelphimorphia")

msleep %>% 
  select(order, name, sleep_total) %>% 
  filter(!order == "Didelphimorphia")


msleep %>% 
  select(order, name, sleep_total) %>% 
  filter(order > "v")

msleep %>% 
  select(order, name, sleep_total) %>% 
  filter(order %in% c("Didelphimorphia", "Diprotodontia"))

remove <- c("Rodentia", "Carnivora", "Primates")
msleep %>% 
  select(order, name, sleep_total) %>% 
  filter(!order %in% remove)

#filter using regex
msleep %>% 
  select(name, sleep_total) %>% 
  filter(str_detect(tolower(name), pattern = "mouse"))

#filter with multiple conditions
msleep %>% 
  select(name, order, sleep_total:bodywt) %>% 
  filter(bodywt > 100, (sleep_total > 15 | order != "Carnivora"))

msleep %>%
  select(name, bodywt:brainwt) %>% 
  filter(xor(bodywt > 100, brainwt > 1))

msleep %>% 
  select(name, sleep_total, brainwt, bodywt) %>% 
  filter(brainwt > 1, !bodywt > 100)

#filter out NA
msleep %>% 
  select(name, conservation:sleep_cycle) %>% 
  filter(!is.na(conservation))

#filter_all - filtering across multiple columns
msleep %>% 
  select(name:order, sleep_total, -vore) %>% 
  filter_all(any_vars(str_detect(., pattern = "Ca")))
msleep %>%  
  select(name, sleep_total:bodywt) %>% 
  filter_all(any_vars(. < 0.1))
msleep %>%  
  select(name, sleep_total:bodywt, -awake) %>% 
  filter_all(all_vars(. > 1))

#filter_if - filtering across multiple columns
msleep %>% 
  select(name:order, sleep_total:sleep_rem) %>% 
  filter_if(is.character, any_vars(is.na(.)))

#filter_at - filtering across multiple columns
msleep %>% 
  select(name, sleep_total:sleep_rem, brainwt:bodywt) %>% 
  filter_at(vars(sleep_total, sleep_rem), all_vars(. > 5))
msleep %>% 
  select(name, sleep_total:sleep_rem, brainwt:bodywt) %>% 
  filter_at(vars(contains("sleep")), all_vars(. > 5))

###########################
#Summarizing and slicing
###########################

#counting the number of observations
msleep %>%
  count(vore, sort = TRUE)

#counting the number of observations with multiple vars
msleep %>%
  count(order, vore, sort = TRUE)

#count total number of observations in the file
msleep %>%
  tally()

#add tally variable to the file
msleep %>%
  select(1:3) %>%
  add_tally()

#add count variable to the file 
msleep %>%
  select(name:vore) %>%
  add_count(vore)

#summaries 
msleep %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

#summaries by group
msleep %>%
  group_by(vore) %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

msleep %>%
  group_by(vore) %>%
  summarise(avg_sleep_day = mean(sleep_total)/24)

#summarize all
msleep %>%
  group_by(vore) %>%
  summarise_all(mean, na.rm = TRUE)

#summarize all with a function
msleep %>%
  group_by(vore) %>%
  summarise_all(~mean(., na.rm = TRUE) + 5)

#summarize if with a function
msleep %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

msleep %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  rename_if(is.numeric, ~paste0("avg_", .))

msleep %>%
  group_by(vore) %>%
  summarise_at(vars(contains("sleep")), mean, na.rm = TRUE) %>%
  rename_at(vars(contains("sleep")), ~paste0("avg_", .))

#arranging rows
msleep %>%
  group_by(vore) %>%
  summarise(avg_sleep = mean(sleep_total)) %>%
  arrange(desc(avg_sleep))

msleep %>%
  select(order, name, sleep_total) %>%
  group_by(order) %>%
  arrange(desc(sleep_total), .by_group = TRUE)

#showing just some of your data in the summary - top 5
msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(5)

#lowest 5
msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(-5)

#more than 1 variable
msleep %>%
  group_by(order) %>%
  summarise(average_sleep = mean(sleep_total), max_sleep = max(sleep_total)) %>%
  top_n(5, average_sleep)

#sample data
msleep %>%
  sample_frac(.1)

#user defined slice
msleep %>%
  slice(50:55)
