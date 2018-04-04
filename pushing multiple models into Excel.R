library(tidyverse)
library(broom)
library(writexl)
library(descr)

mtcars %>% 
  select(disp:wt) %>%
  map(~ tidy(lm(.x ~ as.factor(cyl) * as.factor(am), data = mtcars))) %>% 
  writexl::write_xlsx(., "mtcars_example2.xlsx")

mtcars %>% 
    map(~freq(.x, plot = F))