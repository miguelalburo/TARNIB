rm(list = ls())

library(tidyverse)

########## SECTION 1 ##########

train <- read_csv(file = "data/train.csv") 
test <- read_csv(file = "data/test.csv")
wide <- rbind(train,test)


wide %>% select(LoS, days_since_discharge) %>%
  na.omit() %>%
  ggplot(aes(y = LoS, x = days_since_discharge)) +
  geom_point() +
  theme_bw()


wide %>% select(LoS, ER_LoS) %>%
  na.omit() %>%
  ggplot(aes(y = LoS, x = ER_LoS)) +
  geom_point() +
  theme_bw()
