library(tidyverse)
library(ROCR)

scores <- read.csv('../compas-analysis/compas-scores-two-years.csv')
View(scores)

# filter to white and Black only
scoresWB <- scores %>% 
  filter(race %in% c('African-American', 'Caucasian'))

# roc curve for all


# roc curve by race

