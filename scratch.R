library(tidyverse)
library(ROCR)

scores <- read.csv('../compas-analysis/compas-scores-two-years.csv')
View(scores)

# filter to white and Black only
scoresWB <- scores %>% 
  filter(race %in% c('African-American', 'Caucasian'))

# roc curve for all

# create prediciton object
predAll <- prediction(scoresWB$decile_score,scoresWB$is_recid)
perfAll <- performance(predAll, 'tpr','fpr')
aucAll <- performance(predAll,'auc')


# roc curve
plot(perfAll)
aucAll@y.values[[1]]

# roc curve by race

# groups 
g <- unique(as.character(scoresWB$race))
lineColors <- c('red','blue')

# list for group values
outPerf <- list()

z <- 1
for(i in g) {
  # filter 
  s <- scoresWB %>% filter(race == i)
  
  # create prediction/performance
  pred <- prediction(s$decile_score, s$is_recid)
  perf <- performance(pred, 'tpr','fpr')
  
  # create dataframe of cutoffs, tpr, and fpr
  outPerf[[z]] <- data.frame(race = i,
                             cutoff  = perf@alpha.values[[1]],
                             tpr = perf@y.values[[1]],
                             fpr = perf@x.values[[1]])
  
  # if not first curve, add to existing plot
  b <- if(z > 1) {
    TRUE
  } else {
    FALSE
  }
  
  # plot
  plot(perf, add = b, col = lineColors[z])
  
  if (z == 1) {
    legend('bottomright',
           legend = g,
           col = lineColors,
           lty =1)
  }
  
  z <- z + 1
} 

# bind group dataframes 
perfDf <- do.call(rbind,outPerf)

# plot tpr and fpr comparing groups
ggplot(data = perfDf, aes(x = as.factor(cutoff), y = tpr, fill = race)) + 
  geom_bar(position = 'dodge', stat = 'identity')

# plot tpr and fpr comparing groups
ggplot(data = perfDf, aes(x = as.factor(cutoff), y = fpr, fill = race)) + 
  geom_bar(position = 'dodge', stat = 'identity')
