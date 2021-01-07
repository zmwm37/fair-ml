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
accAll <- performance(predAll,'acc')
fnrAll <- performance(predAll, 'fnr')


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
  rocPerf <- performance(pred, 'tpr','fpr')
  acc <- performance(pred,'acc')
  tnr <- performance(pred,'tnr')
  fnr <- performance(pred,'fnr')
  
  
  
  # create dataframe of cutoffs, tpr, and fpr
  outPerf[[z]] <- data.frame(race = i,
                             cutoff  = perf@alpha.values[[1]],
                             tpr = rocPerf@y.values[[1]],
                             fpr = rocPerf@x.values[[1]],
                             acc = acc@y.values[[1]],
                             tnr = tnr@y.values[[1]],
                             fnr = fnr@y.values[[1]])
  
  # if not first curve, add to existing plot
  b <- if(z > 1) {
    TRUE
  } else {
    FALSE
  }
  
  # plot
  plot(rocPerf, add = b, col = lineColors[z])
  
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
ggplot(data = perfDf, aes(x = as.factor(cutoff), y = tpr, group = race, color = race)) + 
  geom_line(stat = 'identity')

# plot fpr comparing groups
ggplot(data = perfDf, aes(x = as.factor(cutoff), y = fpr, group = race, color = race)) + 
  geom_line(stat = 'identity')

# plot fnr comparing groups
ggplot(data = perfDf, aes(x = as.factor(cutoff), y = fnr, group = race, color = race)) + 
   geom_line(stat = 'identity')

# plot tnr comparing groups
ggplot(data = perfDf, aes(x = as.factor(cutoff), y = tnr, group = race, color = race)) + 
  geom_line(stat = 'identity')

