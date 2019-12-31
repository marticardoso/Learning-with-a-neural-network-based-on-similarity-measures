rm(list = ls())
set.seed(104)

# Set environment
setwd(".")


library(rattle.data)
library(faraway)
library(mlbench)
library(tree)
library(ggplot2)
library(xtable)
library(beepr)

#First we load some useful function for the model selection task
source('SNN.R')
source('tests/testSNNOptUtils.R')
source('loadDatasets.R')
source('benchmarkutils.R')

################
## Regression ##
################
set.seed(2)
automobile <- LoadAutomobileDS()
autoMPG <- LoadAutoMPGDS()
communities <- LoadCommunitiesDataset()
df <- runSNNOptTests(list(automobile, autoMPG), nRuns = 5)


#save(df, file = "tests/regressionAutomAutoMPGAndCommunities2.Rdata")
#load('data/RegData.Rdata')
#df <- list(fullResults = fullResults, shortResult = shortResult)
#fullResults$fullMethod <- paste(fullResults$clust.method, paste('P:', fullResults$method), ifelse(fullResults$reg,'Reg','-'), sep = '\n')
#fullResults[fullResults$method == 'tree',]$fullMethod <- 'Tree'

ggplot(df$fullResults[df$fullResults$dataset == 'AutoMPG',], aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() + ggtitle('AutoMPG dataset') + ylab('NRMSE') + xlab(NULL) #+ geom_jitter(shape = 16, position = position_jitter(0.2))


for (ds in c('Automobile', 'AutoMPG', 'Communities')) {
  plot <- ggplot(df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() + ggtitle(paste(ds, ' dataset', sep = '')) + ylab('NRMSE') + xlab(NULL) #+ geom_jitter(shape = 16, position = position_jitter(0.2))
  print(plot)
  ggsave(paste('Pictures/ExpSNN/Reg/Auto/Exp1_NRMSE_', ds, '.png', sep = ''))
}

# Time
for (ds in c('Automobile', 'AutoMPG', 'Communities')) {
  plot <- ggplot(data = df$shortResult[df$shortResult$dataset == ds,], aes(x = fullMethod, y = timeMean, group = dataset)) +
  geom_line() + geom_point() + ggtitle(paste('Mean execution time (',ds,' dataset)',sep = '')) +ylab('time (s)') + xlab('')
  print(plot)
  ggsave(paste('Pictures/ExpSNN/Reg/Auto/Exp1_Time2_',ds,'.png',sep = ''))
}


ggplot(data = df$shortResult, aes(x = fullMethod, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('NRMSE') + ylab('')


df$shortResults$fullMethod <- paste(df$shortResults$dataset, df$shortResults$reg)
ggplot(data = df$shortResults, aes(x = method, y = accMean, group = fullMethod, color = fullMethod)) +
  #geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1) +
  geom_line() +
  geom_point()

ggplot(data = df$shortResults, aes(x = method, y = timeMean, group = dataset, color = dataset)) +
  #geom_errorbar(aes(ymin = timeMean - timeSd, ymax = timeMean + timeSd), width = .1) +
  geom_line() +
  geom_point()

#############
## Classification binomial ##
#############

hc <- LoadBinClassProblems()
df <- runSNNOptTests(hc, nRuns = 3, classification = TRUE, onlyTree = TRUE)

heart <- LoadHeartDataset()
mammographic <- LoadMammographicDataset()
mushroom <- LoadMushroomDataset()

df <- runSNNOptTests(list(mushroom,heart), nRuns = 3, classification = TRUE)
soundEnd()
ggplot(df$fullResults[df$fullResults$dataset == 'Heart',], aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() #+ geom_jitter(shape = 16, position = position_jitter(0.2))

ggplot(df$fullResults[df$fullResults$dataset == 'Mammographic',], aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() #+ geom_jitter(shape = 16, position = position_jitter(0.2))

ggplot(data = df, aes(x = method, y = mean, group = dataset, color = dataset)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1) +
  geom_line() +
  geom_point()

ggplot(data = df, aes(x = method, y = timeMean, group = dataset, color = dataset)) +
  geom_errorbar(aes(ymin = timeMean - timeSd, ymax = timeMean + timeSd), width = .1) +
  geom_line() +
  geom_point()

xtable(df$shortResults)





