rm(list = ls())
set.seed(104)

library(rattle.data)
library(faraway)
library(mlbench)
library(tree)
library(ggplot2)
library(xtable)
library(beepr)



#First we load some useful function for the model selection task
source('SNN.R')
source('tests/testEnsSNNUtils.R')
source('loadDatasets.R')
source('benchmarkutils.R')

#Define folders where to save the results
exp2RegFolder <- 'Pictures/Experiments/Exp2/Reg/'
exp2MultiFolder <- 'Pictures/Experiments/Exp2/Multi/'
exp2BinFolder <- 'Pictures/Experiments/Exp2/Bin/'

################
## Regression ##
################

set.seed(2)
hc <- LoadRegressionProblems()
df <- runEnsSNNTests(hc, nRuns = 50, onlyRandomForest = FALSE)


dsNames <- levels(df$shortResults$dataset)
for (ds in dsNames) {
  tmp <- df$fullResults[df$fullResults$dataset == ds,]
  plot <- ggplot(tmp, aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() + ggtitle(paste(ds, ' dataset', sep = '')) + ylab('NRMSE') + xlab(NULL) + coord_cartesian(ylim = c(0.25, 0.5)) #+ geom_jitter(shape = 16, position = position_jitter(0.2))
  print(plot)
  ggsave(paste(exp2RegFolder, 'Exp2_NRMSE_', ds, '.png', sep = ''), width = 5, height = 3.5)
}

# Time
for (ds in dsNames) {
  plot <- ggplot(data = df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = time)) +
  geom_boxplot() + ggtitle(paste('Execution time (', ds, ' dataset)', sep = '')) + ylab('Execution time (s)') + xlab('')
  print(plot)
  ggsave(paste(exp2RegFolder, 'Exp2_Time_', ds, '.png', sep = ''), width = 5, height = 3.5)
}

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = accMean, group = dataset, color = dataset)) + ylab('NRMSE') + xlab(NULL) +
  geom_line() + geom_point()
print(p)
ggsave(paste(exp2RegFolder, 'Exp2_AccSummary.png', sep = ''))

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = timeMean, group = dataset, color = dataset)) +ylab('Execution time (s)') + xlab('') +
  geom_line() + geom_point()
print(p)
ggsave(paste(exp2RegFolder, 'Exp2_TimeSummary.png', sep = ''))

xtable(df$shortResults)



#############
## Binomial classification ##
#############

hc <- LoadBinClassProblems()
df <- runEnsSNNTests(hc, nRuns = 50, classification = TRUE, onlyRandomForest = FALSE)
#load('tests/exp2BinClassSmall.Rdata')

df$shortResults <- df$shortResults[df$shortResults$ensMethod != 'D',]
df$fullResults <- df$fullResults[df$fullResults$ensMethod != 'D',]


levels(df$shortResults$ensMethod)[5] <- 'C2'
levels(df$fullResults$ensMethod)[5] <- 'C2'
df$fullResults[df$fullResults$ensMethod == 'C2',]$fullMethod <- 'Ens: C2'
df$shortResults[df$shortResults$ensMethod == 'C2',]$fullMethod <- 'Ens: C2'

dsNames <- levels(df$shortResults$dataset)
for (ds in dsNames) {
  plot <- ggplot(df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() + ggtitle(paste(ds, ' dataset', sep = '')) + ylab('Accuracy (%)') + xlab(NULL) #+ geom_jitter(shape = 16, position = position_jitter(0.2))
  print(plot)
  ggsave(paste(exp2BinFolder, 'Exp2_Acc_', ds, '.png', sep = ''), width = 7, height = 4.5)
}

# Time
for (ds in dsNames) {
  plot <- ggplot(data = df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = time)) +
  geom_boxplot() + ggtitle(paste('Execution time (', ds, ' dataset)', sep = '')) + ylab('Execution time (s)') + xlab('')
  print(plot)
  ggsave(paste(exp2BinFolder, 'Exp2_Time_', ds, '.png', sep = ''), width = 7, height = 4.5)
}

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point()
print(p)
ggsave(paste(exp2BinFolder, 'Exp2_AccSummary.png', sep = ''), width = 7, height = 4.5)

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point()
print(p)
ggsave(paste(exp2BinFolder, 'Exp2_TimeSummary.png', sep = ''), width = 7, height = 4.5)

xtable(df$shortResults)


################################
## Multinomial classification ##
################################

hc <- LoadMultiClassProblems()
df <- runEnsSNNTests(hc, nRuns = 50, classification = TRUE, onlyRandomForest = FALSE)

#load('tests/exp2BinClassSmall.Rdata')

dsNames <- levels(df$shortResults$dataset)
for (ds in dsNames) {
  tmp <- df$fullResults[df$fullResults$dataset == ds,]
  plot <- ggplot(tmp, aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() + ggtitle(paste(ds, ' dataset', sep = '')) + ylab('Accuracy (%)') + xlab(NULL) #+ geom_jitter(shape = 16, position = position_jitter(0.2))
  print(plot)
  #ggsave(paste(exp2MultiFolder, 'Exp2_Acc_', ds, '.png', sep = ''), width = 7, height = 4.5)
}

# Time
for (ds in dsNames) {
  plot <- ggplot(df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = time)) +
  geom_boxplot() + ggtitle(paste('Execution time (', ds, ' dataset)', sep = '')) + ylab('Execution time (s)') + xlab('')
  print(plot)
  ggsave(paste(exp2MultiFolder, 'Exp2_Time_', ds, '.png', sep = ''), width = 7, height = 4.5)
}

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point()
print(p)
ggsave(paste(exp2MultiFolder, 'Exp2_AccSummary.png', sep = ''), width = 7, height = 4.5)

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point()
print(p)
ggsave(paste(exp2MultiFolder, 'Exp2_TimeSummary.png', sep = ''), width = 7, height = 4.5)

xtable(df$shortResults)





tmp <- df$shortResults
tmp$Learner <- ifelse(tmp$ensMethod == 'RandForest', 'Random Forest', 'Ens SNN')
levels(tmp$ensMethod)[8] <- ''
tmp$clust.method <- NULL
tmp$fullMethod <- NULL

tmp <- tmp[c("dataset", "Learner", "ensMethod", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)