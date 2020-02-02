# Run Experiment 2 (Tests the Ensemble of SNNs for several parameters).
rm(list = ls())
set.seed(104)

library(rattle.data)
library(faraway)
library(mlbench)
library(tree)
library(ggplot2)
library(xtable)
library(beepr)
source('SNN.R')
source('experiments/exp2testEnsSNNSource.R')
source('test/loadDatasets.R')
source('test/benchmarkutils.R')

#Define folders where to save the results
exp2RegFolder <- 'Pictures/'
exp2MultiFolder <- 'Pictures/'
exp2BinFolder <- 'Pictures/'

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
  geom_boxplot() + ggtitle(paste(ds, ' dataset', sep = '')) + ylab('NRMSE') + xlab(NULL) #+ coord_cartesian(ylim = c(0.25, 0.5))
  print(plot)
  #ggsave(paste(exp2RegFolder, 'Exp2_NRMSE_', ds, '.png', sep = ''), width = 5, height = 3.5)
}

# Time
for (ds in dsNames) {
  plot <- ggplot(data = df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = time)) +
  geom_boxplot() + ggtitle(paste('Execution time (', ds, ' dataset)', sep = '')) + ylab('Execution time (s)') + xlab(NULL)
  print(plot)
  #ggsave(paste(exp2RegFolder, 'Exp2_Time_', ds, '.png', sep = ''), width = 5, height = 3.5)
}

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = accMean, group = dataset, color = dataset)) + ylab('NRMSE') + xlab(NULL) +
  geom_line() + geom_point()
print(p)
#ggsave(paste(exp2RegFolder, 'Exp2_AccSummary.png', sep = ''))

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = timeMean, group = dataset, color = dataset)) +ylab('Execution time (s)') + xlab(NULL) +
  geom_line() + geom_point()
print(p)
#ggsave(paste(exp2RegFolder, 'Exp2_TimeSummary.png', sep = ''))


#############
## Binomial classification ##
#############

hc <- LoadBinClassProblems()
df <- runEnsSNNTests(hc, nRuns = 50, classification = TRUE, onlyRandomForest = FALSE)
#save(df, file ='experiments/exp2BinClass.Rdata')
#load('experiments/exp2BinClass.Rdata')

# Rename E -> C2
#levels(df$shortResults$ensMethod)[6] <- 'C2'
#levels(df$fullResults$ensMethod)[6] <- 'C2'
#df$fullResults[df$fullResults$ensMethod == 'C2',]$fullMethod <- 'Ens: C2'
#df$shortResults[df$shortResults$ensMethod == 'C2',]$fullMethod <- 'Ens: C2'

dsNames <- levels(df$shortResults$dataset)
for (ds in dsNames) {
  plot <- ggplot(df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() + ggtitle(paste(ds, ' dataset', sep = '')) + ylab('Accuracy (%)') + xlab(NULL) + coord_cartesian(ylim = c(99.5, 100))
  print(plot)
  ggsave(paste(exp2BinFolder, 'Exp2_Acc_', ds, '2.png', sep = ''), width = 5, height = 3.5)
}

# Time
for (ds in dsNames) {
  plot <- ggplot(data = df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = time)) +
  geom_boxplot() + ggtitle(paste('Execution time (', ds, ' dataset)', sep = '')) + ylab('Execution time (s)') + xlab(NULL)
  print(plot)
  ggsave(paste(exp2BinFolder, 'Exp2_Time_', ds, '2.png', sep = ''), width = 5, height = 3.5)
}

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point()
print(p)
#ggsave(paste(exp2BinFolder, 'Exp2_AccSummary.png', sep = ''), width = 7, height = 4.5)

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point()
print(p)
#ggsave(paste(exp2BinFolder, 'Exp2_TimeSummary.png', sep = ''), width = 7, height = 4.5)

################################
## Multinomial classification ##
################################

hc <- LoadMultiClassProblems()
df <- runEnsSNNTests(hc, nRuns = 50, classification = TRUE, onlyRandomForest = FALSE)
#source(df, file='experiments/exp2MultiClassDF.Rdata')
#load('experiments/exp2MultiClassDF.Rdata')

dsNames <- levels(df$shortResults$dataset)
for (ds in dsNames) {
  tmp <- df$fullResults[df$fullResults$dataset == ds,]
  plot <- ggplot(tmp, aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() + ggtitle(paste(ds, ' dataset', sep = '')) + ylab('Accuracy (%)') + xlab(NULL) 
  print(plot)
  #ggsave(paste(exp2MultiFolder, 'Exp2_Acc_', ds, '.png', sep = ''), width = 5, height = 3.5)
}

# Time
for (ds in dsNames) {
  plot <- ggplot(df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = time)) +
  geom_boxplot() + ggtitle(paste('Execution time (', ds, ' dataset)', sep = '')) + ylab('Execution time (s)') + xlab(NULL)
  print(plot)
  #ggsave(paste(exp2MultiFolder, 'Exp2_Time_', ds, '.png', sep = ''), width = 5, height = 3.5)
}

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point()
print(p)
#ggsave(paste(exp2MultiFolder, 'Exp2_AccSummary.png', sep = ''), width = 7, height = 4.5)

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point()
print(p)
#ggsave(paste(exp2MultiFolder, 'Exp2_TimeSummary.png', sep = ''), width = 7, height = 4.5)

# Print results as latex table
tmp <- df$shortResults
tmp$Learner <- ifelse(tmp$ensMethod == 'RandForest', 'Random Forest', 'Ens SNN')
levels(tmp$ensMethod)[8] <- ''
tmp$clust.method <- NULL
tmp$fullMethod <- NULL
tmp <- tmp[c("dataset", "Learner", "ensMethod", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)

