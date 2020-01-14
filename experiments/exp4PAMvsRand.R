# Run Experiment 4 (Tests the clustering method of each SNN of an Ensemble).
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
source('experiments/exp4PAMvsRandSource.R')
source('test/loadDatasets.R')
source('test/benchmarkutils.R')

#Output folder
exp4RegFolder <- 'Pictures/Experiments/Exp4/Reg/'
exp4MultiFolder <- 'Pictures/Experiments/Exp4/Multi/'
exp4BinFolder <- 'Pictures/Experiments/Exp4/Bin/'

################
## Regression ##
################

set.seed(2)
regDs <- LoadRegressionProblems(large = FALSE)
df <- runExperiment4(regDs, nRuns = 20)
#save(df, file = "experiments/Exp4/Exp4Reg.Rdata")
#load("experiments/Exp4/Exp4Reg.Rdata")
ggplot(data = df$shortResult, aes(x = clust.method, y = accMean, group = dataset, color = dataset)) +
  geom_point(size= 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('NRMSE') + xlab('Clustering method') + ggtitle('Regression problems')
#ggsave(paste(exp4RegFolder, 'Exp4_NRMSE_Regression.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = clust.method, y = timeMean, group = dataset, color = dataset)) +
  geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Clustering method') + ggtitle('Regression problems')
#ggsave(paste(exp4RegFolder, 'Exp4_Time_Regression.png', sep = ''), width = 5, height = 3.5)



#############
## Classification binomial ##
#############

hc <- LoadBinClassProblems(large = FALSE)
df <- runExperiment4(hc, nRuns = 20, classification = TRUE)
#save(df, file = "experiments/Exp4/Exp4BinClass.Rdata")
#load("experiments/Exp4/Exp4BinClass.Rdata")
ggplot(data = df$shortResult, aes(x = clust.method, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Clustering method') + ggtitle('Binomial classification problems')
#ggsave(paste(exp4BinFolder, 'Exp4_Acc_BinClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = clust.method, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Clustering method') + ggtitle('Binomial classification problems')
#ggsave(paste(exp4BinFolder, 'Exp4_Time_BinClass.png', sep = ''), width = 5, height = 3.5)

#############
## Multinomial Classification ##
#############

hc <- LoadMultiClassProblems(large = FALSE)
df <- runExperiment4(hc, nRuns = 10, classification = TRUE)
#save(df, file = "experiments/Exp4/Exp4MultiClass.Rdata")
#load("experiments/Exp4/Exp4MultiClass.Rdata")

ggplot(data = df$shortResult, aes(x = clust.method, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Clustering method') + ggtitle('Multinomial classification problems')
#ggsave(paste(exp4MultiFolder, 'Exp4_Acc_MultiClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = clust.method, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Clustering method') + ggtitle('Multinomial classification problems')
#ggsave(paste(exp4MultiFolder, 'Exp4_Time_MultiClass.png', sep = ''), width = 5, height = 3.5)



# Print results as latex table
tmp <- df$shortResults
tmp <- tmp[c("dataset", "clust.method", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)






