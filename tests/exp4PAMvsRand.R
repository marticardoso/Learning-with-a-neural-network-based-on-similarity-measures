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
source('tests/exp4PAMvsRandSource.R')
source('loadDatasets.R')
source('benchmarkutils.R')

#Output folder
exp4RegFolder <- 'Pictures/Experiments/Exp4/Reg/'
exp4MultiFolder <- 'Pictures/Experiments/Exp4/Multi/'
exp4BinFolder <- 'Pictures/Experiments/Exp4/Bin/'

################
## Regression ##
################

set.seed(2)
regDs <- LoadRegressionProblems(large = FALSE)
df <- runExperiment4(regDs, nRuns = 10)
save(df, file = "tests/Exp4/Exp4RegSmall.Rdata")
#load("tests/Exp4/Exp4RegSmall.Rdata")
ggplot(data = df$shortResult, aes(x = clust.method, y = accMean, group = dataset, color = dataset)) +
  geom_point(size= 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('NRMSE') + xlab('Clustering method') + ggtitle('Regression problems')
ggsave(paste(exp4RegFolder, 'Exp4_NRMSE_Regression.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = clust.method, y = timeMean, group = dataset, color = dataset)) +
  geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Clustering method') + ggtitle('Regression problems')
ggsave(paste(exp4RegFolder, 'Exp4_Time_Regression.png', sep = ''), width = 5, height = 3.5)



#############
## Classification binomial ##
#############

hc <- LoadBinClassProblems(large = FALSE)
df <- runExperiment4(hc, nRuns = 10, classification = TRUE)
save(df, file = "tests/Exp4/Exp4BinClassSmall.Rdata")
#load("tests/Exp4/Exp4BinClassSmall.Rdata")
ggplot(data = df$shortResult, aes(x = clust.method, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Clustering method') + ggtitle('Binomial classification problems')
ggsave(paste(exp4BinFolder, 'Exp4_Acc_BinClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = clust.method, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Clustering method') + ggtitle('Binomial classification problems')
ggsave(paste(exp4BinFolder, 'Exp4_Time_BinClass.png', sep = ''), width = 5, height = 3.5)

#############
## Multinomial Classification ##
#############

hc <- LoadMultiClassProblems(large = FALSE)
df <- runExperiment4(hc, nRuns = 10, classification = TRUE)
save(df, file = "tests/Exp4/Exp4MultiClassSmall.Rdata")
#load("tests/Exp4/Exp4MultiClassSmall.Rdata")

ggplot(data = df$shortResult, aes(x = clust.method, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Clustering method') + ggtitle('Multinomial classification problems')
ggsave(paste(exp4MultiFolder, 'Exp4_Acc_MultiClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = clust.method, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Clustering method') + ggtitle('Multinomial classification problems')
ggsave(paste(exp4MultiFolder, 'Exp4_Time_MultiClass.png', sep = ''), width = 5, height = 3.5)



# Export to latex
tmp <- df$shortResults

tmp <- tmp[c("dataset", "clust.method", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)






