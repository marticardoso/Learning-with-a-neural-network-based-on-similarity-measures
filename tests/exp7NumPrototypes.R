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
source('tests/exp7NumPrototypesSource.R')
source('loadDatasets.R')
source('benchmarkutils.R')

#Output folder
exp7RegFolder <- 'Pictures/Experiments/Exp7/Reg/'
exp7MultiFolder <- 'Pictures/Experiments/Exp7/Multi/'
exp7BinFolder <- 'Pictures/Experiments/Exp7/Bin/'

################
## Regression ##
################

set.seed(2)
regDs <- LoadRegressionProblems(large = FALSE)
df <- runExperiment7(regDs, nRuns = 10)
save(df, file = "tests/Exp7/Exp7RegSmall.Rdata")
#load("tests/Exp7/Exp7RegSmall.Rdata")
ggplot(data = df$shortResult, aes(x = propPrototypes * 100, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('NRMSE') + xlab('Percentage of prototypes (%)') + ggtitle('Regression problems') + xlim(0, 100) + coord_cartesian(ylim = c(0, 1))
ggsave(paste(exp7RegFolder, 'Exp7_NRMSE_Regression.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = propPrototypes * 100, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Execution time (s)') + xlab('Percentage of prototypes (%)') + ggtitle('Regression problems') + xlim(0, 100)
ggsave(paste(exp7RegFolder, 'Exp7_Time_Regression.png', sep = ''), width = 5, height = 3.5)



#############
## Classification binomial ##
#############

hc <- LoadBinClassProblems(large = FALSE)
df <- runExperiment7(hc, nRuns = 10, classification = TRUE)
save(df, file = "tests/Exp7/Exp7BinClassSmall.Rdata")
#load("tests/Exp7/Exp7BinClassSmall.Rdata")
ggplot(data = df$shortResult, aes(x = propPrototypes * 100, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Accuracy (%)') + xlab('Percentage of prototypes (%)') + ggtitle('Binomial classification problems') + xlim(0, 100)
ggsave(paste(exp7BinFolder, 'Exp7_Acc_BinClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = propPrototypes * 100, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Execution time (s)') + xlab('Percentage of prototypes (%)') + ggtitle('Binomial classification problems') + xlim(0, 100)
ggsave(paste(exp7BinFolder, 'Exp7_Time_BinClass.png', sep = ''), width = 5, height = 3.5)

#############
## Multinomial Classification ##
#############

hc <- LoadMultiClassProblems(large = FALSE)
df <- runExperiment7(hc, nRuns = 10, classification = TRUE)
save(df, file = "tests/Exp7/Exp7MultiClassSmall.Rdata")
#load("tests/Exp7/Exp7MultiClassSmall.Rdata")

ggplot(data = df$shortResult, aes(x = propPrototypes * 100, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Accuracy (%)') + xlab('Percentage of prototypes (%)') + ggtitle('Multinomial classification problems') + xlim(0, 100)
ggsave(paste(exp7MultiFolder, 'Exp7_Acc_MultiClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = propPrototypes * 100, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Execution time (s)') + xlab('Percentage of prototypes (%)') + ggtitle('Multinomial classification problems') + xlim(0, 100)
ggsave(paste(exp7MultiFolder, 'Exp7_Time_MultiClass.png', sep = ''), width = 5, height = 3.5)



# Export to latex
tmp <- df$shortResults
tmp$Prop <- paste(tmp$propPrototypes * 100, '%', sep = '')
tmp$propPrototypes <- NULL
tmp$clust.method <- NULL

tmp <- tmp[c("dataset", "Prop", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)






