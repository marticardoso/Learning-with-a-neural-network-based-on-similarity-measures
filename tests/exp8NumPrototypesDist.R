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
source('tests/exp8NumPrototypesDistSource.R')
source('loadDatasets.R')
source('benchmarkutils.R')

#Output folder
exp8RegFolder <- 'Pictures/Experiments/Exp8/Reg/'
exp8MultiFolder <- 'Pictures/Experiments/Exp8/Multi/'
exp8BinFolder <- 'Pictures/Experiments/Exp8/Bin/'

################
## Regression ##
################

set.seed(2)
regDs <- LoadRegressionProblems(large = FALSE)
df <- runExperiment8(regDs, nRuns = 20)
save(df, file = "tests/Exp8/Exp8RegSmall.Rdata")
#load("tests/Exp8/Exp8RegSmall.Rdata")
ggplot(data = df$shortResult, aes(x = nclust.method, y = accMean, group = dataset, color = dataset)) +
  geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('NRMSE') + xlab('Method to select the number of prototypes') + ggtitle('Regression problems')
ggsave(paste(exp8RegFolder, 'Exp8_NRMSE_Regression.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = nclust.method, y = timeMean, group = dataset, color = dataset)) +
  geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Method to select the number of prototypes') + ggtitle('Regression problems')
ggsave(paste(exp8RegFolder, 'Exp8_Time_Regression.png', sep = ''), width = 5, height = 3.5)



#############
## Classification binomial ##
#############

hc <- LoadBinClassProblems(large = FALSE)
df <- runExperiment8(hc, nRuns = 10, classification = TRUE)
save(df, file = "tests/Exp8/Exp8BinClassSmall.Rdata")
#load("tests/Exp8/Exp8BinClassSmall.Rdata")
ggplot(data = df$shortResult, aes(x = nclust.method, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Method to select the number of prototypes') + ggtitle('Binomial classification problems')
ggsave(paste(exp8BinFolder, 'Exp8_Acc_BinClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = nclust.method, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Method to select the number of prototypes') + ggtitle('Binomial classification problems')
ggsave(paste(exp8BinFolder, 'Exp8_Time_BinClass.png', sep = ''), width = 5, height = 3.5)

#############
## Multinomial Classification ##
#############

hc <- LoadMultiClassProblems(large = FALSE)
df <- runExperiment8(hc, nRuns = 10, classification = TRUE)
save(df, file = "tests/Exp8/Exp8MultiClassSmall.Rdata")
#load("tests/Exp8/Exp8MultiClassSmall.Rdata")

ggplot(data = df$shortResult, aes(x = nclust.method, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Method to select the number of prototypes') + ggtitle('Multinomial classification problems')
ggsave(paste(exp8MultiFolder, 'Exp8_Acc_MultiClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = nclust.method, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Method to select the number of prototypes') + ggtitle('Multinomial classification problems')
ggsave(paste(exp8MultiFolder, 'Exp8_Time_MultiClass.png', sep = ''), width = 5, height = 3.5)



# Export to latex
tmp <- df$shortResults

tmp <- tmp[c("dataset", "nclust.method", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)


regDs <- LoadRegressionProblems(large = FALSE)
df <- runExperiment8(regDs, nRuns = 20)
save(df, file = "tests/Exp8/Exp8RegSmall.Rdata")
df2 <- df
hc <- LoadBinClassProblems(large = FALSE)
df <- runExperiment8(hc, nRuns = 20, classification = TRUE)
save(df, file = "tests/Exp8/Exp8BinClassSmall.Rdata")
df3 <- df
hc <- LoadMultiClassProblems(large = FALSE)
df <- runExperiment8(hc, nRuns = 20, classification = TRUE)
save(df, file = "tests/Exp8/Exp8MultiClassSmall.Rdata")
df4 <- df
