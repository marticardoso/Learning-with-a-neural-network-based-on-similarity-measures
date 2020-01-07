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
source('tests/exp5MissingsSource.R')
source('loadDatasets.R')
source('benchmarkutils.R')

exp5RegFolder <- 'Pictures/Experiments/Exp5/Reg/'
exp5MultiFolder <- 'Pictures/Experiments/Exp5/Multi/'
exp5BinFolder <- 'Pictures/Experiments/Exp5/Bin/'

################
## Regression ##
################

changeFirstByLastCol <- function(ds) {
  ds$dataset = ds$dataset[, c(colnames(ds$dataset)[2:length(colnames(ds$dataset))], colnames(ds$dataset)[1])]
  ds
}

set.seed(2)
hc <- LoadRegressionProblems(large = FALSE)
hc[[2]] <- changeFirstByLastCol(hc[[2]])
df <- runExperiment5(hc, nRuns = 10, targetFirstPosition = FALSE)
save(df, file = "tests/Exp5/Exp5RegSmall.Rdata")
#load("tests/Exp5/Exp5RegSmall.Rdata")
ggplot(data = df$shortResult, aes(x = propMissings * 100, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('NRMSE') + xlab('Percentage of missing values (%)') + ggtitle('Regression problems') + xlim(0,100)
ggsave(paste(exp5RegFolder, 'Exp5_NRMSE_Regression.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = propMissings * 100, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Execution time (s)') + xlab('Percentage of missing values (%)') + ggtitle('Regression problems') + xlim(0, 100)
ggsave(paste(exp5RegFolder, 'Exp5_Time_Regression.png', sep = ''), width = 5, height = 3.5)



#############
## Classification binomial ##
#############

hc <- LoadBinClassProblems(large = FALSE)
df <- runExperiment5(hc, nRuns = 10, classification = TRUE, targetFirstPosition = FALSE)
#save(df, file = "tests/Exp5/Exp5BinClassSmall.Rdata")
#load("tests/Exp5/Exp5BinClassSmall.Rdata")
ggplot(data = df$shortResult, aes(x = propMissings * 100, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Accuracy (%)') + xlab('Percentage of missing values (%)') + ggtitle('Binomial classification problems') + xlim(0, 100)
ggsave(paste(exp5BinFolder, 'Exp5_Acc_BinClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = propMissings * 100, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Execution time (s)') + xlab('Percentage of missing values (%)') + ggtitle('Binomial classification problems') + xlim(0, 100)
ggsave(paste(exp5BinFolder, 'Exp5_Time_BinClass.png', sep = ''), width = 5, height = 3.5)
plot(df$shortResults$exp5BinFolder, df$shortResults$accMean, type = 'l')


#############
## Multinomial Classification ##
#############

hc <- LoadMultiClassProblems(large = FALSE)
df <- runExperiment5(hc, nRuns = 10, classification = TRUE, targetFirstPosition = FALSE)
save(df, file = "tests/Exp5/Exp5MultiClassSmall.Rdata")
#load("tests/Exp5/Exp5MultiClassSmall.Rdata")

ggplot(data = df$shortResult, aes(x = propMissings * 100, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Accuracy (%)') + xlab('Percentage of missing values (%)') + ggtitle('Multinomial classification problems') + xlim(0, 100)
ggsave(paste(exp5MultiFolder, 'Exp5_Acc_MultiClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = propMissings * 100, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Execution time (s)') + xlab('Percentage of missing values (%)') + ggtitle('Multinomial classification problems') + xlim(0, 100)
ggsave(paste(exp5MultiFolder, 'Exp5_Time_MultiClass.png', sep = ''), width = 5, height = 3.5)



# Export to latex
tmp <- df$shortResults
tmp$Prop <- paste(tmp$propMissings * 100, '%', sep = '')
tmp$propMissings <- NULL
tmp$ensMethod <- NULL
tmp$clust.method <- NULL

tmp <- tmp[c("dataset", "Prop", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)




