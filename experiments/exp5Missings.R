# Run Experiment 5 (Tests the robustness of the Ensemble against missing values).
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
source('experiments/exp5MissingsSource.R')
source('test/loadDatasets.R')
source('test/benchmarkutils.R')

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
df <- runExperiment5(hc, nRuns = 20, targetFirstPosition = FALSE)
#save(df, file = "experiments/Exp5/Exp5Reg.Rdata")
#load("experiments/Exp5/Exp5Reg.Rdata")
ggplot(data = df$shortResult, aes(x = propMissings * 100, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('NRMSE') + xlab('Percentage of missing values (%)') + ggtitle('Regression problems') + xlim(0,100)
#ggsave(paste(exp5RegFolder, 'Exp5_NRMSE_Regression.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = propMissings * 100, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Execution time (s)') + xlab('Percentage of missing values (%)') + ggtitle('Regression problems') + xlim(0, 100)
#ggsave(paste(exp5RegFolder, 'Exp5_Time_Regression.png', sep = ''), width = 5, height = 3.5)



#############
## Classification binomial ##
#############

hc <- LoadBinClassProblems(large = FALSE)
df <- runExperiment5(hc, nRuns = 20, classification = TRUE, targetFirstPosition = FALSE)
#save(df, file = "experiments/Exp5/Exp5BinClass.Rdata")
#load("experiments/Exp5/Exp5BinClass.Rdata")
ggplot(data = df$shortResult, aes(x = propMissings * 100, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Accuracy (%)') + xlab('Percentage of missing values (%)') + ggtitle('Binomial classification problems') + xlim(0, 100)
#ggsave(paste(exp5BinFolder, 'Exp5_Acc_BinClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = propMissings * 100, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Execution time (s)') + xlab('Percentage of missing values (%)') + ggtitle('Binomial classification problems') + xlim(0, 100)
#ggsave(paste(exp5BinFolder, 'Exp5_Time_BinClass.png', sep = ''), width = 5, height = 3.5)
#plot(df$shortResults$exp5BinFolder, df$shortResults$accMean, type = 'l')


#############
## Multinomial Classification ##
#############

hc <- LoadMultiClassProblems(large = FALSE)
df <- runExperiment5(hc, nRuns = 20, classification = TRUE, targetFirstPosition = FALSE)
#save(df, file = "experiments/Exp5/Exp5MultiClass.Rdata")
#load("experiments/Exp5/Exp5MultiClass.Rdata")

ggplot(data = df$shortResult, aes(x = propMissings * 100, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Accuracy (%)') + xlab('Percentage of missing values (%)') + ggtitle('Multinomial classification problems') + xlim(0, 100)
#ggsave(paste(exp5MultiFolder, 'Exp5_Acc_MultiClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = propMissings * 100, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Execution time (s)') + xlab('Percentage of missing values (%)') + ggtitle('Multinomial classification problems') + xlim(0, 100)
#ggsave(paste(exp5MultiFolder, 'Exp5_Time_MultiClass.png', sep = ''), width = 5, height = 3.5)



# Export to latex table
tmp <- df$shortResults
tmp$Prop <- paste(tmp$propMissings * 100, '%', sep = '')
tmp$propMissings <- NULL
tmp$ensMethod <- NULL
tmp$clust.method <- NULL
tmp <- tmp[c("dataset", "Prop", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)




