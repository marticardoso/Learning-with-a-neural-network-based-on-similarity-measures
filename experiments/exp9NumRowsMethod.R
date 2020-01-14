# Run Experiment 9 (Tests the number of rows distribution used to train each SNN of an Ensemble).
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
source('experiments/exp9NumRowsMethodSource.R')
source('test/loadDatasets.R')
source('test/benchmarkutils.R')

#Output folder
exp9RegFolder <- 'Pictures/Experiments/Exp9/Reg/'
exp9MultiFolder <- 'Pictures/Experiments/Exp9/Multi/'
exp9BinFolder <- 'Pictures/Experiments/Exp9/Bin/'

################
## Regression ##
################

set.seed(2)
regDs <- LoadRegressionProblems(large = FALSE)
df <- runExperiment9(regDs, nRuns = 20)
#save(df, file = "experiments/Exp9/Exp9RegSmall.Rdata")
#load("experiments/Exp9/Exp9RegSmall.Rdata")
ggplot(data = df$shortResult, aes(x = nrow.method, y = accMean, group = dataset, color = dataset)) +
  geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('NRMSE') + xlab('Method to choose the number of obs. of each SNN') + ggtitle('Regression problems')
#ggsave(paste(exp9RegFolder, 'Exp9_NRMSE_Regression.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = nrow.method, y = timeMean, group = dataset, color = dataset)) +
  geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Method to choose the number of obs. of each SNN') + ggtitle('Regression problems')
#ggsave(paste(exp9RegFolder, 'Exp9_Time_Regression.png', sep = ''), width = 5, height = 3.5)


#############
## Classification binomial ##
#############

hc <- LoadBinClassProblems(large = FALSE)
df <- runExperiment9(hc, nRuns = 20, classification = TRUE)
#save(df, file = "experiments/Exp9/Exp9BinClassSmall.Rdata")
#load("experiments/Exp9/Exp9BinClassSmall.Rdata")
ggplot(data = df$shortResult, aes(x = nrow.method, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Method to choose the number of obs. of each SNN') + ggtitle('Binomial classification problems')
#ggsave(paste(exp9BinFolder, 'Exp9_Acc_BinClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = nrow.method, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Method to choose the number of obs. of each SNN') + ggtitle('Binomial classification problems')
#ggsave(paste(exp9BinFolder, 'Exp9_Time_BinClass.png', sep = ''), width = 5, height = 3.5)

#############
## Multinomial Classification ##
#############

hc <- LoadMultiClassProblems(large = FALSE)
df <- runExperiment9(hc, nRuns = 20, classification = TRUE)
#save(df, file = "experiments/Exp9/Exp9MultiClassSmall.Rdata")
#load("experiments/Exp9/Exp9MultiClassSmall.Rdata")

ggplot(data = df$shortResult, aes(x = nrow.method, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Method to choose the number of obs. of each SNN') + ggtitle('Multinomial classification problems')
#ggsave(paste(exp9MultiFolder, 'Exp9_Acc_MultiClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = nrow.method, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Method to choose the number of obs. of each SNN') + ggtitle('Multinomial classification problems')
#ggsave(paste(exp9MultiFolder, 'Exp9_Time_MultiClass.png', sep = ''), width = 5, height = 3.5)



# Export to latex table
tmp <- df$shortResults
tmp <- tmp[c("dataset", "nrow.method", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)
