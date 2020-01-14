# Run Experiment 10 (Tests the regularization of each SNN of an Ensemble).
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
source('experiments/exp11RegSNNSource.R')
source('test/loadDatasets.R')
source('test/benchmarkutils.R')

#Output folder
exp11RegFolder <- 'Pictures/Experiments/Exp11/Reg/'
exp11MultiFolder <- 'Pictures/Experiments/Exp11/Multi/'
exp11BinFolder <- 'Pictures/Experiments/Exp11/Bin/'

################
## Regression ##
################

set.seed(2)
regDs <- LoadRegressionProblems(large = FALSE)
df <- runExperiment11(regDs, nRuns = 20)
#save(df, file = "experiments/Exp11/Exp11RegSmall.Rdata")
#load("experiments/Exp11/Exp11RegSmall.Rdata")
ggplot(data = df$shortResult, aes(x = isRegularization, y = accMean, group = dataset, color = dataset)) +
  geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('NRMSE') + xlab('Regularization of the SNNs (EnsSNN) ') + ggtitle('Regression problems')
#ggsave(paste(exp11RegFolder, 'Exp11_NRMSE_Regression.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = isRegularization, y = timeMean, group = dataset, color = dataset)) +
  geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Regularization of the SNNs (EnsSNN) ') + ggtitle('Regression problems')
#ggsave(paste(exp11RegFolder, 'Exp11_Time_Regression.png', sep = ''), width = 5, height = 3.5)



#############
## Classification binomial ##
#############

hc <- LoadBinClassProblems(large = FALSE)
df <- runExperiment11(hc, nRuns = 20, classification = TRUE)
#save(df, file = "experiments/Exp11/Exp11BinClassSmall.Rdata")
#load("experiments/Exp11/Exp11BinClassSmall.Rdata")
ggplot(data = df$shortResult, aes(x = isRegularization, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Regularization of the SNNs (EnsSNN) ') + ggtitle('Binomial classification problems')
#ggsave(paste(exp11BinFolder, 'Exp11_Acc_BinClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = isRegularization, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Regularization of the SNNs (EnsSNN) ') + ggtitle('Binomial classification problems')
#ggsave(paste(exp11BinFolder, 'Exp11_Time_BinClass.png', sep = ''), width = 5, height = 3.5)

#############
## Multinomial Classification ##
#############

hc <- LoadMultiClassProblems(large = FALSE)
df <- runExperiment11(hc, nRuns = 20, classification = TRUE)
#save(df, file = "experiments/Exp11/Exp11MultiClassSmall.Rdata")
#load("experiments/Exp11/Exp11MultiClassSmall.Rdata")

ggplot(data = df$shortResult, aes(x = isRegularization, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Regularization of the SNNs (EnsSNN) ') + ggtitle('Multinomial classification problems')
#ggsave(paste(exp11MultiFolder, 'Exp11_Acc_MultiClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = isRegularization, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Regularization of the SNNs (EnsSNN) ') + ggtitle('Multinomial classification problems')
#ggsave(paste(exp11MultiFolder, 'Exp11_Time_MultiClass.png', sep = ''), width = 5, height = 3.5)



# Export to latex table
tmp <- df$shortResults
tmp <- tmp[c("dataset", "isRegularization", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)

