# Run Experiment 10 (Tests the number of rows used to train each SNN of an Ensemble).
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
source('experiments/exp10NumRowsSource.R')
source('test/loadDatasets.R')
source('test/benchmarkutils.R')

#Output folder
exp10RegFolder <- 'Pictures/Experiments/Exp10/Reg/'
exp10MultiFolder <- 'Pictures/Experiments/Exp10/Multi/'
exp10BinFolder <- 'Pictures/Experiments/Exp10/Bin/'

################
## Regression ##
################

set.seed(2)
regDs <- LoadRegressionProblems(large = FALSE)
df <- runExperiment10(regDs, nRuns = 20)
#save(df, file = "experiments/Exp10/Exp10RegSmall.Rdata")
#load("experiments/Exp10/Exp10RegSmall.Rdata")
ggplot(data = df$shortResult, aes(x = nrow.prop, y = accMean, group = dataset, color = dataset)) +
  geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('NRMSE') +
  xlab('Percentage of obs. used in each SNN (EnsSNN)') + ggtitle('Regression problems') 
#ggsave(paste(exp10RegFolder, 'Exp10_NRMSE_Regression.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = nrow.prop, y = timeMean, group = dataset, color = dataset)) +
  geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Percentage of obs. used in each SNN (EnsSNN)') + ggtitle('Regression problems')
#ggsave(paste(exp10RegFolder, 'Exp10_Time_Regression.png', sep = ''), width = 5, height = 3.5)



#############
## Classification binomial ##
#############

hc <- LoadBinClassProblems(large = FALSE)
df <- runExperiment10(hc, nRuns = 20, classification = TRUE)
#save(df, file = "experiments/Exp10/Exp10BinClassSmall.Rdata")
#load("experiments/Exp10/Exp10BinClassSmall.Rdata")
ggplot(data = df$shortResult, aes(x = nrow.prop, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Percentage of obs. used in each SNN (EnsSNN)') + ggtitle('Binomial classification problems')
#ggsave(paste(exp10BinFolder, 'Exp10_Acc_BinClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = nrow.prop, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Percentage of obs. used in each SNN (EnsSNN)') + ggtitle('Binomial classification problems')
#ggsave(paste(exp10BinFolder, 'Exp10_Time_BinClass.png', sep = ''), width = 5, height = 3.5)


#############
## Multinomial Classification ##
#############

hc <- LoadMultiClassProblems(large = FALSE)
df <- runExperiment10(hc, nRuns = 20, classification = TRUE)
#save(df, file = "experiments/Exp10/Exp10MultiClassSmall.Rdata")
#load("experiments/Exp10/Exp10MultiClassSmall.Rdata")

ggplot(data = df$shortResult, aes(x = nrow.prop, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Percentage of obs. used in each SNN (EnsSNN)') + ggtitle('Multinomial classification problems')
#ggsave(paste(exp10MultiFolder, 'Exp10_Acc_MultiClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = nrow.prop, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Percentage of obs. used in each SNN (EnsSNN)') + ggtitle('Multinomial classification problems')
#ggsave(paste(exp10MultiFolder, 'Exp10_Time_MultiClass.png', sep = ''), width = 5, height = 3.5)



# Export to latex table
tmp <- df$shortResults
tmp$Prop <- paste(tmp$nrow.prop * 100, '%', sep = '')
tmp <- tmp[c("dataset", "Prop", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)