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
source('tests/exp10NumRowsSource.R')
source('loadDatasets.R')
source('benchmarkutils.R')

#Output folder
exp10RegFolder <- 'Pictures/Experiments/Exp10/Reg/'
exp10MultiFolder <- 'Pictures/Experiments/Exp10/Multi/'
exp10BinFolder <- 'Pictures/Experiments/Exp10/Bin/'

################
## Regression ##
################

set.seed(2)
regDs <- LoadRegressionProblems(large = FALSE)
df <- runExperiment10(regDs, nRuns = 10)
save(df, file = "tests/Exp10/Exp10RegSmall.Rdata")
#load("tests/Exp10/Exp10RegSmall.Rdata")
ggplot(data = df$shortResult, aes(x = nrow.prop, y = accMean, group = dataset, color = dataset)) +
  geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('NRMSE') +
  xlab('Percentage of obs. used in each SNN (EnsSNN)') + ggtitle('Regression problems') + coord_cartesian(ylim = c(0, 0.5))
ggsave(paste(exp10RegFolder, 'Exp10_NRMSE_Regression.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = nrow.prop, y = timeMean, group = dataset, color = dataset)) +
  geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Percentage of obs. used in each SNN (EnsSNN)') + ggtitle('Regression problems')
ggsave(paste(exp10RegFolder, 'Exp10_Time_Regression.png', sep = ''), width = 5, height = 3.5)



#############
## Classification binomial ##
#############

hc <- LoadBinClassProblems(large = FALSE)
df2 <- runExperiment10(hc, nRuns = 10, classification = TRUE)
save(df, file = "tests/Exp10/Exp10BinClassSmall.Rdata")
#load("tests/Exp10/Exp10BinClassSmall.Rdata")
ggplot(data = df$shortResult, aes(x = nrow.prop, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Percentage of obs. used in each SNN (EnsSNN)') + ggtitle('Binomial classification problems')
ggsave(paste(exp10BinFolder, 'Exp10_Acc_BinClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = nrow.prop, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Percentage of obs. used in each SNN (EnsSNN)') + ggtitle('Binomial classification problems')
ggsave(paste(exp10BinFolder, 'Exp10_Time_BinClass.png', sep = ''), width = 5, height = 3.5)

#############
## Multinomial Classification ##
#############

hc <- LoadMultiClassProblems(large = FALSE)
df <- runExperiment10(hc, nRuns = 10, classification = TRUE)
save(df, file = "tests/Exp10/Exp10MultiClassSmall.Rdata")
#load("tests/Exp10/Exp10MultiClassSmall.Rdata")

ggplot(data = df$shortResult, aes(x = nrow.prop, y = accMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Accuracy (%)') + xlab('Percentage of obs. used in each SNN (EnsSNN)') + ggtitle('Multinomial classification problems')
ggsave(paste(exp10MultiFolder, 'Exp10_Acc_MultiClass.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(x = nrow.prop, y = timeMean, group = dataset, color = dataset)) +
   geom_point(size = 2, aes(shape = dataset)) + geom_line(alpha = 0.5) + ylab('Execution time (s)') + xlab('Percentage of obs. used in each SNN (EnsSNN)') + ggtitle('Multinomial classification problems')
ggsave(paste(exp10MultiFolder, 'Exp10_Time_MultiClass.png', sep = ''), width = 5, height = 3.5)



# Export to latex
tmp <- df$shortResults
tmp$Prop <- paste(tmp$nrow.prop * 100, '%', sep = '')
tmp <- tmp[c("dataset", "Prop", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)