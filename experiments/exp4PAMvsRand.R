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
exp4RegFolder <- 'Pictures/'
exp4MultiFolder <-'Pictures/'
exp4BinFolder <- 'Pictures/'

################
## Regression ##
################

set.seed(2)
regDs <- LoadRegressionProblems(large = FALSE)
df <- runExperiment4(regDs, nRuns = 20)
#save(df, file = "experiments/Exp4/Exp4Reg.Rdata")
#load("experiments/Exp4/Exp4RegSmall.Rdata")
ggplot(data = df$shortResult, aes(fill = clust.method, y = accMean, x = dataset)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(), alpha = 1) + ylab('NRMSE') + xlab('Dataset') + ggtitle('Regression problems') +
  guides(fill = guide_legend(title = "Clustering method")) + scale_fill_brewer(palette = "Blues")
ggsave(paste(exp4RegFolder, 'Exp4_NRMSE_Regression3.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(fill = clust.method, y = timeMean, x = dataset, color = dataset)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(), alpha = 1) + ylab('Execution time (s)') + xlab('Dataset') + ggtitle('Regression problems') +
  guides(fill = guide_legend(title = "Clustering method")) + scale_fill_brewer(palette = "Blues")
ggsave(paste(exp4RegFolder, 'Exp4_Time_Regression3.png', sep = ''), width = 5, height = 3.5)



#############
## Classification binomial ##
#############

hc <- LoadBinClassProblems(large = FALSE)
df <- runExperiment4(hc, nRuns = 20, classification = TRUE)
#save(df, file = "experiments/Exp4/Exp4BinClass.Rdata")
#load("experiments/Exp4/Exp4BinClassSmall.Rdata")
ggplot(data = df$shortResult, aes(fill = clust.method, y = accMean, x = dataset, color = dataset)) +
   geom_bar(stat = "identity", color = "black", position = position_dodge(), alpha = 1) + ylab('Accuracy (%)') + xlab('Dataset') + ggtitle('Binomial classification problems') +
   guides(fill = guide_legend(title = "Clustering method")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_cartesian(ylim = c(75, 85)) + scale_fill_brewer(palette = "Blues")
ggsave(paste(exp4BinFolder, 'Exp4_Acc_BinClass3.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(fill = clust.method, y = timeMean, x = dataset, color = dataset)) +
   geom_bar(stat = "identity", color = "black", position = position_dodge(), alpha = 1) + ylab('Execution time (s)') + xlab('Dataset') + ggtitle('Binomial classification problems') +
   guides(fill = guide_legend(title = "Clustering method")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_brewer(palette = "Blues")
ggsave(paste(exp4BinFolder, 'Exp4_Time_BinClass3.png', sep = ''), width = 5, height = 3.5)

#############
## Multinomial Classification ##
#############

hc <- LoadMultiClassProblems(large = FALSE)
df <- runExperiment4(hc, nRuns = 10, classification = TRUE)
#save(df, file = "experiments/Exp4/Exp4MultiClass.Rdata")
#load("experiments/Exp4/Exp4MultiClassSmall.Rdata")

ggplot(data = df$shortResult, aes(fill = clust.method, y = accMean, x = dataset, color = dataset)) +
   geom_bar(stat = "identity", color = "black", position = position_dodge(), alpha = 1) + ylab('Accuracy (%)') + xlab('Dataset') + ggtitle('Multinomial classification problems') +
   guides(fill = guide_legend(title = "Clustering method")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_cartesian(ylim = c(50, 100)) + scale_fill_brewer(palette = "Blues")
ggsave(paste(exp4MultiFolder, 'Exp4_Acc_MultiClass3.png', sep = ''), width = 5, height = 3.5)

ggplot(data = df$shortResult, aes(fill = clust.method, y = timeMean, x = dataset, color = dataset)) +
   geom_bar(stat = "identity", color = "black", position = position_dodge(), alpha = 1) + ylab('Execution time (s)') + xlab('Dataset') + ggtitle('Multinomial classification problems') +
   guides(fill = guide_legend(title = "Clustering method")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_brewer(palette = "Blues")
ggsave(paste(exp4MultiFolder, 'Exp4_Time_MultiClass3.png', sep = ''), width = 5, height = 3.5)



# Print results as latex table
tmp <- df$shortResults
tmp <- tmp[c("dataset", "clust.method", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)






