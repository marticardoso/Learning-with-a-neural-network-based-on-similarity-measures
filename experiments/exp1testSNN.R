# Run Experiment 1 (Tests the SNN for several parameters).
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
source('experiments/exp1testSNNSource.R')
source('test/loadDatasets.R')
source('test/benchmarkutils.R')

#Define folders where to save the results
exp1RegFolder <- 'Pictures/Exp1/'
exp1MultiFolder <- 'Pictures/Experiments/Exp1/Multi/'
exp1BinFolder <- 'Pictures/Experiments/Exp1/Bin/'


################
## Regression ##
################
set.seed(2)
r <- LoadRegressionProblems()
df <- runSNNOptTests(r, nRuns = 50) # Warning: Execution time high

#save(df, file = "experiments/Exp1SNNReg.Rdata")
#load('experiments/Exp1SNNReg.Rdata')
df$fullResults <- df$fullResults[df$fullResults$dataset == 'MV',]
df$fullResults$fullMethod2 <- as.factor(gsub("\n", " / ", df$fullResults$fullMethod))


dsNames <- levels(df$shortResults$dataset)
for (ds in dsNames) {
  plot <- ggplot(df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod2, y = saccOrNRMSE)) +
  geom_boxplot() + ggtitle(paste(ds, ' dataset', sep = '')) + ylab('NRMSE') + xlab(NULL) +
  coord_flip() + scale_x_discrete(limits = rev(levels(df$fullResults$fullMethod2)))
  print(plot)
  ggsave(paste(exp1RegFolder, 'Exp1_NRMSE_', ds, '2.png', sep = ''), width = 5, height = 2.5)
}

# Time
for (ds in dsNames) {
  plot <- ggplot(data = df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod2, y = time)) +
  geom_boxplot() + ggtitle(paste('Execution time (', ds, ' dataset)', sep = '')) + ylab('Execution time (s)') + xlab(NULL) +
  coord_flip() + scale_x_discrete(limits = rev(levels(df$fullResults$fullMethod2)))
  print(plot)
  ggsave(paste(exp1RegFolder, 'Exp1_Time_', ds, '2.png', sep = ''), width = 5, height = 2.5)
}

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('NRMSE') + xlab(NULL)
print(p)
#ggsave(paste(exp1RegFolder, 'Exp2_AccSummary.png', sep = ''))

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = timeMean, group = dataset, color = dataset)) + ylab('Execution time (s)') + xlab('') +
  geom_line() + geom_point()
print(p)
#ggsave(paste(exp1RegFolder, 'Exp2_TimeSummary.png', sep = ''))


#####################
## Binomial class  ##
#####################

hc <- LoadBinClassProblems()
df <- runSNNOptTests(hc, nRuns = 50, classification = TRUE, onlyTree = FALSE)

# save(df, file = 'experiments/Exp1BinClassDF.Rdata')
# load('experiments/Exp1BinClassDF.Rdata')

dsNames <- levels(df$shortResults$dataset)
for (ds in dsNames) {
  plot <- ggplot(df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() + ggtitle(paste(ds, ' dataset', sep = '')) + ylab('Accuracy (%)') + xlab(NULL)  
  print(plot)
  #ggsave(paste(exp1BinFolder, 'Exp1_Acc_', ds, '.png', sep = ''), width = 7, height = 4.5)
}

# Time
for (ds in dsNames) {
  plot <- ggplot(data = df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = time)) +
  geom_boxplot() + ggtitle(paste('Execution time (', ds, ' dataset)', sep = '')) + ylab('Execution time (s)') + xlab(NULL)
  print(plot)
  #ggsave(paste(exp1BinFolder, 'Exp1_Time_', ds, '.png', sep = ''), width=7, height = 4.5)
}

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Mean accuracy (%)') +xlab(NULL)
print(p)
#ggsave(paste(exp1BinFolder, 'Exp1_AccSummary.png', sep = ''), width = 8, height = 4.5)

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Mean execution time (s)')
print(p)
#ggsave(paste(exp1BinFolder, 'Exp1_TimeSummary.png', sep = ''), width = 8, height = 4.5)

#####################
## Multinomial class##
#####################

hc <- LoadMultiClassProblems()
df <- runSNNOptTests(hc, nRuns = 50, classification = TRUE, onlyTree = FALSE)

# save(df, file='experiments/Exp1MultiClassDF.Rdata')
# load('experiments/Exp1MultiClassDF.Rdata')

dsNames <- levels(df$shortResults$dataset)
for (ds in dsNames) {
  plot <- ggplot(df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() + ggtitle(paste(ds, ' dataset', sep = '')) + ylab('Accuracy (%)') + xlab(NULL) 
  print(plot)
  #ggsave(paste(exp1MultiFolder, 'Exp1_Acc_', ds, '.png', sep = ''), width = 7, height = 4.5)
}

# Time
for (ds in dsNames) {
  plot <- ggplot(data = df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = time)) +
  geom_boxplot() + ggtitle(paste('Execution time (', ds, ' dataset)', sep = '')) + ylab('Execution time (s)') + xlab('')
  print(plot)
  #ggsave(paste(exp1MultiFolder, 'Exp1_Time_', ds, '.png', sep = ''), width = 7, height = 4.5)
}

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point()
print(p)
#ggsave(paste(exp1MultiFolder, 'Exp1_AccSummary.png', sep = ''))

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point()
print(p)
#ggsave(paste(exp1MultiFolder, 'Exp1_TimeSummary.png', sep = ''))

# Print results as latex table
tmp <- df$shortResults
tmp$reg <- ifelse(tmp$reg, 'Yes', 'No')
tmp$reg[tmp$method == 'tree'] <- ''
tmp$fullMethod <- NULL
tmp$Learner <- ifelse(tmp$method == 'tree', 'Decision tree', 'SNN')
tmp$method[tmp$method == 'tree'] <- ''
tmp <- tmp[c("dataset", "Learner", "clust.method", "method", "reg", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)
