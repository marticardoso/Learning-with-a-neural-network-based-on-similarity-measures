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
source('tests/testSNNOptUtils.R')
source('loadDatasets.R')
source('benchmarkutils.R')

#Define folders where to save the results
exp1RegFolder <- 'Pictures/Experiments/Exp1/Reg/'
exp1MultiFolder <- 'Pictures/Experiments/Exp1/Multi/'
exp1BinFolder <- 'Pictures/Experiments/Exp1/Bin/'


################
## Regression ##
################
set.seed(2)
automobile <- LoadAutomobileDS()
autoMPG <- LoadAutoMPGDS()
communities <- LoadCommunitiesDataset()
df <- runSNNOptTests(list(automobile, autoMPG), nRuns = 5)

r <- LoadRegressionProblems()
r[[5]]<-NULL
df <- runSNNOptTests(r, nRuns = 50)

#save(df, file = "tests/regressionAutomAutoMPGAndCommunities2.Rdata")
#load('tests/ExpSNNBinClass.Rdata')
#fullResults$fullMethod <- paste(fullResults$clust.method, paste('P:', fullResults$method), ifelse(fullResults$reg,'Reg','-'), sep = '\n')
#fullResults[fullResults$method == 'tree',]$fullMethod <- 'Tree'
#df <- list(fullResults = fullResults, shortResult = shortResult)

dsNames <- levels(df$shortResults$dataset)
for (ds in dsNames) {
  plot <- ggplot(df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() + ggtitle(paste(ds, ' dataset', sep = '')) + ylab('NRMSE') + xlab(NULL) #+ geom_jitter(shape = 16, position = position_jitter(0.2))
  print(plot)
  ggsave(paste(exp1RegFolder, 'Exp1_NRMSE_', ds, '.png', sep = ''), width = 7, height = 4.5)
}

# Time
for (ds in dsNames) {
  plot <- ggplot(data = df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = time)) +
  geom_boxplot() + ggtitle(paste('Execution time (', ds, ' dataset)', sep = '')) + ylab('Execution time (s)') + xlab('')
  print(plot)
  ggsave(paste(exp1RegFolder, 'Exp1_Time_', ds, '.png', sep = ''), width = 7, height = 4.5)
}

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('NRMSE') + xlab(NULL)
  #geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1) +
print(p)
ggsave(paste(exp1RegFolder, 'Exp2_AccSummary.png', sep = ''))

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = timeMean, group = dataset, color = dataset)) + ylab('Execution time (s)') + xlab('') +
  geom_line() + geom_point()
  #geom_errorbar(aes(ymin = timeMean - timeSd, ymax = timeMean + timeSd), width = .1) +
print(p)
ggsave(paste(exp1RegFolder, 'Exp2_TimeSummary.png', sep = ''))


#####################
## Binomial class  ##
#####################

hc <- LoadBinClassProblems()
df <- runSNNOptTests(hc, nRuns = 50, classification = TRUE, onlyTree = FALSE)
soundEnd()

# load('tests/Exp1BinClassSmallDFV2.Rdata')
df$fullResults$fullMethod <- paste(ifelse(df$fullResults$clust.method=='PAM','PAM','Rand'), paste('P:', df$fullResults$method), ifelse(df$fullResults$reg, 'Reg', '-'), sep = '\n')
df$fullResults[df$fullResults$method == 'tree',]$fullMethod <- 'Tree'
df$shortResults$fullMethod <- paste(ifelse(df$shortResults$clust.method == 'PAM', 'PAM', 'Rand'), paste('P:', df$shortResults$method), ifelse(df$shortResults$reg, 'Reg', '-'), sep = '\n')
df$shortResults[df$shortResults$method == 'tree',]$fullMethod <- 'Tree'

dsNames <- levels(df$shortResults$dataset)
for (ds in dsNames) {
  plot <- ggplot(df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() + ggtitle(paste(ds, ' dataset', sep = '')) + ylab('Accuracy (%)') + xlab(NULL)  #+ geom_jitter(shape = 16, position = position_jitter(0.2))
  print(plot)
  ggsave(paste(exp1BinFolder, 'Exp1_Acc_', ds, '.png', sep = ''), width = 7, height = 4.5)
}

# Time
for (ds in dsNames) {
  plot <- ggplot(data = df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = time)) +
  geom_boxplot() + ggtitle(paste('Execution time (', ds, ' dataset)', sep = '')) + ylab('Execution time (s)') + xlab('')
  print(plot)
  ggsave(paste(exp1BinFolder, 'Exp1_Time_', ds, '.png', sep = ''), width=7, height = 4.5)
}

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Mean accuracy (%)') +xlab(NULL)
print(p)
ggsave(paste(exp1BinFolder, 'Exp1_AccSummary.png', sep = ''), width = 8, height = 4.5)

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point() + ylab('Mean execution time (s)')
print(p)
ggsave(paste(exp1BinFolder, 'Exp1_TimeSummary.png', sep = ''), width = 8, height = 4.5)

#####################
## Multinomial class##
#####################

hc <- LoadMultiClassProblems()
df <- runSNNOptTests(hc, nRuns = 50, classification = TRUE, onlyTree = FALSE)
soundEnd()

# load('tests/Exp1MultiClassSmallDFV2.Rdata')
df$fullResults$fullMethod <- paste(ifelse(df$fullResults$clust.method == 'PAM', 'PAM', 'Rand'), paste('P:', df$fullResults$method), ifelse(df$fullResults$reg, 'Reg', '-'), sep = '\n')
df$fullResults[df$fullResults$method == 'tree',]$fullMethod <- 'Tree'


dsNames <- levels(df$shortResults$dataset)
for (ds in dsNames) {
  plot <- ggplot(df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = saccOrNRMSE)) +
  geom_boxplot() + ggtitle(paste(ds, ' dataset', sep = '')) + ylab('Accuracy (%)') + xlab(NULL) #+ geom_jitter(shape = 16, position = position_jitter(0.2))
  print(plot)
  ggsave(paste(exp1MultiFolder, 'Exp1_Acc_', ds, '.png', sep = ''), width = 7, height = 4.5)
}

# Time
for (ds in dsNames) {
  plot <- ggplot(data = df$fullResults[df$fullResults$dataset == ds,], aes(x = fullMethod, y = time)) +
  geom_boxplot() + ggtitle(paste('Execution time (', ds, ' dataset)', sep = '')) + ylab('Execution time (s)') + xlab('')
  print(plot)
  ggsave(paste(exp1MultiFolder, 'Exp1_Time_', ds, '.png', sep = ''), width = 7, height = 4.5)
}

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = accMean, group = dataset, color = dataset)) +
  geom_line() + geom_point()
print(p)
ggsave(paste(exp1MultiFolder, 'Exp1_AccSummary.png', sep = ''))

p <- ggplot(data = df$shortResults, aes(x = fullMethod, y = timeMean, group = dataset, color = dataset)) +
  geom_line() + geom_point()
print(p)
ggsave(paste(exp1MultiFolder, 'Exp1_TimeSummary.png', sep = ''))


tmp <- df$shortResults
tmp$reg <- ifelse(tmp$reg, 'Yes', 'No')
tmp$reg[tmp$method == 'tree'] <- ''
tmp$fullMethod <- NULL
tmp$Learner <- ifelse(tmp$method == 'tree', 'Decision tree', 'SNN')
tmp$method[tmp$method == 'tree'] <- ''
tmp <- tmp[c("dataset", "Learner", "clust.method", "method", "reg", "accMean", "accSd", "timeMean", "timeSd")]
print(xtable(tmp, digits = 4), include.rownames = FALSE)
