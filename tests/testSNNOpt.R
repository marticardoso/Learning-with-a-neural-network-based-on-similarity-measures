rm(list = ls())
set.seed(104)

# Set environment
setwd(".")


library(rattle.data)
library(faraway)
library(mlbench)
library(tree)

#First we load some useful function for the model selection task
source('SNN.R')
source('testUtils.R')
source('loadDatasets.R')


################
## Regression ##
################
set.seed(2)
automobile <- LoadAutomobileDS()
autoMPG <- LoadAutoMPGDS()
communities <- LoadCommunitiesDataset()
df <- runSNNOptTests(list(automobile, autoMPG, communities), nRuns = 10)

ggplot(data = df, aes(x = method, y = mean, group = dataset, color=dataset)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1) +
  geom_line() +
  geom_point()

ggplot(data = df, aes(x = method, y = timeMean, group = dataset, color = dataset)) +
  geom_errorbar(aes(ymin = timeMean - timeSd, ymax = timeMean + timeSd), width = .1) +
  geom_line() +
  geom_point()

#############
## Classification binomial ##
#############

heart <- LoadHeartDataset()
mammographic <- LoadMammographicDataset()
mushroom <- LoadMushroomDataset()
adult <- LoadAdultDataset()

df <- runSNNOptTests(list(heart, mammographic), nRuns = 10, classification = TRUE)
ggplot(data = df, aes(x = method, y = mean, group = dataset, color = dataset)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .1) +
  geom_line() +
  geom_point()

ggplot(data = df, aes(x = method, y = timeMean, group = dataset, color = dataset)) +
  geom_errorbar(aes(ymin = timeMean - timeSd, ymax = timeMean + timeSd), width = .1) +
  geom_line() +
  geom_point()