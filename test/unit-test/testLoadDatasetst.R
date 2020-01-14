# In this file, there are some unit tests that check the right loading of the datasets
# (this is not an experiment)

source('test/benchmarkutils.R')
source('test/loadDatasets.R')

# Regression
r <- LoadAutomobileDS()
getColumnClassTable(r$dataset[, -26])
getPercentageOfNa(r$dataset[, -26])
getColumnClass(r$dataset[, -26])
r$simil.types

r <- LoadAutoMPGDS()
getColumnClassTable(r$dataset[, -1])
getPercentageOfNa(r$dataset[, -1])
getColumnClass(r$dataset[, -1])
r$simil.types

r <- LoadCommunitiesDataset()
getColumnClassTable(r$dataset[, -123])
getPercentageOfNa(r$dataset[, -123])
getColumnClass(r$dataset[, -123])
r$simil.types

r <- LoadMvDataset()
getColumnClassTable(r$dataset[, -11])
getPercentageOfNa(r$dataset[, -11])
getColumnClass(r$dataset[, -11])
r$simil.types

# Binomial Classification

r <- LoadHeartDataset()
getColumnClassTable(r$dataset[, -14])
getPercentageOfNa(r$dataset[, -14])
getColumnClass(r$dataset[, -14])
r$simil.types

r <- LoadHorseColicV2()
getColumnClassTable(r$dataset[, -22])
getColumnClass(r$dataset[, -22])
getPercentageOfNa(r$dataset[, -22])
getColumnClass(r$dataset[, -22])
r$simil.types

r <- LoadPimaDataset()
getColumnClassTable(r$dataset[, -9])
getColumnClass(r$dataset[, -9])
getPercentageOfNa(r$dataset[, -9])
r$simil.types

r <- LoadMammographicDataset()
getColumnClassTable(r$dataset[, -5])
getColumnClass(r$dataset[, -5])
getPercentageOfNa(r$dataset[, -5])
r$simil.types

r <- LoadMushroomDataset()
getColumnClassTable(r$dataset[, -1])
getColumnClass(r$dataset[, -1])
getPercentageOfNa(r$dataset[, -1])
r$simil.types

r <- LoadCensus()
getColumnClassTable(r$dataset[, -42])
getColumnClass(r$dataset[, -42])
getPercentageOfNa(r$dataset[, -42])
r$simil.types

# Multinomial classification

r <- LoadAudiologyDs()
getColumnClassTable(r$dataset[, -32])
getColumnClass(r$dataset[, -32])
getPercentageOfNa(r$dataset[, -32])
r$simil.types

r <- LoadGlass()
getColumnClassTable(r$dataset[, -10])
getColumnClass(r$dataset[, -10])
getPercentageOfNa(r$dataset[, -10])
r$simil.types

r <- LoadHorseColicV1()
getColumnClassTable(r$dataset[, -22])
getColumnClass(r$dataset[, -22])
getPercentageOfNa(r$dataset[, -22])
r$simil.types

r <- LoadAnnealing()
getColumnClassTable(r$dataset[, -32])
getColumnClass(r$dataset[, -32])
getPercentageOfNa(r$dataset[, -32])
r$simil.types

r <- LoadContraceptiveDs()
getColumnClassTable(r$dataset[, -10])
getColumnClass(r$dataset[, -10])
getPercentageOfNa(r$dataset[, -10])
r$simil.types

r <- LoadDiabetis()
getColumnClassTable(r$dataset[, -46])
getColumnClass(r$dataset[, -46])
getPercentageOfNa(r$dataset[, -46])
r$simil.types
