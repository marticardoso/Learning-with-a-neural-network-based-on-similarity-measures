

LoadAdultDataset <- function() {
  ds <- read.table("datasets/classification/+/Adult/adult.data", sep = ",", strip.white = TRUE, na.strings = "?")
  colnames(ds)[15] <- "Target"
  return(list(datasets = ds))
}


LoadBankMarketingDS <- function() {
  ds <- read.csv('./datasets/classification/BankMarketing/bank-full.csv', sep = ';')
  ds
}

LoadCreditApprovalDS <- function() {
  ds <- read.table('./datasets/classification/CreditApproval/crx.data', sep = ',', na.strings = '?')
  ds
}

LoadHepatitisDs <- function() {
  ds <- read.table('./datasets/classification/Hepatitis/hepatitis.data', sep = ',', na.string = '?')

  colnames(ds) <- c('Class', 'age', 'sex', 'steroid', 'antivirals', 'fatigue', 'malaise', 'anorexia', 'liverbig', 'liverfirm',
           'Spleenpalpable', 'spiders', 'ascites', 'varices', 'bilirubin', 'alk', 'sgot', 'albumin', 'protime', 'histology')
  for (i in c(4:14, 20))
    ds[, i] <- as.logical(ds[, i] - 1)
  ds[, 3] <- as.factor(ds[, 3])
  levels(ds[, 3]) <- c('male', 'female')
  ds[, 1] <- as.factor(ds[, 1])
  levels(ds[, 1]) <- c('die', 'live')
  ds
}


Load <- function() {
  ds <- read.csv('./datasets/online-sex-work/online_sex_work.csv', sep = ',')
  ds$Friends_ID_list <- NULL
  kds <- rbind(ds[ds$Risk == 'High_risk',], ds[ds$Risk == 'No_risk',])
  ds$User_ID <- NULL
}


LoadHepatitisDs <- function() {
  ds <- read.table('./datasets/classification/Hepatitis/hepatitis.data', sep = ',', na.string = '?')

  colnames(ds) <- c('Class', 'age', 'sex', 'steroid', 'antivirals', 'fatigue', 'malaise', 'anorexia', 'liverbig', 'liverfirm',
           'Spleenpalpable', 'spiders', 'ascites', 'varices', 'bilirubin', 'alk', 'sgot', 'albumin', 'protime', 'histology')
  for (i in c(4:14, 20))
    ds[, i] <- as.logical(ds[, i] - 1)
  ds[, 3] <- as.factor(ds[, 3])
  levels(ds[, 3]) <- c('male', 'female')
  ds[, 1] <- as.factor(ds[, 1])
  levels(ds[, 1]) <- c('die', 'live')
  ds
}