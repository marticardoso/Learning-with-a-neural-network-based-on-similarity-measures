library("readxl")
ds6 <- data.frame(read_excel('./datasets/regression/Concrete_Data.xls'))
colnames(ds6) <- c('Cement', 'BlastFurnace', 'FlyAsh', 'Water', 'Superplasticizer', 'Coarse', 'Fine', 'Age', 'CCS')

LoadBankMarketingDS <- function() {
  ds <- read.csv('./datasets/classification/BankMarketing/bank-full.csv', sep = ';')
  ds
}

LoadCreditApprovalDS <- function() {
  ds <- read.table('./datasets/classification/CreditApproval/crx.data', sep = ',', na.strings = '?')
  ds
}


LoadHeartDS <- function() {
  ds <- read.table('./datasets/classification/Heart/heart.dat', sep = '')
  ds[, 11] <- ordered(ds[, 11])
  ds[, 2] <- as.logical(ds[, 2])
  ds[, 6] <- as.logical(ds[, 6])
  ds[, 9] <- as.logical(ds[, 9])
  ds[, 7] <- as.factor(ds[, 7])
  ds[, 3] <- as.factor(ds[, 3])
  ds[, 13] <- as.factor(ds[, 13])
  ds[, 14] <- as.factor(ds[, 14])
  ds
}

LoadHepatitisDs <- function() {
  ds <- read.table('./datasets/classification/Hepatitis/hepatitis.data', sep = ',', na.string = '?')

  colnames(ds) <- c('Class', 'age', 'sex', 'steroid', 'antivirals','fatigue','malaise','anorexia','liverbig','liverfirm',
           'Spleenpalpable', 'spiders', 'ascites', 'varices', 'bilirubin', 'alk', 'sgot', 'albumin', 'protime', 'histology')
  for (i in c(4:14, 20))
    ds[, i] <- as.logical(ds[, i] - 1)
  ds[, 3] <- as.factor(ds[, 3])
  levels(ds[, 3]) <- c('male', 'female')
  ds[, 1] <- as.factor(ds[, 1])
  levels(ds[, 1]) <- c('die', 'live')
  ds
}
#summary(LoadHepatitisDs())

LoadAudiologyDs <- function() {
  ds <- read.table('./datasets/classification/Audiology/audiology.standardized.data', sep = ',', na.string = '?')

  for (i in c(1, 3,7, 9:58, 61:63,65,67:69))
    ds[, i] <- as.logical(ds[, i] == 't')
  ds$V2 <- as.ordered(ds$V2)
  ds$V22 <- NULL
  ds$V29 <- NULL
  ds$V32 <- NULL
  ds$V42 <- NULL
  ds$V49 <- NULL
  ds$V48 <- NULL
  ds$V51 <- NULL
  ds$V70 <- NULL
  ds
}
#summary(LoadAudiologyDs())

LoadContraceptiveDs <- function() {
  ds <- read.table('./datasets/classification/Contraceptive/cmc.data', sep = ',', na.string = '?')
  ds$V2 <- as.ordered(ds$V2)
  ds$V3 <- as.ordered(ds$V3)
  ds$V5 <- as.logical(ds$V5)
  ds$V6 <- as.logical(ds$V6 == 0)
  ds$V7 <- as.factor(ds$V7)
  ds$V8 <- as.ordered(ds$V8)
  ds$V9 <- as.logical(ds$V9)
  ds$V10 <- as.factor(ds$V10)
  ds
}
#summary(LoadContraceptiveDs())

ds <- (LoadContraceptiveDs())
s <- sample(nrow(kds), 60)
r1 <- snn(Risk ~ ., kds, subset = s, regularization = TRUE, standardizeSimils = TRUE, x = TRUE)
r1$testContingencyTable
r1$testAccuracy

Load <- function() {
  ds <- read.csv('./datasets/online-sex-work/online_sex_work.csv', sep = ',')
  ds$Friends_ID_list <- NULL
  kds <- rbind(ds[ds$Risk == 'High_risk',], ds[ds$Risk == 'No_risk',])
  ds$User_ID <- NULL
}





##################
# Classification #
##################

LoadHeartDataset <- function() {
  heart <- read.table("datasets/classification/Heart/heart.dat", sep = "", na.strings = "?")

  heart <- heart[!is.na(heart[, "V14"]),]

  # Recode variables
  colnames(heart)[14] <- "Target"

  t.oldvalues <- c("1", "2")
  t.newvalues <- factor(c("Absence", "Presence")) # Make this a factor

  heart$Target <- t.newvalues[match(heart$Target, t.oldvalues)]

  # V2:  Sex
  oldvalues <- c(0.0, 1.0)
  newvalues <- factor(c("0", "1"))

  heart$V2 <- newvalues[match(heart$V2, oldvalues)]

  # V3: chest pain type  (4 values)

  oldvalues <- c(1.0, 2.0, 3.0, 4.0)
  newvalues <- factor(c("1", "2", "3", "4"))

  heart$V3 <- newvalues[match(heart$V3, oldvalues)]

  # V6: fasting blood sugar > 120 mg/dl
  oldvalues <- c(0.0, 1.0)
  newvalues <- factor(c("0", "1"))

  heart$V6 <- newvalues[match(heart$V6, oldvalues)]

  # V7: resting electrocardiographic results  (values 0,1,2)
  oldvalues <- c(0, 1, 2)
  newvalues <- factor(c("0", "1", "2"))

  heart$V7 <- newvalues[match(heart$V7, oldvalues)]

  # V9:  exercise induced angina
  oldvalues <- c(0.0, 1.0)
  newvalues <- factor(c("0", "1"))

  heart$V9 <- newvalues[match(heart$V9, oldvalues)]

  # V11: the slope of the peak exercise ST segment
  oldvalues <- c(1, 2, 3)
  newvalues <- factor(c("1", "2", "3"), ordered = TRUE)

  heart$V11 <- newvalues[match(heart$V11, oldvalues)]

  # V13: thal: 3 = normal; 6 = fixed defect; 7 = reversable defect
  oldvalues <- c(3, 6, 7)
  newvalues <- factor(c("normal", "fixed defect", "reversable defect"))

  heart$V13 <- newvalues[match(heart$V13, oldvalues)]

  # For Gower
  simil.types <- list(ordratio = c("V1", "V4", "V5", "V8", "V10", "V12"))

  
  return(list(datasets=heart, simil.types = simil.types))
}


LoadMammographicDataset <- function() {
  ds <- read.table("datasets/classification/Mammographic/mammographic_masses.data", sep=",", na.strings = "?")

  colnames(ds) <- c('birads', 'age', 'shape','margin','density','severity')

  ds$birads[(!is.na(ds$birads)) & ds$birads == 55] <- 5

  # Recode variables

  ds$severity <- as.factor(ds$severity)
  names(ds$severity) <- c("benign", "malignant")

  ds$shape <- as.factor(ds$shape)
  names(ds$shape) <- c("round", "oval", "lobular", "irregular")


  ds$margin <- as.factor(ds$margin)
  names(ds$margin) <- c("circumscribed", "microlobulated", "obscured", "illdefined", "spiculated")

  ds$density <- as.factor(ds$density)
  names(ds$density) <- c("high", "iso", "low", "fat")

  
  # For Gower
  simil.types <- list(ordratio = c("density"))

  return(list(datasets = ds, simil.types = simil.types))
}


LoadMushroomDataset <- function() {
  ds <- read.table("datasets/classification/Mushroom/agaricus-lepiota.data", sep = ",", na.strings = "?")
  colnames(ds)[1] <- "Target"
  # Empty column
  ds$V17 <- NULL

  return(list(datasets = ds))
}



LoadHorseColicV1 <- function() {
  horse.colic.tr <- read.table('datasets/classification/HorseColic/horse-colic.data', na.strings = '?')
  horse.colic.te <- read.table('datasets/classification/HorseColic/horse-colic.test', na.strings = '?')

  ds <- rbind(horse.colic.tr, horse.colic.te)

  # Remove last variables (other possible targets)
  ds <- ds[1:23]

  # Remove file number (class V23 is now in V22)
  ds <- ds[-3]

  # Remove instances with all values missing
  ds <- Filter(function(x)!all(is.na(x)), ds)

  # Remove instances that do not have a defined class
  ds <- ds[!is.na(ds[, "V23"]),]

  # remove useless observations (only V1 and V2 non missing)
  ds <- ds[-c(29, 216, 288),]

  # Recode target
  colnames(ds)[22] <- "Target"

  t.oldvalues <- c("1", "2", "3")
  t.newvalues <- factor(c("Lived", "Died", "Euthanized"))
  ds$Target <- t.newvalues[match(ds$Target, t.oldvalues)]

  # Recode predictors appropriately

  # V1:  surgery?
  oldvalues <- c(1, 2)
  newvalues <- factor(c("Yes", "No"))
  ds$V1 <- newvalues[match(ds$V1, oldvalues)]

  # V2:  Age
  oldvalues <- c(1, 9)
  newvalues <- factor(c("Adult", "Young"))
  ds$V2 <- newvalues[match(ds$V2, oldvalues)]

  # V7:  temperature of extremities
  oldvalues <- c(4, 3, 1, 2)
  newvalues <- factor(c("Cold", "Cool", "Normal", "Warm"), ordered = TRUE)
  ds$V7 <- newvalues[match(ds$V7, oldvalues)]

  # V8:  peripheral pulse
  oldvalues <- c(4, 3, 1, 2)
  newvalues <- factor(c("1Absent", "2Reduced", "3Normal", "4Increased"), ordered = TRUE)
  ds$V8 <- newvalues[match(ds$V8, oldvalues)]

  # V9:  mucous membranes
  oldvalues <- seq(6)
  newvalues <- factor(c("normal pink", "bright pink", "pale pink", "pale cyanotic", "bright red / injected", "dark cyanotic"))
  ds$V9 <- newvalues[match(ds$V9, oldvalues)]

  # V10: capillary refill time
  # WARNING! there is a couple of values '3' ??? we treat as a NA

  ds$V10[ds$V10 == 3] <- NA

  oldvalues <- c(1, 2)
  newvalues <- factor(c("< 3 seconds", ">= 3 seconds"))
  ds$V10 <- newvalues[match(ds$V10, oldvalues)]

  # V11: pain
  oldvalues <- seq(5)
  newvalues <- factor(
  c("1alert, no pain", "2depressed", "3intermittent mild pain", "4intermittent severe pain", "5continuous severe pain"), ordered = TRUE)
  ds$V11 <- newvalues[match(ds$V11, oldvalues)]

  # V12: peristalsis
  # WARNING! value '4' is documented as NA

  ds$V12[ds$V12 == 4] <- NA
  oldvalues <- c(3, 2, 1)
  newvalues <- factor(c("1hypomotile", "2normal", "3hypermotile"), ordered = TRUE)
  ds$V12 <- newvalues[match(ds$V12, oldvalues)]

  # V13: abdominal distension
  oldvalues <- seq(4)
  newvalues <- factor(c("1none", "2slight", "3moderate", "4severe"), ordered = TRUE)
  ds$V13 <- newvalues[match(ds$V13, oldvalues)]

  # V14: nasogastric tube
  oldvalues <- seq(3)
  newvalues <- factor(c("1none", "2slight", "3significant"), ordered = TRUE)
  ds$V14 <- newvalues[match(ds$V14, oldvalues)]

  # V15: nasogastric reflux

  oldvalues <- c(1, 3, 2)
  newvalues <- factor(c("1none", "2< 1 liter", "3> 1 liter"), ordered = TRUE)
  ds$V15 <- newvalues[match(ds$V15, oldvalues)]

  # V17: rectal examination - feces
  # WARNING! value '4' is documented as NA

  ds$V17[ds$V17 == 4] <- NA
  oldvalues <- c(3, 1, 2)
  newvalues <- factor(c("1decreased", "2normal", "3increased"), ordered = TRUE)
  ds$V17 <- newvalues[match(ds$V17, oldvalues)]

  # V18: abdomen
  oldvalues <- seq(5)
  newvalues <- factor(c(
  "normal",
  "other",
  "firm feces in the large intestine",
  "distended small intestine",
  "distended large intestine"
  ))
  ds$V18 <- newvalues[match(ds$V18, oldvalues)]

  # V21: abdominocentesis appearance
  oldvalues <- seq(3)
  newvalues <- factor(c("clear", "cloudy", "serosanguinous"))
  ds$V21 <- newvalues[match(ds$V21, oldvalues)]

  # For Gower
  simil.types <- list(ordratio = c("V5", "V6", "V20", "V22"))

  return(list(datasets = ds, simil.types = simil.types))
}


LoadHorseColicV2 <- function() {
  horse.colic.tr <- read.table('datasets/classification/HorseColic/horse-colic.data', sep = "", na.strings = '?')
  horse.colic.te <- read.table('datasets/classification/HorseColic/horse-colic.test', sep = "", na.strings = '?')

  ds <- rbind(horse.colic.tr, horse.colic.te)
  # Remove last variables (other possible targets)
  ds <- ds[1:24]
  colnames(ds)[24] <- "Target"
  # Remove file number and the other target variable (class V24 is now V22)
  ds <- ds[-c(3, 23)]

  # Remove instances with all values missing
  ds <- Filter(function(x)!all(is.na(x)), ds)

  # Remove instances that do not have a defined class
  ds <- ds[!is.na(ds[, "Target"]),]


  # remove useless observations (only V1 and V2 non missing)

  ds <- ds[-c(29, 216, 288),]


  # Recode target
  t.oldvalues <- c("1", "2")
  t.newvalues <- factor(c("Yes", "No"))
  ds$Target <- t.newvalues[match(ds$Target, t.oldvalues)]

  # Recode predictors appropriately

  # V1:  surgery?
  oldvalues <- c(1, 2)
  newvalues <- factor(c("Yes", "No"))
  ds$V1 <- newvalues[match(ds$V1, oldvalues)]

  # V2:  Age
  oldvalues <- c(1, 9)
  newvalues <- factor(c("Adult", "Young"))
  ds$V2 <- newvalues[match(ds$V2, oldvalues)]

  # V7:  temperature of extremities
  oldvalues <- c(4, 3, 1, 2)
  newvalues <- factor(c("Cold", "Cool", "Normal", "Warm"), ordered = TRUE)
  ds$V7 <- newvalues[match(ds$V7, oldvalues)]

  # V8:  peripheral pulse
  oldvalues <- c(4, 3, 1, 2)
  newvalues <- factor(c("1Absent", "2Reduced", "3Normal", "4Increased"), ordered = TRUE)
  ds$V8 <- newvalues[match(ds$V8, oldvalues)]

  # V9:  mucous membranes
  oldvalues <- seq(6)
  newvalues <- factor(c("normal pink","bright pink", "pale pink", "pale cyanotic", "bright red / injected", "dark cyanotic"))
  ds$V9 <- newvalues[match(ds$V9, oldvalues)]

  # V10: capillary refill time
  # WARNING! there is a couple of values '3' ??? we treat as a NA
  ds$V10[ds$V10 == 3] <- NA
  oldvalues <- c(1, 2)
  newvalues <- factor(c("< 3 seconds", ">= 3 seconds"))
  ds$V10 <- newvalues[match(ds$V10, oldvalues)]

  # V11: pain
  oldvalues <- seq(5)
  newvalues <- factor(
  c("1alert, no pain", "2depressed", "3intermittent mild pain", "4intermittent severe pain", "5continuous severe pain"), ordered = TRUE)
  ds$V11 <- newvalues[match(ds$V11, oldvalues)]

  # V12: peristalsis
  # WARNING! value '4' is documented as NA
  ds$V12[ds$V12 == 4] <- NA
  oldvalues <- c(3, 2, 1)
  newvalues <- factor(c("1hypomotile", "2normal", "3hypermotile"), ordered = TRUE)
  ds$V12 <- newvalues[match(ds$V12, oldvalues)]

  # V13: abdominal distension
  oldvalues <- seq(4)
  newvalues <- factor(c("1none", "2slight", "3moderate", "4severe"), ordered = TRUE)
  ds$V13 <- newvalues[match(ds$V13, oldvalues)]

  # V14: nasogastric tube
  oldvalues <- seq(3)
  newvalues <- factor(c("1none", "2slight", "3significant"), ordered = TRUE)
  ds$V14 <- newvalues[match(ds$V14, oldvalues)]

  # V15: nasogastric reflux
  oldvalues <- c(1, 3, 2)
  newvalues <- factor(c("1none", "2< 1 liter", "3> 1 liter"), ordered = TRUE)
  ds$V15 <- newvalues[match(ds$V15, oldvalues)]

  # V17: rectal examination - feces
  # WARNING! value '4' is documented as NA

  ds$V17[ds$V17 == 4] <- NA
  oldvalues <- c(3, 1, 2)
  newvalues <- factor(c("1decreased", "2normal", "3increased"), ordered = TRUE)
  ds$V17 <- newvalues[match(ds$V17, oldvalues)]

  # V18: abdomen
  oldvalues <- seq(5)
  newvalues <- factor(c("normal", "other", "firm feces in the large intestine", "distended small intestine", "distended large intestine"))
  ds$V18 <- newvalues[match(ds$V18, oldvalues)]

  # V21: abdominocentesis appearance
  oldvalues <- seq(3)
  newvalues <- factor(c("clear", "cloudy", "serosanguinous"))
  ds$V21 <- newvalues[match(ds$V21, oldvalues)]


  # For Gower
  simil.types <- list(ordratio = c("V5", "V6", "V20", "V22"))


  return(list(datasets = ds, simil.types = simil.types))
}



LoadDiabetis <- function() {
  ds <- read.csv('./datasets/classification/Diabetic/diabetic_data.csv', dec = '.', sep = ',', header = TRUE, na.string = '?')
  ds$encounter_id <- NULL
  ds$patient_nbr <- NULL
  ds$examide <- NULL
  ds$citoglipton <- NULL
  ds$age <- as.ordered(ds$age)
  ds$admission_source_id <- as.factor(ds$admission_source_id)
  ds$discharge_disposition_id <- as.factor(ds$discharge_disposition_id)
  # For Gower
  simil.types <- list()


  return(list(datasets = ds, simil.types = simil.types))
}

LoadAnnealing <- function() {
  ds <- read.table('./datasets/classification/Annealing/anneal.data', sep = ',', na.strings = '?')
  colnames(ds)[39] <- 'Target'
  ds$V19 <- NULL
  ds$V2 <- NULL
  ds$V23 <- NULL
  ds$V26 <- NULL
  ds$V29 <- NULL
  ds$V30 <- NULL
  ds$V31 <- NULL
  

  # For Gower
  simil.types <- list()


  return(list(datasets = ds, simil.types = simil.types))
}

LoadCensus <- function() {
  ds <- read.table('./datasets/classification/Census/census-income.data', sep = ',', na.strings = '?', strip.white= TRUE)
  colnames(ds)[42] <- 'Target'

  # For Gower
  simil.types <- list()


  return(list(datasets = ds, simil.types = simil.types))
}




############
# Other #
LoadAdultDataset <- function() {
  ds <- read.table("datasets/classification/+/Adult/adult.data", sep = ",", strip.white = TRUE, na.strings = "?")
  colnames(ds)[15] <- "Target"
  return(list(datasets = ds))
}