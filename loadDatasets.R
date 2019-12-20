library(foreign)
##############
# Regression #
##############

LoadAutomobileDS <- function(removeNATarget = TRUE) {
  ds <- read.table('./datasets/regression/Automobile/imports-85.data', sep = ',',na.strings = '?')
  colnames(ds) <- c("symboling", "normalizedlosses", "make", "fueltype", "aspiration", "numofdoors", "bodystyle", "drivewheel", "enginelocation", "wheelbase",
  "length", "width", "height", "curbweight", "enginetype", "numofcylinders", "enginesize", "fuelsystem", "bore", "stroke", "compressionratio", "horsepower",
  "peakrpm", "citympg", "highwaymp", "price")

  ds$numofcylinders <- factor(ds$numofcylinders, ordered = TRUE, levels = c("two", "three", "four", "five", "six", "eight", "twelve"))
  ds$numofdoors <- factor(ds$numofdoors, ordered = TRUE, levels = c("two","four"))

  if (removeNATarget) ds <- ds[!is.na(ds$price),]

  # For Gower
  simil.types <- list(ordratio = c("numofcylinders", "numofdoors"))
  formula <- price ~ .
  return(list(name="Automobile", dataset = ds, formula = formula , simil.types = simil.types))
}

LoadAutoMPGDS <- function(removeNATarget = TRUE) {
  ds <- read.table('./datasets/regression/AutoMPG/auto-mpg.data-original')
  colnames(ds) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "modelyear", "origin", "carname")
  
  ds$carname <- NULL

  if (removeNATarget) ds <- ds[!is.na(ds$mpg),]

  # For Gower
  simil.types <- list()
  formula <- mpg ~ .
  return(list(name = "AutoMPG", dataset = ds, formula = formula, simil.types = simil.types))
}

LoadCommunitiesDataset <- function() {
  ds <- read.table('./datasets/regression/Communities/communities.data', sep = ',', na.strings = '?')
  colnames(ds) <- c("state","county","community","communityname","fold","population","householdsize","racepctblack","racePctWhite","racePctAsian","racePctHisp","agePct12t21","agePct12t29","agePct16t24","agePct65up","numbUrban",
           "pctUrban","medIncome","pctWWage","pctWFarmSelf","pctWInvInc","pctWSocSec","pctWPubAsst","pctWRetire","medFamInc","perCapInc","whitePerCap","blackPerCap","indianPerCap","AsianPerCap","OtherPerCap","HispPerCap",
           "NumUnderPov","PctPopUnderPov","PctLess9thGrade","PctNotHSGrad","PctBSorMore","PctUnemployed","PctEmploy","PctEmplManu","PctEmplProfServ","PctOccupManu","PctOccupMgmtProf","MalePctDivorce","MalePctNevMarr","FemalePctDiv",
           "TotalPctDiv","PersPerFam","PctFam2Par","PctKids2Par","PctYoungKids2Par","PctTeen2Par","PctWorkMomYoungKids","PctWorkMom","NumIlleg","PctIlleg","NumImmig","PctImmigRecent","PctImmigRec5","PctImmigRec8","PctImmigRec10",
           "PctRecentImmig","PctRecImmig5","PctRecImmig8","PctRecImmig10","PctSpeakEnglOnly","PctNotSpeakEnglWell","PctLargHouseFam","PctLargHouseOccup","PersPerOccupHous","PersPerOwnOccHous","PersPerRentOccHous","PctPersOwnOccup",
           "PctPersDenseHous","PctHousLess3BR","MedNumBR","HousVacant","PctHousOccup","PctHousOwnOcc","PctVacantBoarded","PctVacMore6Mos","MedYrHousBuilt","PctHousNoPhone","PctWOFullPlumb","OwnOccLowQuart","OwnOccMedVal","OwnOccHiQuart",
           "RentLowQ","RentMedian","RentHighQ","MedRent","MedRentPctHousInc","MedOwnCostPctInc","MedOwnCostPctIncNoMtg","NumInShelters","NumStreet","PctForeignBorn","PctBornSameState","PctSameHouse85","PctSameCity85","PctSameState85",
           "LemasSwornFT","LemasSwFTPerPop","LemasSwFTFieldOps","LemasSwFTFieldPerPop","LemasTotalReq","LemasTotReqPerPop","PolicReqPerOffic","PolicPerPop","RacialMatchCommPol","PctPolicWhite","PctPolicBlack","PctPolicHisp",
           "PctPolicAsian","PctPolicMinor","OfficAssgnDrugUnits","NumKindsDrugsSeiz","PolicAveOTWorked","LandArea","PopDens","PctUsePubTrans","PolicCars","PolicOperBudg","LemasPctPolicOnPatr","LemasGangUnitDeploy",
           "LemasPctOfficDrugUn", "PolicBudgPerPop", "Target")
  #Remove non predictive variables
  ds$state <- NULL
  ds$county <- NULL
  ds$community <- NULL
  ds$communityname <- NULL
  ds$fold <- NULL
  #To Ordered
  ds$LemasGangUnitDeploy <- factor(ds$LemasGangUnitDeploy, ordered = TRUE)
  levels(ds$LemasGangUnitDeploy) <- c("No","PartTime", "Yes")
  
  simil.types <- list(ordratio = c("LemasGangUnitDeploy"))
  formula <- Target ~ .
  return(list(name = "Communities", dataset = ds, formula = formula, simil.types = simil.types))
}


LoadMvDataset <- function() {
  ds <- read.arff('./datasets/regression/Mv/mv.arff')
  colnames(ds) <- c(paste('X', 1:10, sep = ''), 'Target')
  levels(ds$X3) <- trimws(levels(ds$X3))
  levels(ds$X7) <- trimws(levels(ds$X7))
  levels(ds$X8) <- trimws(levels(ds$X8))

  ds$X3 <- as.factor(ds$X3)
  ds$X7 <- as.logical(ds$X7 == 'yes')
  ds$X8 <- as.factor(ds$X8)

  # For Gower
  simil.types <- list(symm=c('X7','X8'))

  return(list(name='MV', dataset = ds, simil.types = simil.types))
}

LoadWaveDataset <- function() {
  ds <- read.csv('./datasets/regression/WaveEnergy/Adelaide_Data.csv', dec = '.', sep = ',', header = FALSE)
  colnames(ds) <- c(paste('X', 1:16, sep = ''), paste('Y', 1:16, sep = ''), paste('P', 1:16, sep = ''), 'Target')

  simil.types <- list()

  formula <- Target ~ .
  return(list(name='Wave', dataset = ds, formula = formula, simil.types = simil.types))
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
  #oldvalues <- c(0.0, 1.0)
  #newvalues <- factor(c("0", "1"))
  #heart$V6 <- newvalues[match(heart$V6, oldvalues)]
  heart$V6 <- as.logical(heart$V6)

  # V7: resting electrocardiographic results  (values 0,1,2)
  oldvalues <- c(0, 1, 2)
  newvalues <- factor(c("0", "1", "2"))

  heart$V7 <- newvalues[match(heart$V7, oldvalues)]

  # V9:  exercise induced angina
  #oldvalues <- c(0.0, 1.0)
  #newvalues <- factor(c("0", "1"))
  #heart$V9 <- newvalues[match(heart$V9, oldvalues)]
  heart$V9 <- as.logical(heart$V9)

  # V11: the slope of the peak exercise ST segment
  oldvalues <- c(1, 2, 3)
  newvalues <- factor(c("1", "2", "3"), ordered = TRUE)

  heart$V11 <- newvalues[match(heart$V11, oldvalues)]

  # V13: thal: 3 = normal; 6 = fixed defect; 7 = reversable defect
  oldvalues <- c(3, 6, 7)
  newvalues <- factor(c("normal", "fixed defect", "reversable defect"))

  heart$V13 <- newvalues[match(heart$V13, oldvalues)]

  # For Gower
  simil.types <- list(ordratio = c("V11"), symm=c('V2','V6','V9'))

  formula <- Target ~ .
  return(list(name = 'Heart', dataset = ds, formula = formula, simil.types = simil.types))
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
  formula <- severity ~ .
  return(list(name = 'Mammographic', dataset = ds, formula = formula, simil.types = simil.types))
}


LoadMushroomDataset <- function() {
  ds <- read.table("datasets/classification/Mushroom/agaricus-lepiota.data", sep = ",", na.strings = "?")
  colnames(ds)[1] <- "Target"
  # Empty column
  ds$V17 <- NULL

  simil.types <- list()
  formula <- severity ~ .
  return(list(name = 'Mushroom', dataset = ds, formula = formula, simil.types = simil.types))
}

LoadHorseColic <- function(targetId) {
  horse.colic.tr <- read.table('datasets/classification/HorseColic/horse-colic.data', sep = "", na.strings = '?')
  horse.colic.te <- read.table('datasets/classification/HorseColic/horse-colic.test', sep = "", na.strings = '?')

  ds <- rbind(horse.colic.tr, horse.colic.te)

  # Remove file number
  ds <- ds[-c(3)]

  # Remove instances with all values missing
  ds <- Filter(function(x)!all(is.na(x)), ds)

  # remove useless observations (only V1 and V2 non missing)
  ds <- ds[-c(29, 216, 288),]

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
  newvalues <- factor(c("normal", "other", "firm feces in the large intestine", "distended small intestine", "distended large intestine"))
  ds$V18 <- newvalues[match(ds$V18, oldvalues)]

  # V21: abdominocentesis appearance
  oldvalues <- seq(3)
  newvalues <- factor(c("clear", "cloudy", "serosanguinous"))
  ds$V21 <- newvalues[match(ds$V21, oldvalues)]

  # For Gower
  simil.types <- list(ordratio = c("V5", "V6", "V20", "V22"))

  return(list(name = "HorseColic", dataset = ds, simil.types = simil.types))
}

LoadHorseColicV2 <- function() {
  r <- LoadHorseColic()
  ds <- r$dataset
  
  # Remove last variables (other possible targets)
  ds <- ds[c(1:21, 23)]

  colnames(ds)[22] <- "Target"
  t.oldvalues <- c("1", "2")
  t.newvalues <- factor(c("Yes", "No")) # Make this a factor
  ds$Target <- t.newvalues[match(ds$Target, t.oldvalues)]

  # Remove instances with all values missing
  ds <- Filter(function(x)!all(is.na(x)), ds)

  # Remove instances that do not have a defined class
  ds <- ds[!is.na(ds[, "Target"]),]

  r$dataset <- ds
  r$formula <- Target ~ .
  r$name <- "HorseColic2"
  r
}

LoadCensus <- function() {
  ds <- read.table('./datasets/classification/Census/census-income.data', sep = ',', na.strings = '?', strip.white= TRUE)
  colnames(ds)[42] <- 'Target'

  # For Gower
  simil.types <- list()

  formula <- Target ~ .
  return(list(name = 'Census', dataset = ds, formula = formula, simil.types = simil.types))
}


##################
# Classification (multi) #
##################

LoadAudiologyDs <- function() {
  ds <- read.table('./datasets/classification/Audiology/audiology.standardized.data', sep = ',', na.string = '?')

  for (i in c(1, 3, 7, 9:58, 61:63, 65, 67:69))
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

  # For Gower
  simil.types <- list()
  formula <- V71 ~ .
  return(list(name = 'Audiology", dataset = ds, formula = formula, simil.types = simil.types))
}

LoadHorseColicV1 <- function() {

  r <- LoadHorseColic()
  ds <- r$dataset

  # Remove last variables (other possible targets)
  ds <- ds[c(1:22)]

  colnames(ds)[22] <- "Target"
  t.oldvalues <- c("1", "2", "3")
  t.newvalues <- factor(c("Lived", "Died", "Euthanized"))
  ds$Target <- t.newvalues[match(ds$Target, t.oldvalues)]

  # Remove instances with all values missing
  ds <- Filter(function(x)!all(is.na(x)), ds)

  # Remove instances that do not have a defined class
  ds <- ds[!is.na(ds[, "Target"]),]

  r$dataset <- ds
  r$formula <- Target ~ .
  r$name <- "HorseColic1"
  r
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
  formula <- Target ~ .
  return(list(name='Annealing', dataset = ds, formula = formula, simil.types = simil.types))
}

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
  simil.types <- list()
  formula <- V10 ~ .
  return(list(name='Contraceptive', dataset = ds, formula = formula, simil.types = simil.types))
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
  formula <- readmitted ~ .
  return(list(name='Diabetis', dataset = ds, formula = formula, simil.types = simil.types))
}

############
# Other #
LoadAdultDataset <- function() {
  ds <- read.table("datasets/classification/+/Adult/adult.data", sep = ",", strip.white = TRUE, na.strings = "?")
  colnames(ds)[15] <- "Target"
  simil.types <- list()
  formula <- Target ~ .
  return(list(name='Adult', dataset = ds, formula = formula, simil.types = simil.types))
}
