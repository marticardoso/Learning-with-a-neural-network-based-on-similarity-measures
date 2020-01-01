library(foreign)
##############
# Regression #
##############

LoadAutomobileDS <- function(removeNATarget = TRUE) {
  ds <- read.table('./datasets/regression/Automobile/imports-85.data', sep = ',', na.strings = '?')
  colnames(ds) <- c("symboling", "normalizedlosses", "make", "fueltype", "aspiration", "numofdoors", "bodystyle", "drivewheel", "enginelocation", "wheelbase",
  "length", "width", "height", "curbweight", "enginetype", "numofcylinders", "enginesize", "fuelsystem", "bore", "stroke", "compressionratio", "horsepower",
  "peakrpm", "citympg", "highwaymp", "price")

  ds$numofcylinders <- factor(ds$numofcylinders, ordered = TRUE, levels = c("two", "three", "four", "five", "six", "eight", "twelve"))
  ds$numofdoors <- factor(ds$numofdoors, ordered = TRUE, levels = c("two", "four"))
  ds$symboling <- factor(ds$symboling, ordered = TRUE)
  if (removeNATarget) ds <- ds[!is.na(ds$price),]

  # For Gower
  simil.types <- list(ordratio = c("symboling", "numofcylinders", "numofdoors"))
  formula <- price ~ .
  return(list(name = "Automobile", dataset = ds, formula = formula, simil.types = simil.types))
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
  colnames(ds) <- c("state", "county", "community", "communityname", "fold", "population", "householdsize", "racepctblack", "racePctWhite", "racePctAsian", "racePctHisp", "agePct12t21", "agePct12t29", "agePct16t24", "agePct65up", "numbUrban",
           "pctUrban", "medIncome", "pctWWage", "pctWFarmSelf", "pctWInvInc", "pctWSocSec", "pctWPubAsst", "pctWRetire", "medFamInc", "perCapInc", "whitePerCap", "blackPerCap", "indianPerCap", "AsianPerCap", "OtherPerCap", "HispPerCap",
           "NumUnderPov", "PctPopUnderPov", "PctLess9thGrade", "PctNotHSGrad", "PctBSorMore", "PctUnemployed", "PctEmploy", "PctEmplManu", "PctEmplProfServ", "PctOccupManu", "PctOccupMgmtProf", "MalePctDivorce", "MalePctNevMarr", "FemalePctDiv",
           "TotalPctDiv", "PersPerFam", "PctFam2Par", "PctKids2Par", "PctYoungKids2Par", "PctTeen2Par", "PctWorkMomYoungKids", "PctWorkMom", "NumIlleg", "PctIlleg", "NumImmig", "PctImmigRecent", "PctImmigRec5", "PctImmigRec8", "PctImmigRec10",
           "PctRecentImmig", "PctRecImmig5", "PctRecImmig8", "PctRecImmig10", "PctSpeakEnglOnly", "PctNotSpeakEnglWell", "PctLargHouseFam", "PctLargHouseOccup", "PersPerOccupHous", "PersPerOwnOccHous", "PersPerRentOccHous", "PctPersOwnOccup",
           "PctPersDenseHous", "PctHousLess3BR", "MedNumBR", "HousVacant", "PctHousOccup", "PctHousOwnOcc", "PctVacantBoarded", "PctVacMore6Mos", "MedYrHousBuilt", "PctHousNoPhone", "PctWOFullPlumb", "OwnOccLowQuart", "OwnOccMedVal", "OwnOccHiQuart",
           "RentLowQ", "RentMedian", "RentHighQ", "MedRent", "MedRentPctHousInc", "MedOwnCostPctInc", "MedOwnCostPctIncNoMtg", "NumInShelters", "NumStreet", "PctForeignBorn", "PctBornSameState", "PctSameHouse85", "PctSameCity85", "PctSameState85",
           "LemasSwornFT", "LemasSwFTPerPop", "LemasSwFTFieldOps", "LemasSwFTFieldPerPop", "LemasTotalReq", "LemasTotReqPerPop", "PolicReqPerOffic", "PolicPerPop", "RacialMatchCommPol", "PctPolicWhite", "PctPolicBlack", "PctPolicHisp",
           "PctPolicAsian", "PctPolicMinor", "OfficAssgnDrugUnits", "NumKindsDrugsSeiz", "PolicAveOTWorked", "LandArea", "PopDens", "PctUsePubTrans", "PolicCars", "PolicOperBudg", "LemasPctPolicOnPatr", "LemasGangUnitDeploy",
           "LemasPctOfficDrugUn", "PolicBudgPerPop", "Target")
  #Remove non predictive variables
  ds$state <- NULL
  ds$county <- NULL
  ds$community <- NULL
  ds$communityname <- NULL
  ds$fold <- NULL
  #To Ordered
  ds$LemasGangUnitDeploy <- factor(ds$LemasGangUnitDeploy, ordered = TRUE)
  levels(ds$LemasGangUnitDeploy) <- c("No", "PartTime", "Yes")

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
  simil.types <- list(symm = c('X7'))
  formula <- Target ~ .
  return(list(name = 'MV', dataset = ds, formula = formula, simil.types = simil.types))
}

LoadWaveDataset <- function() {
  ds1 <- read.csv('./datasets/regression/WaveEnergy/Adelaide_Data.csv', dec = '.', sep = ',', header = FALSE)
  ds2 <- read.csv('./datasets/regression/WaveEnergy/Perth_Data.csv', dec = '.', sep = ',', header = FALSE)
  ds3 <- read.csv('./datasets/regression/WaveEnergy/Sydney_Data.csv', dec = '.', sep = ',', header = FALSE)
  ds4 <- read.csv('./datasets/regression/WaveEnergy/Tasmania_Data.csv', dec = '.', sep = ',', header = FALSE)
  ds <- rbind(ds1,ds2,ds3,ds4)
  colnames(ds) <- c(paste('X', 1:16, sep = ''), paste('Y', 1:16, sep = ''), paste('P', 1:16, sep = ''), 'Target')

  simil.types <- list()

  formula <- Target ~ .
  return(list(name = 'Wave', dataset = ds, formula = formula, simil.types = simil.types))
}

LoadRegressionProblems <- function() {
  d1 <- LoadAutomobileDS()
  d2 <- LoadAutoMPGDS()
  d3 <- LoadCommunitiesDataset()
  d4 <- LoadMvDataset()
  d5 <- LoadWaveDataset()
  list(d1, d2, d3, d4, d5)
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
  #oldvalues <- c(0.0, 1.0)
  #newvalues <- factor(c("0", "1"))
  #heart$V2 <- newvalues[match(heart$V2, oldvalues)]
  heart$V2 <- as.logical(heart$V2)

  # V3: chest pain type  (4 values)

  oldvalues <- c(1.0, 2.0, 3.0, 4.0)
  newvalues <- factor(c("1", "2", "3", "4"))

  heart$V3 <- newvalues[match(heart$V3, oldvalues)]

  # V6: fasting blood sugar > 120 mg/dl
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
  simil.types <- list(ordratio = c("V11"), symm = c('V2', 'V6'), asymm=c('V9'))

  formula <- Target ~ .
  return(list(name = 'Heart', dataset = heart, formula = formula, simil.types = simil.types))
}


LoadMammographicDataset <- function() {
  ds <- read.table("datasets/classification/Mammographic/mammographic_masses.data", sep = ",", na.strings = "?")

  colnames(ds) <- c('birads', 'age', 'shape', 'margin', 'density', 'severity')

  ds$birads[(!is.na(ds$birads)) & ds$birads == 55] <- 5
  ds$birads <- NULL
  # Recode variables

  ds$severity <- as.factor(ds$severity)
  names(ds$severity) <- c("benign", "malignant")

  ds$shape <- as.factor(ds$shape)
  names(ds$shape) <- c("round", "oval", "lobular", "irregular")


  ds$margin <- as.factor(ds$margin)
  names(ds$margin) <- c("circumscribed", "microlobulated", "obscured", "illdefined", "spiculated")

  ds$density <- as.ordered(ds$density)
  names(ds$density) <- c("high", "iso", "low", "fat")


  # For Gower
  simil.types <- list(ordratio = c("density"))
  formula <- severity ~ .
  return(list(name = 'Mammographic', dataset = ds, formula = formula, simil.types = simil.types))
}

LoadPimaDataset <- function() {
  ds <- read.table("datasets/classification/Pima/pima.dat", sep = ",")
  colnames(ds) <- c("Pregnancies", "Plasma", "Blood", "Skin", "Serum", "BMI", "Pedigree", "Age", "Target")
  ds$Target <- factor(ds$Target, labels = c("No", "Yes"))

  # Missing values
  ds[ds$Plasma == 0, "Plasma"] <- NA # 5
  ds[ds$Blood == 0, "Blood"] <- NA # 35
  ds[ds$Skin == 0, "Skin"] <- NA # 227
  ds[ds$Serum == 0, "Serum"] <- NA # 374
  ds[ds$BMI == 0, "BMI"] <- NA # 11

  simil.types <- list() #list(ordratio = c("Pregnancies", "Age"))
  formula <- Target ~ .
  return(list(name = 'Pima', dataset = ds, formula = formula, simil.types = simil.types))
}

LoadMushroomDataset <- function() {
  ds <- read.table("datasets/classification/Mushroom/agaricus-lepiota.data", sep = ",", na.strings = "?")
  colnames(ds)[1] <- "Target"
  # Empty column
  ds$V17 <- NULL
  ds$V5 <- as.logical(ds$V5 == 't')
  ds$V9 <- as.logical(ds$V9 == 'b')
  ds$V11 <- as.logical(ds$V11 == 't')
  ds$V19 <- as.numeric(ds$V19)-1
  simil.types <- list(symm = c('V9', 'V11'), asymm=c('V5'))
  formula <- Target ~ .
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
  #oldvalues <- c(1, 2)
  #newvalues <- factor(c("Yes", "No"))
  #ds$V1 <- newvalues[match(ds$V1, oldvalues)]
  ds$V1 <- as.logical(ds$V1 == 2)

  # V2:  Age
  #oldvalues <- c(1, 9)
  #newvalues <- factor(c("Adult", "Young"))
  #ds$V2 <- newvalues[match(ds$V2, oldvalues)]
  ds$V2 <- as.logical(ds$V2 == 1)

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
  #oldvalues <- c(1, 2)
  #newvalues <- factor(c("< 3 seconds", ">= 3 seconds"))
  #ds$V10 <- newvalues[match(ds$V10, oldvalues)]
  ds$V10 <- as.logical(ds$V10 == 1)

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
  simil.types <- list(ordratio = c('V7', 'V8', 'V11', 'V12', 'V13', 'V14', 'V15', 'V17'), symm = c('V1', 'V2', 'V10'))

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
  ds <- read.table('./datasets/classification/Census/census-income.data', sep = ',', na.strings = '?', strip.white = TRUE)
  colnames(ds) <- c("AAGE", "ACLSWKR", "ADTIND", "ADTOCC", "AHGA", "AHRSPAY", "AHSCOL", "AMARITL", "AMJIND", "AMJOCC", "ARACE", "AREORGN", "ASEX", "AUNMEM", "AUNTYPE",  "AWKSTAT", "CAPGAIN", "CAPLOSS", "DIVVAL", "FILESTAT", "GRINREG", "GRINST", "HHDFMX", "HHDREL", "MARSUPWT", "MIGMTR1", "MIGMTR3", "MIGMTR4", "MIGSAME", "MIGSUN", "NOEMP", "PARENT", "PEFNTVTY", "PEMNTVTY", "PENATVTY", "PRCITSHP", "SEOTR", "VETQVA", "VETYN", "WKSWORK", "YEAR", "Target")

  
  ds$ACLSWKR[ds$ACLSWKR == "Not in universe"] <- NA
  ds$PARENT[ds$PARENT == "Not in universe"] <- NA
  ds$AHSCOL[ds$AHSCOL == "Not in universe"] <- NA
  ds$AMJOCC[ds$AMJOCC == "Not in universe"] <- NA
  ds$AUNMEM[ds$AUNMEM == "Not in universe"] <- NA
  ds$AUNTYPE[ds$AUNTYPE == "Not in universe"] <- NA
  ds$VETQVA[ds$VETQVA == "Not in universe"] <- NA
  ds$GRINREG[ds$GRINREG == "Not in universe"] <- NA
  ds$GRINST[ds$GRINST == "Not in universe"] <- NA
  ds$MIGMTR1[ds$MIGMTR1 == "Not in universe"] <- NA
  ds$MIGMTR3[ds$MIGMTR3 == "Not in universe"] <- NA
  ds$MIGMTR4[ds$MIGMTR4 == "Not in universe"] <- NA
  ds$MIGSUN[ds$MIGSUN == "Not in universe"] <- NA

  ds$ADTIND <- as.factor(ds$ADTIND)
  ds$ADTOCC <- as.factor(ds$ADTOCC)
  ds$ASEX <- as.logical(ds$ASEX=='Male')
  ds$AUNMEM <- as.logical(ds$AUNMEM == 'Yes')
  ds$MIGSUN <- as.logical(ds$MIGSUN == 'Yes')
  ds$SEOTR <- as.factor(ds$SEOTR)
  ds$VETQVA <- as.logical(ds$VETQVA == "Yes")
  ds$VETYN <- as.factor(ds$VETYN)
  ds$YEAR <- as.logical(ds$YEAR == 95)
  # For Gower
  simil.types <- list(symm = c("ASEX", "MIGSUN", "AUNMEM", "VETQVA", "YEAR"))

  formula <- Target ~ .
  return(list(name = 'Census', dataset = ds, formula = formula, simil.types = simil.types))
}

LoadBinClassProblems <- function() {
  d1 <- LoadHeartDataset()
  d2 <- LoadHorseColicV2()
  d3 <- LoadPimaDataset()
  d4 <- LoadMammographicDataset()
  d5 <- LoadMushroomDataset()
  d6 <- LoadCensus()
  list(d1, d3, d2, d4, d5,d6)
}

##################
# Classification (multi) #
##################

LoadAudiologyDs <- function() {
  audiology.tr <- read.table("./datasets/classification/Audiology/audiology.standardized.data", sep = ",", na.strings = "?")
  audiology.te <- read.table("./datasets/classification/Audiology/audiology.standardized.test", sep = ",", na.strings = "?")
  ds <- rbind(audiology.tr, audiology.te)

  # Remove identifier
  ds <- ds[-70]

  # Remove non-informative variables
  ds <- ds[-c(8, 9, 12, 13, 16, 20, 21, 22, 23, 24, 28, 29, 30, 31, 32, 33, 34, 35, 36, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 55, 56, 61, 63, 67, 68, 69)]

  # Remove instances with all values missing
  ds <- Filter(function(x)!all(is.na(x)), ds)

  # Remove instances that do not have a defined class
  ds <- ds[!is.na(ds[, "V71"]),]


  colnames(ds)[32] <- "Target"

  t.oldvalues <- c("cochlear_age", "cochlear_age_and_noise", "cochlear_age_plus_poss_menieres", "cochlear_noise_and_heredity", "cochlear_poss_noise", "cochlear_unknown", "mixed_cochlear_age_fixation", "mixed_cochlear_age_otitis_media", "mixed_cochlear_age_s_om", "mixed_cochlear_unk_discontinuity", "mixed_cochlear_unk_fixation", "mixed_cochlear_unk_ser_om",
    "mixed_poss_central_om", "mixed_poss_noise_om", "normal_ear", "otitis_media", "poss_central", "possible_brainstem_disorder", "possible_menieres", "retrocochlear_unknown", "acoustic_neuroma", "bells_palsy", "conductive_discontinuity", "conductive_fixation")
  t.newvalues <- factor(c(rep("Cochlear", 6), rep("Mixed", 8), "Normal", rep("Other", 9))) # Make this a factor
  ds$Target <- t.newvalues[match(ds$Target, t.oldvalues)]

  # Recode predictors appropriately

  # V2:  air()
  ds$V2 <- ordered(ds$V2)

  # V4:  ar_c()
  oldvalues <- c("absent", "normal", "elevated")
  newvalues <- factor(c("1Absent", "2Normal", "3Elevated"), ordered = TRUE)
  ds$V4 <- newvalues[match(ds$V4, oldvalues)]

  # V5:  ar_u()
  oldvalues <- c("absent", "normal", "elevated")
  newvalues <- factor(c("1Absent", "2Normal", "3Elevated"), ordered = TRUE)
  ds$V5 <- newvalues[match(ds$V5, oldvalues)]

  # V6: bone()
  # WARNING: there  is a value 'unmeasured', which we also code as NA
  ds$V6[ds$V6 == "unmeasured"] <- NA
  ds$V6 <- ordered(ds$V6)

  # V59:  o_ar_c()
  oldvalues <- c("absent", "normal", "elevated")
  newvalues <- factor(c("1Absent", "2Normal", "3Elevated"), ordered = TRUE)
  ds$V59 <- newvalues[match(ds$V59, oldvalues)]

  # V60:  o_ar_u()
  oldvalues <- c("absent", "normal", "elevated")
  newvalues <- factor(c("1Absent", "2Normal", "3Elevated"), ordered = TRUE)
  ds$V60 <- newvalues[match(ds$V60, oldvalues)]

  # V64:  speech()
  ds$V64[ds$V64 == "unmeasured"] <- NA
  oldvalues <- c("very_poor", "poor", "normal", "good", "very_good")
  newvalues <- factor(c("1very_poor", "2poor", "3normal", "4good", "5very_good"), ordered = TRUE)
  ds$V64 <- newvalues[match(ds$V64, oldvalues)]

  for (i in c(1, 3, 7:25, 28, 30))
    ds[, i] <- as.logical(ds[, i] == 't')
  

  # For Gower
  simil.types <- list(ordratio = c("V2", "V4", "V5", "V6", "V59", "V60", "V64"), asymm = paste("V", c(1, 3, 7, 10, 11, 14, 15, 17, 18, 19, 25, 26, 27, 37, 38, 39, 40, 53, 54, 57, 58, 62, 65), sep = ""))

  formula <- Target ~ .
  return(list(name = 'Audiology', dataset = ds, formula = formula, simil.types = simil.types))
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

LoadGlass <- function() {
  ds <- read.table('./datasets/classification/Glass/glass.data', sep = ',', na.strings = '?')
  colnames(ds) <- c('Id', 'RI', 'Na', 'Mg', 'Al', 'Si', 'K', 'Ca', 'Ba', 'Fe', 'Target')
  ds$Id <- NULL
  ds$Target <- as.factor(ds$Target)

  # For Gower
  simil.types <- list()
  formula <- Target ~ .
  return(list(name = 'Glass',  dataset = ds, formula = formula, simil.types = simil.types))
}


LoadAnnealing <- function() {
  ds <- read.table('./datasets/classification/Annealing/anneal.data', sep = ',', na.strings = '?')
  colnames(ds)[39] <- 'Target'
  ds$V2 <- NULL
  ds$V19 <- NULL
  ds$V23 <- NULL
  ds$V26 <- NULL
  ds$V29 <- NULL
  ds$V30 <- NULL
  ds$V31 <- NULL

  ds$V10 <- as.logical(ds$V10 == 'N')
  ds$V11 <- as.logical(ds$V11 == 'P')

  for (col in paste('V',c(14:16, 18, 22,24:25, 28, 36),sep='')) {
    ds[, col] <- as.logical(ds[, col] == 'Y')
  }
  ds$V17 <- as.logical(ds$V17 == 'B')
  ds$V20 <- as.logical(ds$V20 == 'C')
  ds$V21 <- as.logical(ds$V21 == 'P')
  ds$V32 <- as.logical(ds$V32 == 'SHEET')

  ds$V37 <- as.ordered(ds$V37)
  # For Gower
  simil.types <- list(ordratio = c("V37"), symm = paste('V', c(11, 17, 32, 36), sep = ''), asymm = paste('V', c(6, 10, 14:16, 18, 20, 21, 22, 24, 25, 28), sep = ''))
  formula <- Target ~ .
  return(list(name = 'Annealing', dataset = ds, formula = formula, simil.types = simil.types))
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
  simil.types <- list(ordratio=c('V2','V3','V8'), symm = c('V5', 'V6', 'V9'))
  formula <- V10 ~ .
  return(list(name = 'Contraceptive', dataset = ds, formula = formula, simil.types = simil.types))
}

LoadDiabetis <- function() {
  ds <- read.csv('./datasets/classification/Diabetic/diabetic_data.csv', dec = '.', sep = ',', header = TRUE, na.string = '?')
  ds$encounter_id <- NULL
  ds$patient_nbr <- NULL
  ds$examide <- NULL
  ds$citoglipton <- NULL

  ds$gender[ds$gender == 'Unknown/Invalid'] <- NA
  ds$gender <- as.logical(ds$gender == 'Male')

  ds$age <- as.ordered(ds$age)
  ds$weight <- factor(ds$weight, levels = c("[0-25)", "[25-50)", "[50-75)", "[75-100)", "[100-125)", "[125-150)", "[150-175)", "[175-200)", ">200"), ordered = TRUE)

  ds$admission_type_id <- as.factor(ds$admission_type_id)
  ds$discharge_disposition_id <- as.factor(ds$discharge_disposition_id)
  ds$admission_source_id <- as.factor(ds$admission_source_id)
  ds$diag_1 <- as.factor(ds$diag_1)
  ds$diag_2 <- as.factor(ds$diag_2)
  ds$diag_3 <- as.factor(ds$diag_3)

  ds$max_glu_serum[ds$max_glu_serum == 'None'] <- NA
  ds$max_glu_serum <- factor(ds$max_glu_serum, levels = c('Norm', '>200', '>300'), ordered = TRUE)

  ds$A1Cresult[ds$A1Cresult == 'None'] <- NA
  ds$A1Cresult <- factor(ds$A1Cresult, levels = c('Norm', '>7', '>8'), ordered = TRUE)

  ds$change <- as.logical(ds$change == 'Ch')
  ds$diabetesMed <- as.logical(ds$diabetesMed == 'Yes')

  for (i in 23:43) {
    ds[,i] <- factor(ds[,i], levels = c('No','Down', 'Steady','Up'), ordered = TRUE)
  }
  # For Gower
  simil.types <- list(ordratio = c('age', 'weight', 'max_glu_serum', 'A1Cresult', colnames(ds)[23:43]), symm = c('gender'), asymm = c('diabetesMed', 'change'))
  formula <- readmitted ~ .
  return(list(name = 'Diabetis', dataset = ds, formula = formula, simil.types = simil.types))
}

LoadMultiClassProblems <- function() {
  d1 <- LoadAudiologyDs()
  d2 <- LoadGlass()
  d3 <- LoadHorseColicV1()
  d4 <- LoadAnnealing()
  d5 <- LoadContraceptiveDs()
  d6 <- LoadDiabetis()
  list(d1, d2, d3, d4, d5, d6)
}

