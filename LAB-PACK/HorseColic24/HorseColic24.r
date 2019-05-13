###########################################################
# HORSE COLIC (V24) DATA READING AND PREPARATION
###########################################################
# V24: surgical lesion?
#           - retrospectively, was the problem (lesion) surgical?
#           - all cases are either operated upon or autopsied so that
#             this value and the lesion type are always known
#           - possible values:
#                1 = Yes
#                2 = No
###########################################################

horse.colic.tr <- read.table ("horse_colic.data", sep="", na.strings = "?")
horse.colic.te <- read.table ("horse_colic.test", sep="", na.strings = "?")

horse.colic <- rbind (horse.colic.tr, horse.colic.te)
dim(horse.colic)

# Remove last variables (other possible targets)
horse.colic <- horse.colic[1:24] 

# Remove file number and the other target variable (class V24 is now V22)
horse.colic <- horse.colic[-c(3,23)]

# Remove instances with all values missing
horse.colic <- Filter(function(x)!all(is.na(x)), horse.colic)

# Remove instances that do not have a defined class
horse.colic <- horse.colic[!is.na(horse.colic[,"V24"]),]


# remove useless observations (only V1 and V2 non missing)

horse.colic <- horse.colic[-c(29,216,288),]


# Recode target
Target.pos <- 22
colnames(horse.colic)[Target.pos] <- "Target"

t.oldvalues <-        c("1", "2")
t.newvalues <- factor(c("Yes","No"))  # Make this a factor

horse.colic$Target <- t.newvalues[ match(horse.colic$Target, t.oldvalues) ]

if (preproc == "sim" || preproc == "raw")
{
# Recode predictors appropriately

# V1:  surgery?
oldvalues <-        c(1, 2)
newvalues <- factor(c("Yes","No"))

horse.colic$V1 <- newvalues[ match(horse.colic$V1, oldvalues) ]

# V2:  Age
oldvalues <-        c(1, 9)
newvalues <- factor(c("Adult","Young"))

horse.colic$V2 <- newvalues[ match(horse.colic$V2, oldvalues) ]

# V7:  temperature of extremities
oldvalues <-        c(4, 3, 1, 2)
newvalues <- factor(c("Cold","Cool","Normal","Warm"), ordered=TRUE)

horse.colic$V7 <- newvalues[ match(horse.colic$V7, oldvalues) ]

# V8:  peripheral pulse
oldvalues <-        c(4, 3, 1, 2)
newvalues <- factor(c("1Absent","2Reduced","3Normal","4Increased"), ordered=TRUE)

horse.colic$V8 <- newvalues[ match(horse.colic$V8, oldvalues) ]

# V9:  mucous membranes
oldvalues <- seq(6)
newvalues <- factor(c(
"normal pink",
"bright pink",
"pale pink",
"pale cyanotic",
"bright red / injected",
"dark cyanotic"))

horse.colic$V9 <- newvalues[ match(horse.colic$V9, oldvalues) ]

# V10: capillary refill time
# WARNING! there is a couple of values '3' ??? we treat as a NA

horse.colic$V10[horse.colic$V10==3] <- NA

oldvalues <-        c(1, 2)
newvalues <- factor(c("< 3 seconds",">= 3 seconds"))

horse.colic$V10 <- newvalues[ match(horse.colic$V10, oldvalues) ]

# V11: pain
oldvalues <-        seq(5)
newvalues <- factor(
c("1alert, no pain",
"2depressed",
"3intermittent mild pain",
"4intermittent severe pain",
"5continuous severe pain"), ordered=TRUE)

horse.colic$V11 <- newvalues[ match(horse.colic$V11, oldvalues) ]

# V12: peristalsis
# WARNING! value '4' is documented as NA

horse.colic$V12[horse.colic$V12==4] <- NA

oldvalues <- c(3,2,1)
newvalues <- factor(c("1hypomotile","2normal","3hypermotile"), ordered=TRUE)

horse.colic$V12 <- newvalues[ match(horse.colic$V12, oldvalues) ]

# V13: abdominal distension
oldvalues <- seq(4)
newvalues <- factor(c("1none","2slight","3moderate", "4severe"), ordered=TRUE)

horse.colic$V13 <- newvalues[ match(horse.colic$V13, oldvalues) ]

# V14: nasogastric tube
oldvalues <- seq(3)
newvalues <- factor(c("1none","2slight","3significant"), ordered=TRUE)

horse.colic$V14 <- newvalues[ match(horse.colic$V14, oldvalues) ]

# V15: nasogastric reflux

oldvalues <- c(1,3,2)
newvalues <- factor(c("1none","2< 1 liter","3> 1 liter"), ordered=TRUE)

horse.colic$V15 <- newvalues[ match(horse.colic$V15, oldvalues) ]

# V17: rectal examination - feces
# WARNING! value '4' is documented as NA

horse.colic$V17[horse.colic$V17==4] <- NA

oldvalues <- c(3,1,2)
newvalues <- factor(c("1decreased","2normal","3increased"), ordered=TRUE)

horse.colic$V17 <- newvalues[ match(horse.colic$V17, oldvalues) ]

# V18: abdomen
oldvalues <- seq(5)
newvalues <- factor(c(
  "normal",
  "other",
  "firm feces in the large intestine",
  "distended small intestine",
  "distended large intestine"
  ))

horse.colic$V18 <- newvalues[ match(horse.colic$V18, oldvalues) ]

# V21: abdominocentesis appearance
oldvalues <- seq(3)
newvalues <- factor(c("clear","cloudy","serosanguinous"))

horse.colic$V21 <- newvalues[ match(horse.colic$V21, oldvalues) ]
}


if (preproc == "std")
{
  # Imputing the missing values by iterated least squares regressions
  
  horse.colic.mice <- mice(horse.colic, m=1)
  horse.colic <- complete(horse.colic.mice)
}

if (preproc == "raw")
{ 
  # Imputing the missing values by a 0

  horse.colic[,-Target.pos] <- factorsNumeric (horse.colic[,-Target.pos])
  horse.colic[is.na(horse.colic)] <- 0
}


# Proceed ...

N <- nrow(horse.colic)

if (shuffle) { horse.colic <- horse.colic[sample.int(N),] }

if (preproc == "raw" || scale) { horse.colic[,-Target.pos] <- scale(horse.colic[,-Target.pos]) }


# For Gower
simil.types <- list(ordratio = c("V5","V6","V20","V22"))
  
# Split target and predictors

Targets <- horse.colic$Target
Predictors <- horse.colic[,-Target.pos]

Nlearn <- Nlearn <- nrow(horse.colic) - 68 # make test 68, as in the original data
dataset <- horse.colic

learn <- 1:Nlearn
Ntest <- N - Nlearn

