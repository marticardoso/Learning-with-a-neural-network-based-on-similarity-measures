###########################################################
# HORSE COLIC (V23) DATA READING AND PREPARATION
###########################################################
# V23: outcome
#           - what eventually happened to the horse?
#           - possible values:
#                1 = lived
#                2 = died
#                3 = was euthanized
###########################################################

horse.colic.tr <- read.table ("datasets/HorseColic23/horse_colic.data", sep="", na.strings = "?")
horse.colic.te <- read.table ("datasets/HorseColic23/horse_colic.test", sep="", na.strings = "?")

horse.colic <- rbind (horse.colic.tr, horse.colic.te)
dim(horse.colic)

# Remove last variables (other possible targets)
horse.colic <- horse.colic[1:23] 

# Remove file number (class V23 is now in V22)
horse.colic <- horse.colic[-3]

# Remove instances with all values missing
horse.colic <- Filter(function(x)!all(is.na(x)), horse.colic)

# Remove instances that do not have a defined class
horse.colic <- horse.colic[!is.na(horse.colic[,"V23"]),]

# remove useless observations (only V1 and V2 non missing)

horse.colic <- horse.colic[-c(29,216,288),]


# Recode target
Target.pos <- 22
colnames(horse.colic)[Target.pos] <- "Target"

t.oldvalues <-        c("1", "2", "3")
t.newvalues <- factor(c("Lived","Died", "Euthanized"))  # Make this a factor

horse.colic$Target <- t.newvalues[ match(horse.colic$Target, t.oldvalues) ]

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


N <- nrow(horse.colic)

 horse.colic <- horse.colic[sample.int(N),] 

# For Gower
simil.types <- list(ordratio = c("V5","V6","V20","V22"))