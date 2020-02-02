###########################################################
# HEART DATA READING AND PREPARATION
###########################################################
# This database contains 13 attributes (which have been extracted from
# a larger set of 75)
###########################################################

heart <- read.table ("heart.dat", sep="", na.strings = "?")

dim(heart)

heart <- heart[!is.na(heart[,"V14"]),]

# Recode target
Target.pos <- 14
colnames(heart)[Target.pos] <- "Target"

t.oldvalues <-        c("1", "2")
t.newvalues <- factor(c("Absence","Presence"))  # Make this a factor

heart$Target <- t.newvalues[ match(heart$Target, t.oldvalues) ]

if (preproc == "sim" || preproc == "std")
{ # Recode predictors appropriately

# V2:  Sex
oldvalues <-        c(0.0,1.0)
newvalues <- factor(c("0","1"))

heart$V2 <- newvalues[ match(heart$V2, oldvalues) ]

# V3: chest pain type  (4 values)

oldvalues <-        c(1.0,2.0,3.0,4.0)
newvalues <- factor(c("1","2","3","4"))

heart$V3 <- newvalues[ match(heart$V3, oldvalues) ]

# V6: fasting blood sugar > 120 mg/dl
oldvalues <-        c(0.0,1.0)
newvalues <- factor(c("0","1"))

heart$V6 <- newvalues[ match(heart$V6, oldvalues) ]

# V7: resting electrocardiographic results  (values 0,1,2)
oldvalues <-        c(0,1,2)
newvalues <- factor(c("0","1","2"))

heart$V7 <- newvalues[ match(heart$V7, oldvalues) ]

# V9:  exercise induced angina
oldvalues <-        c(0.0,1.0)
newvalues <- factor(c("0","1"))

heart$V9 <- newvalues[ match(heart$V9, oldvalues) ]

# V11: the slope of the peak exercise ST segment
oldvalues <-        c(1,2,3)
newvalues <- factor(c("1","2","3"), ordered=TRUE)

heart$V11 <- newvalues[ match(heart$V11, oldvalues) ]

# V13: thal: 3 = normal; 6 = fixed defect; 7 = reversable defect
oldvalues <-        c(3,6,7)
newvalues <- factor(c("normal","fixed defect","reversable defect"))

heart$V13 <- newvalues[ match(heart$V13, oldvalues) ]
}

# Heart has no missing values
if (preproc == "std")
{

}

if (preproc == "raw")
{ 
  heart[,-Target.pos] <- factorsNumeric (heart[,-Target.pos])
}

# Proceed ...

N <- nrow(heart)

if (shuffle) { heart <- heart[sample.int(N),] }

if (preproc == "raw" || scale) { heart[,-Target.pos] <- scale(heart[,-Target.pos]) }


# For Gower
simil.types <- list(ordratio = c("V1","V4","V5","V8","V10","V12"))

# Split target and predictors

Targets <- heart$Target
Predictors <- heart[,-Target.pos]

Nlearn <- 200
dataset <- heart

learn <- 1:Nlearn
Ntest <- N - Nlearn

