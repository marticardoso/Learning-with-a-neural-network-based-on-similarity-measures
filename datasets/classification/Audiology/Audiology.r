###########################################################
# AUDIOLOGY DATA READING AND PREPARATION
###########################################################

audiology.tr <- read.table ("audiology.standardized.data", sep=",", na.strings = "?")
audiology.te <- read.table ("audiology.standardized.test", sep=",", na.strings = "?")

audiology <- rbind (audiology.tr, audiology.te)
dim(audiology)

# Remove identifier
audiology <- audiology[-70]

# Remove non-informative variables
audiology <- audiology[-c(8,9,12,13,16,20,21,22,23,24,28,29,30,31,32,33,34,35,36,41,42,43,44,45,46,47,48,49,50,51,52,55,56,61,63,67,68,69)]

# Remove instances with all values missing
audiology <- Filter(function(x)!all(is.na(x)), audiology)

# Remove instances that do not have a defined class
audiology <- audiology[!is.na(audiology[,"V71"]),]

# Recode target: from to 24 to 4 classes

Target.pos <- 32
colnames(audiology)[Target.pos] <- "Target"

t.oldvalues <-  c(
"cochlear_age",
"cochlear_age_and_noise"                 ,
"cochlear_age_plus_poss_menieres"        ,
"cochlear_noise_and_heredity"            ,
"cochlear_poss_noise"                    ,
"cochlear_unknown"                       ,
"mixed_cochlear_age_fixation"            ,
"mixed_cochlear_age_otitis_media"         ,
"mixed_cochlear_age_s_om"                 ,
"mixed_cochlear_unk_discontinuity"        ,
"mixed_cochlear_unk_fixation"             ,
"mixed_cochlear_unk_ser_om"               ,
"mixed_poss_central_om"                   ,
"mixed_poss_noise_om"                     ,
"normal_ear"                              ,
"otitis_media"                            ,
"poss_central"                            ,
"possible_brainstem_disorder"             ,
"possible_menieres"                       ,
"retrocochlear_unknown"                   ,
"acoustic_neuroma"                        ,
"bells_palsy"                             ,
"conductive_discontinuity"                ,
"conductive_fixation"                     
)

t.newvalues <- factor (c(rep("Cochlear",6), rep("Mixed",8), "Normal", rep("Other",9)))  # Make this a factor

audiology$Target <- t.newvalues[ match(audiology$Target, t.oldvalues) ]

if (preproc == "sim" || preproc == "std")
{
# Recode predictors appropriately

# V2:  air()

audiology$V2 <- ordered(audiology$V2)

# V4:  ar_c()
oldvalues <-        c("absent","normal","elevated")
newvalues <- factor(c("1Absent","2Normal","3Elevated"), ordered=TRUE)

audiology$V4 <- newvalues[ match(audiology$V4, oldvalues) ]

# V5:  ar_u()
oldvalues <-        c("absent","normal","elevated")
newvalues <- factor(c("1Absent","2Normal","3Elevated"), ordered=TRUE)

audiology$V5 <- newvalues[ match(audiology$V5, oldvalues) ]

# V6: bone()
# WARNING: there  is a value 'unmeasured', which we also code as NA

audiology$V6[audiology$V6=="unmeasured"] <- NA

audiology$V6 <- ordered(audiology$V6)

# V59:  o_ar_c()
oldvalues <-        c("absent","normal","elevated")
newvalues <- factor(c("1Absent","2Normal","3Elevated"), ordered=TRUE)

audiology$V59 <- newvalues[ match(audiology$V59, oldvalues) ]

# V60:  o_ar_u()
oldvalues <-        c("absent","normal","elevated")
newvalues <- factor(c("1Absent","2Normal","3Elevated"), ordered=TRUE)

audiology$V60 <- newvalues[ match(audiology$V60, oldvalues) ]

# V64:  speech()
# WARNING: same as before

audiology$V64[audiology$V64=="unmeasured"] <- NA

oldvalues <-        c("very_poor","poor","normal","good","very_good")
newvalues <- factor(c("1very_poor","2poor","3normal","4good","5very_good"), ordered=TRUE)

audiology$V64 <- newvalues[ match(audiology$V64, oldvalues) ]
}

if (preproc == "std")
{
  # Imputing the missing values by iterated least squares regressions
  
  audiology.mice <- mice(audiology, m=1)
  audiology <- complete(audiology.mice)
}

if (preproc == "raw")
{ 
  # Imputing the missing values by a 0

  audiology[,-Target.pos] <- factorsNumeric (audiology[,-Target.pos])
  audiology[is.na(audiology)] <- 0
}

# Proceed ...

N <- nrow(audiology)

if (shuffle) { audiology <- audiology[sample.int(N),] }

if (preproc == "raw" || scale) { audiology[,-Target.pos] <- scale(audiology[,-Target.pos]) }

# For Gower
simil.types <- list() 

# Split target and predictors

Targets <- audiology$Target
Predictors <- audiology[,-Target.pos]

Nlearn <- 200
dataset <- audiology

learn <- 1:Nlearn
Ntest <- N - Nlearn
