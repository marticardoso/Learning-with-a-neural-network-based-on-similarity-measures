###########################################################
# PIMA DATA READING AND PREPARATION
###########################################################

pima <- read.table ("pima.dat", sep=",")

colnames(pima) <- c("Pregnancies","Plasma","Blood","Skin","Serum","BMI","Pedigree","Age","Target")

pima$Target <- factor(pima$Target, labels=c("No","Yes"))
Target.pos <- 9

if (preproc == "sim" || preproc == "std")
{
sum(pima$Plasma==0) # 5
p <- rownames(subset(pima, Plasma==0))
pima[p,"Plasma"] <- NA

sum(pima$Blood==0) # 35
p <- rownames(subset(pima, Blood==0))
pima[p,"Blood"] <- NA

sum(pima$Skin==0) # 227
p <- rownames(subset(pima, Skin==0))
pima[p,"Skin"] <- NA

sum(pima$Serum==0) # 374
p <- rownames(subset(pima, Serum==0))
pima[p,"Serum"] <- NA

sum(pima$BMI==0) # 11
p <- rownames(subset(pima, BMI==0))
pima[p,"BMI"] <- NA
}

if (preproc == "std")
{
  # Imputing the missing values by iterated least squares regressions
  
  pima.mice <- mice(pima, m=1)
  pima <- complete(pima.mice)
}

if (preproc == "raw")
{ 
  # 0's stay as they are
}

# Proceed ...

N <- nrow(pima)

if (shuffle) { pima <- pima[sample.int(N),] }

if (preproc == "raw" || scale==TRUE) { pima[,-Target.pos] <- scale(pima[,-Target.pos]) }

# For Gower
simil.types <- list(ordratio = c("Pregnancies", "Age"))

# Split target and predictors

Targets <- pima$Target
Predictors <- as.matrix(pima[,-Target.pos])

Nlearn <- 500

dataset <- pima

learn <- 1:Nlearn
Ntest <- N - Nlearn
