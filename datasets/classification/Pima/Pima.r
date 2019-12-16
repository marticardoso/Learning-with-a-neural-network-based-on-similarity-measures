###########################################################
# PIMA DATA READING AND PREPARATION
###########################################################

pima <- read.table ("datasets/classification/Pima/pima.dat", sep=",")

colnames(pima) <- c("Pregnancies","Plasma","Blood","Skin","Serum","BMI","Pedigree","Age","Target")

pima$Target <- factor(pima$Target, labels=c("No","Yes"))

p <- rownames(subset(pima, Plasma==0))
pima[p,"Plasma"] <- NA

p <- rownames(subset(pima, Blood==0))
pima[p,"Blood"] <- NA

p <- rownames(subset(pima, Skin==0))
pima[p,"Skin"] <- NA

p <- rownames(subset(pima, Serum==0))
pima[p,"Serum"] <- NA

p <- rownames(subset(pima, BMI==0))
pima[p,"BMI"] <- NA

# For Gower
simil.types <- list(ordratio = c("Pregnancies", "Age"))
