
#############
# data loader
#############

EliBIN2.raw <- read.table("EliBIN2.csv", header=TRUE, sep=",", check.names=TRUE, na.strings = "?", as.is = TRUE)

# remove useless rows (all variables are missing!)
tmp <- EliBIN2.raw[-c(16,90,98), ]

# remove more useless rows (the '41s' are PIGS with some human contribution)
tmp <- subset(tmp, Origen!=41)

# convert everything to factor
tmp <- data.frame(lapply(tmp,factor))

# remove useless features

tmp <- subset(tmp, select=-c(X1,CF193,Enterococs))

# Set up target class (4 classes)
tmp$Origen <- factor(tmp$Origen, labels=c("Human","Cow", "Poultry", "Pig"))


# add a further level, with value 'missing.symbol'

missing.symbol <- '2'
tmp <- as.matrix(tmp) 
tmp[is.na(tmp)] <- missing.symbol

# Dataset with CATEGORICAL variables
EliBIN2.tmp <- as.data.frame(tmp)

# and shuffle
EliBIN2 <- EliBIN2.tmp[sample.int(nrow(EliBIN2.tmp)),]
