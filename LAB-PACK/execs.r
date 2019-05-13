preproc <- "sim"
#preproc <- "raw"
#preproc <- "std"

SVM.C <- 1

datasets <- c('Pima',
              'HorseColic23',
              'HorseColic24',
              'Heart',
              'Audiology')

methods <- c("LogReg","Multinom","SVM","LDA")


# prepare the structure to store the partial results (test predictions)

te.results <- matrix (rep(0,length(methods)*length(datasets)),nrow=length(datasets))
colnames (te.results) <- methods
rownames (te.results) <- datasets


for (d in 1:length(datasets))
  for (m in 1:length(methods))
  {
    te.results[d,m] <- proceed (preproc=preproc, 
                                dataset.name=datasets[d], 
                                method=methods[m], 
                                clust.metric="manhattan", clust.stand=FALSE, 
                                resampling="straight", 
                                classical=FALSE)  
  }

# remove non-sense method/dataset combinations

te.results["HorseColic23","LogReg"] <- NA
te.results["Audiology","LogReg"] <- NA

# add averages by method

(te.results <- rbind(te.results, AVERAGE=apply(te.results,2,mean,na.rm=TRUE)))

# generate LaTeX table, if needed for a report

xtable(te.results, digits=3)

# generate multiple barplot (averages in BLACK)

barplot(te.results, beside = TRUE,
        col = c(terrain.colors(nrow(te.results)-1),"black"),
        legend = rownames(te.results), ylim = c(0, 1))
abline(h = c(0.15,0.2,0.25),lty=2)
title(main = "TEST errors")
