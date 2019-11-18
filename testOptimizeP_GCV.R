

set.seed(1234)
s <- sample(nrow(prostate), 60)
reg.lm <- snn(lpsa ~ ., prostate, subset = s)
gSimils <- reg.lm$simil.matrix.prot
gY <- reg.lm$y

r <- optimize_p_GCV(gSimils, gY, regularization = TRUE)

library(ggplot2)


ds <- expand.grid(l=r$lambdas, p=r$ps)
ds$value <- as.vector(t(r$grid.search))
# Heatmap 

ggplot(ds, aes(l, p, fill = log(value))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") + scale_x_continuous(trans = 'log10') +
  xlab('lambda')

plot(r$grid.search[,36], type = 'l')


r2 <- optimize_p_kFoldCV(gSimils, gY)

plot(r2$ps, r2$E / var(gY), type = 'l')


longley # not the same as the S-PLUS dataset
names(longley)[1] <- "y"
r <- lm.ridge(y ~ ., longley, lambda = seq(0, 0.1, 0.001))
plot(lm.ridge(y ~ ., longley,
              lambda = seq(0, 0.1, 0.001)))