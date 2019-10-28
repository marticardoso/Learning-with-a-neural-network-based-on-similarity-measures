library(dplyr)
library(magrittr)
library(ggplot2)



# gradient descent implementation

# F = Objective function
# G = Gradient function
GD <- function(f, g, x= 0.1, alphaMax= 0.5,maxIter = 1000) {
  eps_f <- 1e-8
  eps_p <- 1e-6
  xtrace <- x
  ftrace <- f(x)
  for (i in 1:maxIter) {
    g_x<-g(x)
    f_x<- f(x)
    alpha <- alphaMax
    cat('Iter',i, ' , x:', x,' alpha=', alpha,' objFunc:',f_x, '\n')
    
    
    while(TRUE){
      xn <- (x - alpha * g_x)
      
      if(xn>0 && (abs(alpha * g_x) < 1)){
          f_xn <- f(xn)
          if(f_xn < f_x) break;
      }
          
      cat(' # reducing alpha:',alpha/2, '\n')
      alpha <- alpha/2
    }
    
    if(abs(xn - x) < eps_p ) {
      cat('Break by eps_p')
      cat('# x:', x, 'xn:',xn, '\n')
      cat('# f:', f_x, 'f(xn):',f_xn, '\n')
      cat('# g(x):', g_x, 'g(xn):',g(xn),'\n')
      break;
    }
    if(abs(f_x - f_xn) < eps_f ){
      cat('Break by eps_f')
      cat('# x:', x, 'xn:',xn, '\n')
      cat('# f:', f_x, 'f(xn):',f_xn, '\n')
      cat('# g(x):', g_x, 'g(xn):',g(xn),'\n')
      break;
    }
    
    x <- xn
    xtrace <- c(xtrace,x)
    ftrace <- c(ftrace,f(x))
    
  }
  list(
    "xtrace" = xtrace,
    "ftrace" = ftrace,
    "x" = x,
    "fx" = f(x)
  )
}

create_plot <- function(f, xs, title= '', add=FALSE, ylim=NULL, color="black") {
  f_xs <- sapply(xs,f)
  #Remove NA
  xs <- xs[!is.na(f_xs)]
  f_xs <- f_xs[!is.na(f_xs)]
  if(!is.null(ylim)){
    ylim[1] <- min(ylim[1], f_xs)
    ylim[2] <- max(ylim[2], f_xs)
  }
  if(add){
    lines(x = xs, y = f_xs, type = "l", ylab = 'f(x)', xlab = "x", col=color)
  }
  else{
    plot(x = xs, y = f_xs, type = "l", ylim=ylim,ylab = 'f(x)', xlab = "x", main = title, col=color)
  }
  abline(v=xs[which.min(f_xs)[1]], lty=2, col=color)
}

plot_optimization <- function(gd.res, title, color="green") {
  points(gd.res$xtrace, gd.res$ftrace, type = "b", col = color)
  abline(v=gd.res$x, lty=2,col=color)
}


#Simple example
xs <- seq(0,4,len = 100) # create some values
f <-  function(x) 1.2 * (x-2)^2 + 3.2
grad <- function(x) 1.2 * 2 * (x-2)

GD(
  f =  f,
  g = grad,
  x = 0.1, # initialisation of x
  alpha = 5, # learning rate
  maxIter = 100 # iterations
)

gdRes <- GD(f =  f,g = grad, x = 3.1, alpha = 5, maxIter = 100)
create_plot(f, xs, 'title')
plot_optimization(gdRes)


# Problem example
if(FALSE){
  gdRes <- GD(f =  gFunc,g = gGrad, x = 3.1, alpha = 10, maxIter = 100)
  create_plot(gFunc, xs, 'title')
  plot_optimization(gdRes)
  
  
  gdRes <- GD(f =  gFunc,g = gGrad, x = 0.5, alphaMax=0.1,maxIter = 100)
  
  xs <- seq(0.3,0.7,len = 100)
  xs <- xs[xs>0]
  create_plot(gFunc, xs, 'title alpha=0.1', ylim=c(11,13.5))
  create_plot(gFuncVal, xs, add=TRUE, color = "blue")
  plot_optimization(gdRes, color='green')
}

