library(dplyr)
library(magrittr)
library(ggplot2)



# gradient descent implementation

# F = Objective function
# G = Gradient function
# CONTROL
# - alpha = learning rate
# - eps_p <- stop when p_(t+1) - p_t < eps_p
# - eps_f <- stop when f(p) - f(p+1) < eps_f
# - eps_g <- stop when g(p) < eps_g

GD <- function(f, g, x= 0.1, control) {
  eps_f <- 1e-8
  eps_p <- 1e-5
  eps_g <- 1e-8
  alphaMax <- 1
  maxIter <- 100
  if(!is.null(control)){
    if(!is.null(control$alpha)) alphaMax <- control$alpha
    if(!is.null(control$maxIter)) maxIter <- control$maxIter
    if(!is.null(control$eps_f)) eps_f <- control$eps_f
    if(!is.null(control$eps_p)) eps_p <- control$eps_p
    if(!is.null(control$eps_g)) eps_g <- control$eps_g
  }
  
  xtrace <- x
  ftrace <- f(x)
  g_x <- g(x)
  f_x<- f(x)
  continue <- TRUE
  for (i in 1:maxIter) {
    
    alpha <- alphaMax
    cat('Iter',i, ' , x:', x,' alpha=', alpha,' objFunc:',f_x, '\n')
    
    while(continue){
      xn <- (x - alpha * g_x)
      
      if(xn>0 && (abs(alpha * g_x) < 1)){
          f_xn <- f(xn)
          if(f_xn < f_x) break;
      }
      
      if(alphaMax == alpha) cat('# reducing alpha: ', alpha/2)
      else cat(' > ', alpha/2)
      
      alpha <- alpha/2
    }
    if(alpha < alphaMax) cat('\n')
    g_xn <- g(xn)
    
    if(abs(xn - x) < eps_p ) {
      cat('Break by eps_p\n')
      cat('# x:', x, 'xn:',xn, '\n')
      cat('# f:', f_x, 'f(xn):',f_xn, '\n')
      cat('# g(x):', g_x, 'g(xn):',g_xn,'\n')
      continue <- FALSE
    }
    else if(sign(g_xn)==sign(g_x) && abs(f_x - f_xn) < eps_f ){
      cat('Break by eps_f\n')
      cat('# x:', x, 'xn:',xn, '\n')
      cat('# f:', f_x, 'f(xn):',f_xn, '\n')
      cat('# g(x):', g_x, 'g(xn):',g_xn,'\n')
      continue <- FALSE
    }
    else if(abs(g_xn) < eps_g ){
      cat('Break by eps_g\n')
      cat('# x:', x, 'xn:',xn, '\n')
      cat('# f:', f_x, 'f(xn):',f_xn, '\n')
      cat('# g(x):', g_x, 'g(xn):',g_xn,'\n')
      continue <- FALSE
    }
    
    #Update fields
    x <- xn
    f_x <- f_xn
    g_x <- g_xn
    
    xtrace <- c(xtrace,x)
    ftrace <- c(ftrace,f_x)
    if(!continue) break;
  }
  list(
    "xtrace" = xtrace,
    "ftrace" = ftrace,
    "x" = x,
    "fx" = f(x)
  )
}

create_plot <- function(f, xs, title= '', add=FALSE, ylim=NULL, color="black") {
  if(is.function(f)) f_xs <- sapply(xs,f)
  else f_xs <- f
  #Remove NA
  xs <- xs[!is.na(f_xs)]
  f_xs <- f_xs[!is.na(f_xs)]
  if(!is.null(ylim)){
    ylim[1] <- min(ylim[1], f_xs)
    ylim[2] <- max(ylim[2], f_xs)
  }
  if(add){
    lines(x = xs, y = f_xs, type = "l", col=color)
  }
  else{
    plot(x = xs, y = f_xs, type = "l", ylim=ylim,ylab = 'f(p)', xlab = "p", main = title, col=color)
  }
  abline(v=xs[which.min(f_xs)[1]], lty=2, col=color)
}

plot_optimization <- function(gd.res, title, color="green") {
  points(gd.res$xtrace, gd.res$ftrace, type = "b", col = color)
  abline(v=gd.res$x, lty=2,col=color)
}

# Plot GD optimization 
plot_GD_result <- function(gdRes,f, f.val, title=''){
  
  xtrace.r <- range(gdRes$xtrace)
  xs <- seq(xtrace.r[1]-0.05,xtrace.r[2]+0.05,len = 100)
  xs <- xs[xs>0]
  
  xs.f <- sapply(xs, f)
  xs.f.val <- sapply(xs, f.val)
  create_plot(gFunc, xs, title, ylim=range(xs.f,xs.f.val))
  create_plot(gFuncVal, xs, add=TRUE, color = "blue")
  plot_optimization(gdRes, color='green')
  
  legend("topright", legend=c("train", "validation", "optimization"), col=c("black", "blue", "green"), lty=1:1, cex=0.8)
}

# Plot trace 
plot_trace <- function(GD.res, f, f.val, title=''){
  ftrace <- sapply(GD.res$xtrace, f)
  ftraceVal <- sapply(GD.res$xtrace,f.val)
  n <- length(GD.res$xtrace)
  # Train
  plot(1:n, ftrace, type='l', xlab='#Iterations', ylab='f(x)', ylim=range(c(ftrace,ftraceVal)), col='black', main=title )
  # Val
  lines(1:n, ftraceVal, type='l', col='blue')
  abline(v=which.min(ftraceVal)[1], lty=2,col='blue')
  
  legend("topright", legend=c("train", "validation"), col=c("black", "blue"), lty=1:1, cex=0.8)
}

#Simple example
xs <- seq(0,4,len = 100) # create some values
f <-  function(x) 1.2 * (x-2)^2 + 3.2
grad <- function(x) 1.2 * 2 * (x-2)

GD(
  f =  f,
  g = grad,
  x = 0.1, # initialisation of x
  control = list(alpha = 5, maxIter = 100)
)

gdRes <- GD(f =  f,g = grad, x = 3.1, list(alpha = 5, maxIter = 100))
create_plot(f, xs, 'title')
plot_optimization(gdRes)


# Problem example
if(FALSE){
  gdRes <- GD(f =  gFunc,g = gGrad, x = 3.1, list(alpha = 10, maxIter = 100))
  create_plot(gFunc, xs, 'title')
  plot_optimization(gdRes)
  
  gdRes <- GD(f =  gFunc,g = gGrad, x = 0.1, list(alpha = 0.01, maxIter = 100))
  plot_GD_result(gdRes, gFunc,  gFuncVal, title='alpha=0.1')
  
  gdRes <- GD(f =  gFunc,g = gGrad, x = 0.8, list(alpha = 1, maxIter = 100))
  plot_trace(gdRes,gFunc,  gFuncVal, title='Trace plot (alpha 0.05)')
  plot_GD_result(gdRes, gFunc,  gFuncVal)
  
  gdRes <- GD(f =  gFunc,g = gGrad, x = 0.8, list(alpha = 100, maxIter = 100))
  plot_trace(gdRes,gFunc,  gFuncVal, title='Trace plot (alpha 0.1)')
  plot_GD_result(gdRes, gFunc,  gFuncVal)
}

