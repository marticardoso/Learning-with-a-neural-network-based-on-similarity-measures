
#Line search algorithm satisfying strong Wolfe conditions
backlinesearch <- function(f,g,d,x0,alpham,c1=0,c2=0,maxiter=1000,eps=1e-4){
  alpha0 <- 0
  alphap <- alpha0
  
  iout <- 0
  if(c1 == 0) c1 <- 1e-4
  if(c2 == 0) c2 <- 0.5
  
  alphax <- alpham
  
  fx0 <- f(x0)
  gx0 <- g(x0)*d
  
  fxp <- fx0
  gxp <- gx0
  
  i <- 1
  
  while (i < maxiter){
    if( (alphap - alphax) < eps)
      return(list(alphas=alphax, iout=2))
    
    xx <- x0 + alphax*d
    fxx <- f(xx)
    gxx <- g(xx)*d
    
    if((fxx > fx0 + c1 *alphax*gx0) || ((i>1) && (fxx >= fxp))){
      z <- zoom(f,g,x0,d,alphap,alphax,c1,c2,eps);
      return(list(alphas=z$alphas, iout=z$iout))
    }
    
    
    if( abs(gxx) <= -c2*gx0){
      return(list(alphas=alphax, iout=0))
    }
    
    if(gxx >= 0){
      z <- zoom(f,g,x0,d,alphax,alphap,c1,c2,eps)
      return(list(alphas=z$alphas, iout=z$iout))
    }
    alphap <- alphax
    fxp <- fxx
    gxp <- gxx
    alphax <- alphax + (alpham - alphax)*runif(1)
    i <- i+1
  }
  if(i==iter_btls)
    return(list(alphas=alphax, iout=0))
  return(list(alphas=alphas, iout=0))
} 


zoom <- function(f,g,x0,d,alphal, alphah, c1, c2, eps){
  fx0 <- f(x0)
  gx0 <- g(x0)*d
  while(TRUE){
    if(abs(alphal-alphah)< eps){
      return(list(alphas=alphax,iout=2))
    }
    alphax <- 1/2*(alphal+alphah)
    xx <- x0 + alphax*d
    fxx <- f(xx)
    gxx <- g(xx)*d
    
    xl = x0 + alphal*d
    
    fxl = f(xl)
    if((fxx > fx0 + c1*alphax*gx0) || (fxx >= fxl))
      alphah <- alphax
    else {
      if(abs(gxx) <= -c2*gx0){
        alphas <- alphax
        return(list(alphas=alphas, iout=0))
      }
      if(gxx*(alphah-alphal) >= 0){
        alphah <- alphal
      }
      alphal <- alphax
    }
  } 
}


find_alpha <- function(f){
  phi <- function(alpha) f(x + alpha*d)
  alpha <- optimize(phi, lower=0, maximum=almax)
}
