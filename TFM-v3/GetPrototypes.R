library(cluster)
# Data
# Data: expected similarity matrix (PAM)
# Method: method to be used (PAM, uniform, binomial and poisson)
# M: number of prototypes
getPrototypes <- function (data, method,
                           clust.metric = "euclidean", 
                           clust.stand=FALSE, 
                           M = NULL, 
                           p = NULL, 
                           lambda = NULL){
  n <- nrow(data)
  if(is.null(M)){
    M = 0.1*n
  }
  
  if(method=="PAM"){
    dataset.pam <- pam (data, k=M, metric = clust.metric, stand=clust.stand, keep.diss=FALSE, keep.data=FALSE)
    return(dataset.pam$id.med)  
  }
  else if(method=="Uniform"){
    return(sample(1:n,M))
  }
  else if(method=="Binomial"){
    if(is.null(p)){
      p = M/n
    }
    r <- rbinom(n, 1, p)
    return(which(r>0))
  }
  else if(method=="Poisson"){
    if(is.null(lambda)){
      lambda = M/n
    }
    r <- rpois(n, lambda)
    return(which(r>0))
  }
  else{
    stop("Enter a valid method ('PAM', 'Uniform', 'Binomial', 'Poisson')")
  }
}
