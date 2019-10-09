
dyn.load("daisy/daisy2.dll")
cl_daisy <-"cldaisy"
dissiCl <- c("dissimilarity", "dist")


# Only for gower
daisy2.newObservations <- function(x, daisyObj)
{
  ## check type of input matrix
  if(length(dx <- dim(x)) != 2 || !(is.data.frame(x) || is.numeric(x)))
    stop("x is not a dataframe or a numeric matrix.")
  
  if(!is.null(attr(daisyObj,"daisyObj"))) daisyObj <- attr(daisyObj,"daisyObj")
  type <- daisyObj$type
  weights <- daisyObj$weights
  colmin <- daisyObj$colmin
  colmean <- daisyObj$colmean
  sx <- daisyObj$sx
  type2 <- daisyObj$type2
  stand <- daisyObj$stand
  
  ndyst <- daisyObj$ndyst
  jdat <- daisyObj$jdat
  n <- nrow(x)
  p  <- ncol(x)
  if(p != daisyObj$p)
    stop("x must have the same number of dimensions")
  
  ## transform variables and construct 'type' vector
  if(is.data.frame(x)) 
    x <- data.matrix(x)
  
  if(length(type)) {
    tT <- type$ordratio
    tL <- type$logratio
    x[, names(type2[tT])] <- unclass(as.ordered(x[, names(type2[tT])]))
    x[, names(type2[tL])] <- log10(		    x[, names(type2[tL])])
  }
 
  ## standardize, if necessary
  if(jdat == 2L) { # All numerical and not gower
    if(stand)
      x <- scale(x, center = colmean, scale = sx)
  }
  else  ## mixed case or explicit "gower"
    x <- scale(x, center = colmin, scale = sx)
  
  typeCodes <- c('A','S','N','O','I','T')
  ##              1   2   3   4   5   6  --> passed to Fortran below
  type3 <- match(type2, typeCodes)# integer
  
  # Missing data calculation
  if((mdata <- any(inax <- is.na(x)))) { # TRUE if x[] has any NAs
    jtmd <- integer(p)
    jtmd[apply(inax, 2L, any)] <- -1L
    valmisdat <- 1.1* max(abs(range(x, na.rm=TRUE)))
    x[inax] <- valmisdat
  }
  
  ## call Fortran routine
  storage.mode(x) <- "double"
  disv <- .Fortran(cl_daisy, ## -> ../src/daisy.f
                   n,
                   p,
                   x,
                   if(mdata) rep(valmisdat, p) else double(1),
                   as.double(weights),
                   if(mdata) jtmd else integer(1),
                   jdat,
                   type3,		# vtype
                   ndyst,
                   as.integer(mdata),
                   dis = double((n * (n - 1))/2),
                   NAOK = TRUE# only to allow "+- Inf"
  )$dis
  ## adapt Fortran output to S:
  ## convert lower matrix, read by rows, to upper matrix, read by rows.
  disv[disv == -1] <- NA
  full <- matrix(0, n, n)
  full[!lower.tri(full, diag = TRUE)] <- disv
  disv <- t(full)[lower.tri(full)]
  ## give warning if some dissimilarities are missimg
  if(anyNA(disv)) attr(disv, "NA.message") <-
    "NA-values in the dissimilarity matrix !"
  
  ## construct S object -- "dist" methods are *there* !
  class(disv) <- dissiCl 
  attr(disv, "Size") <- n
  
  disv
}
