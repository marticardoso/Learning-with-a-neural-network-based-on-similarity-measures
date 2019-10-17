
dyn.load("daisy/daisy2.dll")
cl_daisy <-"cldaisy"
dissiCl <- c("dissimilarity", "dist")

daisy2 <- function(x, metric = c("euclidean", "manhattan", "gower"),
                            stand = FALSE, type = list(), weights = rep.int(1, p), fixUnbalancedDiss = TRUE,
                            warnBin = warnType, warnAsym = warnType, warnConst = warnType, warnType = TRUE)
{
  ## check type of input matrix
  if(length(dx <- dim(x)) != 2 || !(is.data.frame(x) || is.numeric(x)))
    stop("x is not a dataframe or a numeric matrix.")
  n <- dx[1]# nrow
  p <- dx[2]# ncol
  varnms <- dimnames(x)[[2]]
  pColl <- function(n) paste(n, collapse = ", ")
  if(length(type)) {
    if(!is.list(type) || is.null(ntyp <- names(type)) || any(ntyp == ""))
      stop(gettextf("invalid %s; must be named list", sQuote("type")))
    ## check each component to be valid column names or numbers:
    for(nt in ntyp) {
      cvec <- type[[nt]]
      ct <- paste0("type$", nt)
      if(is.character(cvec)) {
        if(!is.null(varnms) && !all(cvec %in% varnms))
          stop(gettextf("%s has invalid column names", ct))
      }
      else if(is.numeric(cvec)) {
        if(!all(1 <= cvec & cvec <= p))
          stop(gettextf("%s must be in 1:ncol(x)", ct))
      }
      else stop(gettextf("%s must contain column names or numbers", ct))
    }
    tA <- type$asymm
    tS <- type$symm
    if(!is.null(tA) || !is.null(tS)) {
      ## tA and tS might be character and integer!
      d.bin <- cbind(as.data.frame(x[, tA, drop= FALSE]),
                     x[, tS, drop= FALSE])
      lenB <- sapply(lapply(d.bin, function(y)
        levels(as.factor(y))), length)
      if(any(lenB > 2))
        stop("at least one binary variable has more than 2 levels.")
      if(any(lenB < 2))
        warning("at least one binary variable has not 2 different levels.")
      ## Convert factors to integer, such that ("0","1") --> (0,1):
      if(any(is.f <- sapply(d.bin, is.factor)))
        d.bin[is.f] <- lapply(d.bin[is.f],
                              function(f) as.integer(as.character(f)))
      if(!all(sapply(d.bin, function(y)
        is.logical(y) ||
        all(sort(unique(as.numeric(y[!is.na(y)])))%in% 0:1))))
        stop("at least one binary variable has values not in {0,1,NA}")
    }
  }
  ## transform variables and construct 'type' vector
  if(is.data.frame(x)) {
    type2 <- sapply(x, data.class)
    x <- data.matrix(x)
  } else { ## matrix
    type2 <- rep("numeric", p)
    names(type2) <- colnames(x)
  }
  if(length(type)) {
    tT <- type$ ordratio
    ordRatioLevels <- list()
    for(colName in names(type2[tT])){
      ordRatioLevels[[colName]] <- levels(as.ordered(x[, colName]))
      x[, colName] <- unclass(as.ordered(x[, colName]))
    }
    #x[, names(type2[tT])] <- unclass(as.ordered(x[, names(type2[tT])]))
    
    tL <- type$ logratio
    x[, names(type2[tL])] <- log10(x[, names(type2[tL])])
    
    type2[tA] <- "A"
    type2[tS] <- "S"
    type2[tT] <- "T" 
  }
  type2[tI <- type2 %in% c("numeric", "integer") ] <- "I"
  if(warnBin && n > 9 && any(tI) &&
     any(iBin <- apply(x[, tI, drop = FALSE], 2,
                       function(v) length(table(v)) == 2)))
    warning(gettextf("binary variable(s) %s treated as interval scaled",
                     pColl(which(tI)[iBin])))
  
  type2[type2 == "ordered"] <- "O"
  type2[type2 == "factor"] <- "N"
  if(any(ilog <- type2 == "logical")) {
    if(warnAsym) warning(sprintf(ngettext(sum(ilog),
                                          "setting 'logical' variable %s to type 'asymm'",
                                          "setting 'logical' variables %s to type 'asymm'"),
                                 pColl(which(ilog))), domain = NA)
    type2[ilog] <- "A"
  }
  ## Note: We have 2 status codings:  ndyst = (0,1,2) and jdat = (1,2);
  ##       the latter is superfluous in principle
  
  ## standardize, if necessary
  all.I <- all(type2 == "I")
  if(all.I && { metric <- match.arg(metric); metric != "gower" }) {
    if(stand) {
      x <- scale(x, center = TRUE, scale = FALSE) #-> 0-means
      sx <- colMeans(abs(x), na.rm = TRUE)# can still have NA's
      if(0 %in% sx) {
        if(warnConst) warning(gettextf(
          "%s has constant columns %s; these are standardized to 0",
          sQuote("x"), pColl(which(sx == 0))))
        sx[sx == 0] <- 1
      }
      x <- scale(x, center = FALSE, scale = sx)
    }
    jdat <- 2L
    ndyst <- if(metric == "manhattan") 2L else 1L # == diss_kind
  }
  else { ## mixed case or explicit "gower"
    if(!missing(metric) && metric != "gower" && !all.I)
      warning("with mixed variables, metric \"gower\" is used automatically")
    ## FIXME: think of a robust alternative scaling to
    ##        Gower's  (x - min(x)) / (max(x) - min(x))
    colR <- apply(x, 2, range, na.rm = TRUE)
    colmin <- colR[1, ]
    sx <- colR[2, ] - colmin
    if(any(sx == 0))
      sx[sx == 0] <- 1
    x <- scale(x, center = colmin, scale = sx)
    jdat <- 1L
    ndyst <- 0L ## diss_kind = "mixed | gower"
    ## weights only used in this "gower" case
    if(length(weights) == 1)
      weights <- rep.int(weights, p)
    else if(length(weights) != p)
      stop("'weights' must be of length p (or 1)")
  }
  
  ##	type2 <- paste(type2, collapse = "")
  typeCodes <- c('A','S','N','O','I','T')
  ##              1   2   3   4   5   6  --> passed to Fortran below
  type3 <- match(type2, typeCodes)# integer
  if(any(ina <- is.na(type3)))
    stop(gettextf("invalid type %s for column numbers %s",
                  type2[ina], pColl(which(ina))))
  if((mdata <- any(inax <- is.na(x)))) { # TRUE if x[] has any NAs
    jtmd <- integer(p)
    jtmd[apply(inax, 2L, any)] <- -1L
    ## VALue for MISsing DATa
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
  
  #Apply sqrt/squre if dissimilaries are unbalanced
  applySqrt <- fixUnbalancedDiss && max(disv)<=0.5
  applySqure <- fixUnbalancedDiss && min(disv)>= 0.5 
  if(applySqrt) {
    disv <- sqrt(disv)
    cat('Applied sqrt correction')
  }
  if(applySqure) {
    disv <- disv^2
    cat('Applied square correction')
  }
  
  ## give warning if some dissimilarities are missimg
  if(anyNA(disv)) attr(disv, "NA.message") <- "NA-values in the dissimilarity matrix !"
  
  ## construct S object -- "dist" methods are *there* !
  class(disv) <- dissiCl # see ./0aaa.R
  attr(disv, "Labels") <- dimnames(x)[[1]]
  attr(disv, "Size") <- n
  attr(disv, "Metric") <- if(!ndyst) "mixed" else metric
  if(!ndyst) attr(disv, "Types") <- typeCodes[type3]
  
  #Return daisy object fields to recompute dissimilarities
  z = list()
  z$type <- type
  z$weights <- weights
  if(exists("colmin")) z$colmin <- colmin
  if(exists("colmean")) z$colmean <- colmean
  if(exists("sx")) z$sx <- sx
  z$type2 <- type2
  z$stand <- stand
  z$ndyst <- ndyst
  z$jdat <- jdat
  z$p <- p
  if(exists("ordRatioLevels")) z$ordRatioLevels <- ordRatioLevels
  z$applySqrt <- applySqrt
  z$applySqure <- applySqure
  attr(disv, "daisyObj") <- z
  
  disv
}
