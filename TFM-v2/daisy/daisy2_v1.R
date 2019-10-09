daisy2 <- function (x, metric = c("euclidean", "manhattan", "gower"), 
          stand = FALSE, type = list(), weights = rep.int(1, p), warnBin = warnType, 
          warnAsym = warnType, warnConst = warnType, warnType = TRUE,  colmin = NULL, sx = NULL) 
{
  if (length(dx <- dim(x)) != 2 || !(is.data.frame(x) || is.numeric(x))) 
    stop("x is not a dataframe or a numeric matrix.")
  n <- dx[1]
  p <- dx[2]
  varnms <- dimnames(x)[[2]]
  pColl <- function(n) paste(n, collapse = ", ")
  if (length(type)) {
    if (!is.list(type) || is.null(ntyp <- names(type)) || 
        any(ntyp == "")) 
      stop(gettextf("invalid %s; must be named list", 
                    sQuote("type")))
    for (nt in ntyp) {
      cvec <- type[[nt]]
      ct <- paste0("type$", nt)
      # Check string types
      if (is.character(cvec)) {
        if (!is.null(varnms) && !all(cvec %in% varnms)) 
          stop(gettextf("%s has invalid column names", 
                        ct))
      }
      # Check idx types
      else if (is.numeric(cvec)) {
        if (!all(1 <= cvec & cvec <= p)) 
          stop(gettextf("%s must be in 1:ncol(x)", ct))
      }
      else stop(gettextf("%s must contain column names or numbers", 
                         ct))
    }
    tA <- type$asymm
    tS <- type$symm
    if (!is.null(tA) || !is.null(tS)) {
      d.bin <- cbind(as.data.frame(x[, tA, drop = FALSE]), 
                     x[, tS, drop = FALSE])
      lenB <- sapply(lapply(d.bin, function(y) levels(as.factor(y))), 
                     length)
      if (any(lenB > 2)) 
        stop("at least one binary variable has more than 2 levels.")
      if (any(lenB < 2)) 
        warning("at least one binary variable has not 2 different levels.")
      if (any(is.f <- sapply(d.bin, is.factor))) 
        d.bin[is.f] <- lapply(d.bin[is.f], function(f) as.integer(as.character(f))) # Transform to integer
      if (!all(sapply(d.bin, function(y) is.logical(y) || 
                      all(sort(unique(as.numeric(y[!is.na(y)]))) %in% 
                          0:1)))) 
        stop("at least one binary variable has values not in {0,1,NA}")
    }
  }
  if (is.data.frame(x)) {
    type2 <- sapply(x, data.class)
    x <- data.matrix(x)
  }
  else {
    type2 <- rep("numeric", p)
    names(type2) <- colnames(x)
  }
  if (length(type)) {
    tT <- type$ordratio
    tL <- type$logratio
    x[, names(type2[tT])] <- unclass(as.ordered(x[, names(type2[tT])]))
    x[, names(type2[tL])] <- log10(x[, names(type2[tL])])
    type2[tA] <- "A"
    type2[tS] <- "S"
    type2[tT] <- "T"
  }
  type2[tI <- type2 %in% c("numeric", "integer")] <- "I"
  if (warnBin && n > 9 && any(tI) && any(iBin <- apply(x[, 
                                                         tI, drop = FALSE], 2, function(v) length(table(v)) == 
                                                       2))) 
    warning(gettextf("binary variable(s) %s treated as interval scaled", 
                     pColl(which(tI)[iBin])))
  type2[type2 == "ordered"] <- "O"
  type2[type2 == "factor"] <- "N"
  if (any(ilog <- type2 == "logical")) {
    if (warnAsym) 
      warning(sprintf(ngettext(sum(ilog), "setting 'logical' variable %s to type 'asymm'", 
                               "setting 'logical' variables %s to type 'asymm'"), 
                      pColl(which(ilog))), domain = NA)
    type2[ilog] <- "A"
  }
  all.I <- all(type2 == "I")
  if (all.I && {
    metric <- match.arg(metric)
    metric != "gower"
  }) {
    if (stand) {
      x <- scale(x, center = TRUE, scale = FALSE)
      sx <- colMeans(abs(x), na.rm = TRUE)
      if (0 %in% sx) {
        if (warnConst) 
          warning(gettextf("%s has constant columns %s; these are standardized to 0", 
                           sQuote("x"), pColl(which(sx == 0))))
        sx[sx == 0] <- 1
      }
      x <- scale(x, center = FALSE, scale = sx)
    }
    jdat <- 2L
    ndyst <- if (metric == "manhattan") 
      2L
    else 1L
  }
  else {
    if (!missing(metric) && metric != "gower" && !all.I) 
      warning("with mixed variables, metric \"gower\" is used automatically")
    if(is.null(colmin) || is.null(sx)){
      colR <- apply(x, 2, range, na.rm = TRUE)
      print("1")
      colmin <- colR[1, ]
      print("1")
      sx <- colR[2, ] - colmin
      print("1")
    }
    if (any(sx == 0)) 
      sx[sx == 0] <- 1
    x <- scale(x, center = colmin, scale = sx)
    jdat <- 1L
    ndyst <- 0L
    if (length(weights) == 1) 
      weights <- rep.int(weights, p)
    else if (length(weights) != p) 
      stop("'weights' must be of length p (or 1)")
  }
  typeCodes <- c("A", "S", "N", "O", "I", "T")
  type3 <- match(type2, typeCodes)
  if (any(ina <- is.na(type3))) 
    stop(gettextf("invalid type %s for column numbers %s", 
                  type2[ina], pColl(which(ina))))
  if ((mdata <- any(inax <- is.na(x)))) {
    jtmd <- integer(p)
    jtmd[apply(inax, 2L, any)] <- -1L
    valmisdat <- 1.1 * max(abs(range(x, na.rm = TRUE)))
    x[inax] <- valmisdat
  }
  storage.mode(x) <- "double"
  disv <- .Fortran(cl_daisy, n, p, x, if (mdata) rep(valmisdat, 
                                                     p) else double(1), as.double(weights), if (mdata) jtmd else integer(1), 
                   jdat, type3, ndyst, as.integer(mdata), dis = double((n * 
                                                                          (n - 1))/2), NAOK = TRUE)$dis
  disv[disv == -1] <- NA
  full <- matrix(0, n, n)
  full[!lower.tri(full, diag = TRUE)] <- disv
  disv <- t(full)[lower.tri(full)]
  if (anyNA(disv)) 
    attr(disv, "NA.message") <- "NA-values in the dissimilarity matrix !"
  class(disv) <- dissiCl
  attr(disv, "Labels") <- dimnames(x)[[1]]
  attr(disv, "Size") <- n
  attr(disv, "Metric") <- if (!ndyst) 
    "mixed"
  else metric
  if (!ndyst) 
    attr(disv, "Types") <- typeCodes[type3]
  disv
}
