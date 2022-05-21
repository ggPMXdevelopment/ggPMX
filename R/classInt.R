gvf <- function(var, cols) {
  sumsq <- function(x) sum((x - mean(x))^2)
  sdam <- sumsq(var)
  sdcm <- sum(tapply(var, factor(cols), sumsq))
  res <- 1 - (sdcm / sdam)
  res
}

tai <- function(var, cols) {
  sumabs <- function(x) sum(abs(x - mean(x)))
  x <- sumabs(var)
  y <- sum(tapply(var, factor(cols), sumabs))
  res <- 1 - (y / x)
  res
}

oai <- function(var, cols, area) {
  sumabs1 <- function(x) sum(abs(x[, 1] - mean(x[, 1])) * x[, 2])
  m <- cbind(as.numeric(var), as.numeric(area))
  x <- sumabs1(m)
  y <- sum(by(m, factor(cols), sumabs1))
  res <- 1 - (y / x)
  res
}

jenks.tests <- function(clI, area) {
  if (!inherits(clI, "classIntervals")) stop("Class interval object required")
  cols <- findCols(clI)
  res <- c(
    "# classes" = length(clI$brks) - 1,
    "Goodness of fit" = gvf(clI$var, cols),
    "Tabular accuracy" = tai(clI$var, cols)
  )
  if (!missing(area)) {
    if (length(area) != length(cols)) {
      stop("area and classified variable different lengths")
    }
    res <- c(res, "Overview accuracy" = oai(clI$var, cols, area))
  }
  res
}



classIntervals2shingle <- function(x) {
  res <- x$var
  nl <- length(x$brks) - 1
  lres <- vector(mode = "list", length = nl)
  for (i in 1:nl) lres[[i]] <- x$brks[c(i, i + 1)]
  class(lres) <- "shingleLevel"
  attr(res, "levels") <- lres
  class(res) <- "shingle"
  res
}


# change contributed by Richard Dunlap 090512
# Added intervalClosure argument to allow specification of whether
# partition intervals are closed on the left or the right
# Added dataPrecision argument to allow rounding of interval boundaries
# to the precision -- the argument equals the number of
# decimal places in the data.  Negative numbers retain the usual
# convention for rounding.
classIntervals <- function(var, n, style = "quantile", rtimes = 3, ..., intervalClosure = c("left", "right"), dataPrecision = NULL, warnSmallN = TRUE) {
  if (is.factor(var)) stop("var is categorical")
  if (!is.numeric(var)) stop("var is not numeric")
  # Matthieu Stigler 120705
  intervalClosure <- match.arg(intervalClosure)
  ovar <- var
  if (any(is.na(var))) {
    warning("var has missing values, omitted in finding classes")
    var <- c(stats::na.omit(var))
  }
  if (any(!is.finite(var))) {
    warning("var has infinite values, omitted in finding classes")
    is.na(var) <- !is.finite(var)
  }
  nobs <- length(unique(var))
  if (nobs == 1) stop("single unique value")
  if (missing(n)) n <- do.call("nclass.Sturges", list(var))
  if (n < 2) stop("n less than 2")
  n <- as.integer(n)
  pars <- NULL
  if (n > nobs) {
    if (warnSmallN) {
      warning(paste(
        "n greater than number of different finite values",
        "n reset to number of different finite values",
        sep = "\\n"
      ))
    }
    n <- nobs
  }
  if (n == nobs) {
    if (warnSmallN) {
      warning(paste(
        "n same as number of different finite values",
        "each different finite value is a separate class",
        sep = "\\n"
      ))
    }
    sVar <- sort(unique(var))
    dsVar <- diff(sVar)
    brks <- c(
      sVar[1] - (mean(dsVar) / 2), sVar[1:(length(sVar) - 1)] + (dsVar / 2),
      sVar[length(sVar)] + (mean(dsVar) / 2)
    )
    style <- "unique"
  } else {
    if (style == "fixed") {
      #      mc <- match.call(expand.dots=FALSE)
      #      fixedBreaks <- sort(eval(mc$...$fixedBreaks))
      # Matthieu Stigler 111110
      dots <- list(...)
      fixedBreaks <- sort(dots$fixedBreaks)
      if (is.null(fixedBreaks)) {
        stop("fixed method requires fixedBreaks argument")
      }
      #      if (length(fixedBreaks) != (n+1))
      #        stop("mismatch between fixedBreaks and n")
      if (!is.numeric(fixedBreaks)) stop("fixedBreaks must be numeric")
      if (any(diff(fixedBreaks) < 0)) stop("decreasing fixedBreaks found")
      fixedBreaks[1] <- max(fixedBreaks[1], min(var))
      fixedBreaks[length(fixedBreaks)] <- min(fixedBreaks[length(fixedBreaks)], max(var))
      if (fixedBreaks[1] > min(var)) {
        fixedBreaks <- c(min(var), fixedBreaks)
      }
      if (fixedBreaks[length(fixedBreaks)] < max(var)) {
        fixedBreaks <- c(fixedBreaks, max(var))
      }
      brks <- fixedBreaks
    } else if (style == "sd") {
      svar <- scale(var)
      pars <- c(attr(svar, "scaled:center"), attr(svar, "scaled:scale"))
      names(pars) <- c("center", "scale")
      sbrks <- pretty(x = svar, n = n, ...)
      brks <- c((sbrks * pars[2]) + pars[1])
    } else if (style == "equal") {
      brks <- seq(min(var), max(var), length.out = (n + 1))
    } else if (style == "pretty") {
      brks <- c(pretty(x = var, n = n, ...))
    } else if (style == "quantile") {
      # stats
      brks <- c(stats::quantile(x = var, probs = seq(0, 1, 1 / n), ...))
      names(brks) <- NULL
    } else if (style == "kmeans") {
      # stats
      pars <- try(stats::kmeans(x = var, centers = n, ...))
      if (inherits(pars, "try-error")) {
        warning("jittering in kmeans")
        jvar <- jitter(rep(x = var, times = rtimes))
        pars <- try(stats::kmeans(x = jvar, centers = n, ...))
        if (inherits(pars, "try-error")) {
          stop("kmeans failed after jittering")
        } else {
          cols <- match(pars$cluster, order(c(pars$centers)))
          rbrks <- unlist(tapply(jvar, factor(cols), range))
        }
      } else {
        cols <- match(pars$cluster, order(c(pars$centers)))
        rbrks <- unlist(tapply(var, factor(cols), range))
      }
      names(rbrks) <- NULL
      brks <- .rbrks(rbrks)
    } else if (style == "hclust") {
      # stats
      pars <- stats::hclust(stats::dist(x = var, method = "euclidean"), ...)
      rcluster <- stats::cutree(tree = pars, k = n)
      rcenters <- unlist(tapply(var, factor(rcluster), mean))
      cols <- match(rcluster, order(c(rcenters)))
      rbrks <- unlist(tapply(var, factor(cols), range))
      names(rbrks) <- NULL
      brks <- .rbrks(rbrks)
    } else if (style == "jenks") { # Jenks Optimisation Method
      # change contributed by Richard Dunlap 090512
      # This version of the Jenks code assumes intervals are closed on
      # the right -- force it.
      intervalClosure <- "right"
      if (storage.mode(var) != "double") storage.mode(var) <- "double"
      d <- sort(var)
      k <- n
      # work<-matrix(0,k,length(d))
      mat1 <- matrix(1, length(d), k)
      mat2 <- matrix(0, length(d), k)
      mat2[2:length(d), 1:k] <- .Machine$double.xmax # R's max double value?
      v <- 0

      for (l in 2:length(d)) {
        s1 <- s2 <- w <- 0
        for (m in 1:l) {
          i3 <- l - m + 1
          val <- d[i3]
          s2 <- s2 + val * val
          s1 <- s1 + val
          w <- w + 1
          v <- s2 - (s1 * s1) / w
          i4 <- trunc(i3 - 1)

          if (i4 != 0) {
            for (j in 2:k) {
              if (mat2[l, j] >= (v + mat2[i4, j - 1])) {
                mat1[l, j] <- i3
                mat2[l, j] <- v + mat2[i4, j - 1]
              }
            }
          }
        }
        mat1[l, 1] <- 1
        mat2[l, 1] <- v
      }

      kclass <- 1:k
      kclass[k] <- length(d)
      k <- length(d)
      last <- length(d)
      for (j in length(kclass):1) {
        id <- trunc(mat1[k, j]) - 1
        kclass[j - 1] <- id
        k <- id # lower
        last <- k - 1 # upper
      }
      # change uncontributed by Richard Dunlap 090512
      # with the specification of intervalClosure for the presentation layer,
      # don't need to change this
      brks <- d[c(1, kclass)]
    } else {
      stop(paste(style, "unknown"))
    }
  }
  if (is.null(brks)) stop("Null breaks")
  res <- list(var = ovar, brks = brks)
  attr(res, "style") <- style
  attr(res, "parameters") <- pars
  attr(res, "nobs") <- nobs
  attr(res, "call") <- match.call()
  # change contributed by Richard Dunlap 090512
  # Add intervalClosure and dataPrecision to the attributes so they're
  # available to the print method
  attr(res, "intervalClosure") <- intervalClosure
  attr(res, "dataPrecision") <- dataPrecision
  class(res) <- "classIntervals"
  res
}

.rbrks <- function(rbrks) {
  nb <- length(rbrks)
  if (nb < 2) stop("single break")
  brks <- c(rbrks[1], rbrks[nb])
  if (nb > 2) {
    if (nb == 3) {
      brks <- append(brks, rbrks[2], 1)
    } else {
      ins <- NULL
      for (i in as.integer(seq(2, (nb - 2), 2))) {
        ins <- c(ins, ((rbrks[i] + rbrks[i + 1]) / 2))
      }
      brks <- append(brks, ins, 1)
    }
  }
  brks
}

findColours <- function(clI, pal, under = "under", over = "over", between = "-",
                        digits = getOption("digits"), cutlabels = TRUE) {
  if (!inherits(clI, "classIntervals")) stop("Class interval object required")
  if (is.null(clI$brks)) stop("Null breaks")
  if (length(pal) < 2) stop("pal must contain at least two colours")
  cols <- findCols(clI)
  palette <- grDevices::colorRampPalette(pal)(length(clI$brks) - 1)
  res <- palette[cols]
  attr(res, "palette") <- palette
  tab <- tableClassIntervals(
    cols = cols, brks = clI$brks, under = under, over = over,
    between = between, digits = digits, cutlabels = cutlabels,
    intervalClosure = attr(clI, "intervalClosure"),
    dataPrecision = attr(clI, "dataPrecision")
  )
  attr(res, "table") <- tab
  res
}

# change contributed by Richard Dunlap 090512
# Looks for intervalClosure attribute to allow specification of
# whether partition intervals are closed on the left or the right
findCols <- function(clI) {
  if (!inherits(clI, "classIntervals")) stop("Class interval object required")
  if (is.null(clI$brks)) stop("Null breaks")
  if (is.null(attr(clI, "intervalClosure")) || (attr(clI, "intervalClosure") == "left")) {
    cols <- findInterval(clI$var, clI$brks, all.inside = TRUE)
  }
  else {
    cols <- apply(array(apply(outer(clI$var, clI$brks, ">"), 1, sum)), 1, max, 1)
  }
  cols
}

# change contributed by Richard Dunlap 090512
# Added intervalClosure argument to allow specification of whether
# partition intervals are closed on the left or the right
# Added dataPrecision for rounding of the interval endpoints
tableClassIntervals <- function(cols, brks, under = "under", over = "over",
                                between = "-", digits = getOption("digits"), cutlabels = TRUE, intervalClosure = c("left", "right"), dataPrecision = NULL, unique = FALSE, var) {
  # Matthieu Stigler 120705 unique
  # Matthieu Stigler 120705
  intervalClosure <- match.arg(intervalClosure)
  lx <- length(brks)
  nres <- character(lx - 1)
  sep <- " "
  if (cutlabels) {
    sep <- ""
    between <- ","
  }

  if (is.null(intervalClosure) || (intervalClosure == "left")) {
    left <- "["
    right <- ")"
  }
  else {
    left <- "("
    right <- "]"
  }

  # The two global endpoints are going through roundEndpoint to get
  # formatting right, nothing more
  if (cutlabels) {
    nres[1] <- paste("[", roundEndpoint(brks[1], intervalClosure, dataPrecision), between, roundEndpoint(brks[2], intervalClosure, dataPrecision), right, sep = sep)
  } else {
    nres[1] <- paste(under, roundEndpoint(brks[2], intervalClosure, dataPrecision), sep = sep)
  }
  for (i in 2:(lx - 2)) {
    if (cutlabels) {
      nres[i] <- paste(
        left, roundEndpoint(brks[i], intervalClosure, dataPrecision), between, roundEndpoint(brks[i + 1], intervalClosure, dataPrecision), right,
        sep = sep
      )
    } else {
      nres[i] <- paste(roundEndpoint(brks[i], intervalClosure, dataPrecision), between, roundEndpoint(brks[i + 1], intervalClosure, dataPrecision), sep = sep)
    }
  }
  if (cutlabels) {
    nres[lx - 1] <- paste(
      left, roundEndpoint(brks[lx - 1], intervalClosure, dataPrecision), between, roundEndpoint(brks[lx], intervalClosure, dataPrecision), "]",
      sep = sep
    )
  } else {
    nres[lx - 1] <- paste(over, roundEndpoint(brks[lx - 1], intervalClosure, dataPrecision), sep = sep)
  }
  tab <- table(factor(cols, levels = 1:(lx - 1)))
  names(tab) <- nres

  # Matthieu Stigler 120705 unique
  ## Assign unique label for intervals containing same left-right points
  if (unique & !missing(var)) {
    tab_unique <- tapply(var, cols, function(x) length(unique(x)))
    #    tab_unique_vals<-tapply(var, cols, function(x) length(unique(x)))
    if (any(tab_unique == 1)) {
      #      w.unique <-which(tab_unique==1)
      w.unique <- as.numeric(names(which(tab_unique == 1)))
      cat("Class found with one single (possibly repeated) value: changed label\n")
      #      cols.unique <-cols%in%names(w.unique)
      cols.unique <- cols %in% w.unique
      names(tab)[w.unique] <- tapply(var[cols.unique ], cols[cols.unique ], function(x) if (is.null(dataPrecision)) unique(x) else round(unique(x), dataPrecision))
    }
  }

  tab
}

# change contributed by Richard Dunlap 090512
# New helper method for tableClassIntervals
roundEndpoint <- function(x, intervalClosure = c("left", "right"), dataPrecision) {
  # Matthieu Stigler 120705
  intervalClosure <- match.arg(intervalClosure)
  if (is.null(dataPrecision)) {
    retval <- x
  }
  else if (is.null(intervalClosure) || (intervalClosure == "left")) {
    retval <- ceiling(x * 10^dataPrecision) / 10^dataPrecision
  }
  else {
    retval <- floor(x * 10^dataPrecision) / 10^dataPrecision
  }
  digits <- getOption("digits")
  format(retval, digits = digits, trim = TRUE)
} # FIXME output trailing zeros in decimals

print.classIntervals <- function(x, digits = getOption("digits"), ..., under = "under", over = "over", between = "-", cutlabels = TRUE, unique = FALSE) {
  if (!inherits(x, "classIntervals")) stop("Class interval object required")
  cat("style: ", attr(x, "style"), "\n", sep = "")
  nP <- nPartitions(x)
  if (is.finite(nP)) {
    cat(
      "  one of ", prettyNum(nP, big.mark = ","),
      " possible partitions of this variable into ", length(x$brks) - 1,
      " classes\n",
      sep = ""
    )
  }
  cols <- findCols(x)
  # change contributed by Richard Dunlap 090512
  # passes the intervalClosure argument to tableClassIntervals
  tab <- tableClassIntervals(
    cols = cols, brks = x$brks, under = under, over = over,
    between = between, digits = digits, cutlabels = cutlabels, intervalClosure = attr(x, "intervalClosure"), dataPrecision = attr(x, "dataPrecision"), unique = unique, x$var
  )
  print(tab, digits = digits, ...)
  invisible(tab)
}

nPartitions <- function(x) {
  n <- attr(x, "nobs")
  if (n > 170) {
    ret <- Inf
  } else {
    k <- length(x$brks) - 1
    ret <- (factorial(n - 1)) / (factorial(n - k) * factorial(k - 1))
  }
  ret
}
