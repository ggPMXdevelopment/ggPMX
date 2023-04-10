context("Test classInt")

#-------------classIntervals START ---------------------------------------------
test_that("classIntervals: params: var, style is equal, n;
          result: identical values", {
  var <- c(0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0)
  style <- "equal"
  n <- 2
  r <- classIntervals(var = var, style = style, n = n)

  expect_true(inherits(r, "classIntervals"))
  expect_identical(names(r), c("var", "brks"))
  expect_identical(r$brks, c(0, 6, 12))
  expect_identical(r$var, c(0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0))
})

test_that("classIntervals: params: var, style is fixed, n;
          result: identical values", {
  var <- c(0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0)
  style <- "fixed"
  n <- 2
  fixedBreaks <- c(0, 2, 4)
  r <- classIntervals(var = var, style = style, n = n, fixedBreaks = fixedBreaks)
  expect_identical(names(r), c("var", "brks"))
  expect_identical(r$brks, c(0, 2, 4, 12))
  expect_identical(r$var, c(0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0))
})

test_that("classIntervals: params: var, style is sd, n;
          result: identical values", {
  var <- c(0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0)
  style <- "sd"
  n <- 2
  r <- classIntervals(var = var, style = style, n = n)
  atr <- attr(r, "parameters")

  expect_true(inherits(atr, "numeric"))
  expect_identical(names(atr), c("center", "scale"))
  expect_equal(length(r$brks), 3)
})

test_that("classIntervals: params: var, style is pretty, n;
          result: identical values", {
  var <- c(0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0)
  style <- "pretty"
  n <- 2
  r <- classIntervals(var = var, style = style, n = n)

  expect_true(inherits(r, "classIntervals"))
  expect_equal(r$brks, c(0, 5, 10, 15))
  expect_equal(attr(r, "nobs"), 9)
})

test_that("classIntervals: params: var, style is kmeans, n;
          result: identical values", {
  var <- c(0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0)
  style <- "kmeans"
  n <- 2
  r <- classIntervals(var = var, style = style, n = n)
  rr <- attr(r, "parameters")

  expect_identical(
    names(rr),
    c(
      "cluster", "centers", "totss", "withinss", "tot.withinss",
      "betweenss", "size", "iter", "ifault"
    )
  )
  expect_true(inherits(r, "classIntervals"))
  expect_equal(r$brks, c(0, 6, 12))
  expect_equal(length(rr[1]$cluster), 9)
  expect_true(inherits(rr[2]$center, "matrix"))
})

test_that("classIntervals: params: var, style is hclust, n;
          result: identical values", {
  var <- c(0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0)
  style <- "hclust"
  n <- 2
  r <- classIntervals(var = var, style = style, n = n)
  rr <- attr(r, "parameters")

  expect_equal(r$brks, c(0, 8, 12))
  expect_identical(names(rr), c(
    "merge", "height", "order", "labels",
    "method", "call", "dist.method"
  ))

  expect_equal(rr$dist.method, "euclidean")
  expect_true(inherits(rr$merge, "matrix"))
})

test_that("classIntervals: params: var, style is jenks, n;
          result: identical values", {
  var <- c(0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0)
  style <- "jenks"
  n <- 2
  r <- classIntervals(var = var, style = style, n = n)

  expect_equal(r$brks, c(0, 5, 12))
  expect_equal(attr(r, "intervalClosure"), "right")
  expect_equal(attr(r, "nobs"), 9)
})

test_that("classIntervals: params: var, style is unknown_style, n;
          result: identical values", {
  var <- c(0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0)
  style <- "unknown_style"
  n <- 2

  expect_error(classIntervals(var = var, style = style, n = n))
})

test_that("classIntervals: params: var is not numerical;
          result: error", {
  var <- c("0.0", "0.5", "1.0", "2.0", "3.0", "5.0", "7.0", "9.0", "12.0")
  style <- "equal"
  n <- 2
  expect_error(classIntervals(var = var, style = style, n = n))
})
#-------------classIntervals END -----------------------------------------------

#-------------gvf START --------------------------------------------------------
test_that("gvf: params: var is vector, cols is vector;
          result: identical values", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  r <- classIntervals(var = var, style = style, n = n)
  cols <- findCols(r)
  r <- gvf(var = var, cols = cols)

  expect_equal(length(cols), 16)
  expect_true(is.numeric(r))
})
#-------------gvf END ----------------------------------------------------------

#-------------tai START --------------------------------------------------------
test_that("tai: params: var is vector, cols is vector;
          result: identical values", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  r <- classIntervals(var = var, style = style, n = n)
  cols <- findCols(r)
  r <- tai(var = var, cols = cols)

  expect_equal(length(cols), 16)
  expect_true(is.numeric(r))
})
#-------------tai END ----------------------------------------------------------

#-------------oai START --------------------------------------------------------
test_that("oai: params: var, cols, area are vectors;
          result: identical type", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  r <- classIntervals(var = var, style = style, n = n)
  cols <- findCols(r)
  area <- c(
    1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
    3, 3, 3, 3, 3
  )
  r <- oai(var = var, cols = cols, area = area)
  expect_true(is.numeric(r))
})
#-------------oai END ----------------------------------------------------------

#-------------jenks.tests START ------------------------------------------------
test_that("jenks.tests: params: clI, area are vectors;
          result: identical class and value", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- classIntervals(var = var, style = style, n = n)
  area <- c(
    1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
    3, 3, 3, 3, 3
  )
  r <- jenks.tests(clI = clI, area = area)
  expect_true(is.numeric(r))
  expect_equal(length(r), 4)
  expect_identical(names(r), c(
    "# classes", "Goodness of fit",
    "Tabular accuracy", "Overview accuracy"
  ))
})
#-------------jenks.tests END --------------------------------------------------

#-------------classIntervals2shingle START -------------------------------------
test_that("classIntervals2shingle: params: clI;
          result: identical class and values", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- classIntervals(var = var, style = style, n = n)
  r <- classIntervals2shingle(x = clI)

  expect_true(inherits(r, "shingle"))
  expect_identical(levels(r)[[1]], c(0.0, 11.5))
  expect_identical(levels(r)[[2]], c(11.5, 23.0))
})
#-------------classIntervals2shingle END ---------------------------------------

#-------------.rbrks START -----------------------------------------------------
test_that(".rbrks: params: rbrks; result: identical class and values", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  r <- classIntervals(var = var, style = style, n = n)$brks
  res <- .rbrks(rbrks = r)

  expect_true(is.numeric(res))
  expect_true(length(res) == 3)
})

test_that(".rbrks: params: single break; result: error", {
  r <- c(11.5)

  expect_error(.rbrks(rbrks = r))
})

test_that(".rbrks: params: rbrks is vector; result: vector", {
  r <- c(0.0, 11.5, 15.0, 18.5, 21.3)
  res <- .rbrks(rbrks = r)

  expect_true(length(res) == 3)
  expect_identical(res, c(0.00, 13.25, 21.30))
})
#-------------.rbrks END -------------------------------------------------------

#-------------findColours START ------------------------------------------------
test_that("findColours: params: clI, pal; result: identical class, length
          and values", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- classIntervals(var = var, style = style, n = n)
  pal <- c("red", "green")
  res <- findColours(clI = clI, pal = pal)

  expect_true(is.character(res))
  expect_true(length(res) == 16)
  expect_true(all(unique(res) %in% c("#FF0000", "#00FF00")))
})

test_that("findColours: params: single color; result: error", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- classIntervals(var = var, style = style, n = n)
  pal <- c("red")

  expect_error(findColours(clI = clI, pal = pal))
})

test_that("findColours: params: no breaks; result: error", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- classIntervals(var = var, style = style, n = n)
  clI$brks <- NULL
  pal <- c("red", "green")

  expect_error(findColours(clI = clI, pal = pal))
})
#-------------findColours END --------------------------------------------------

#-------------findCols START----------------------------------------------------
test_that("findCols: params: clI; result: identical class, length
          and values", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- classIntervals(var = var, style = style, n = n)
  res <- findCols(clI = clI)

  expect_true(is.numeric(res))
  expect_true(length(res) == 16)
  expect_true(all(unique(res) %in% c(1, 2)))
})

test_that("findCols: params: no breaks; result: error", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- classIntervals(var = var, style = style, n = n)
  clI$brks <- NULL
  expect_error(findCols(clI = clI))
})

test_that("findCols: params: clI is not classIntervals object;
          result: error", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- list(var = var, style = style, n = n)
  expect_error(findCols(clI = clI))
})
#-------------findCols END -----------------------------------------------------

#-------------tableClassIntervals START-----------------------------------------
test_that("tableClassIntervals: params: cols, brks; result: identical class
          and values", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- classIntervals(var = var, style = style, n = n)
  cols <- findCols(clI)
  brks <- clI$brks
  res <- tableClassIntervals(cols = cols, brks = brks)

  expect_true(is.numeric(res))
  expect_true(inherits(res, "table"))
  expect_identical(names(res), c("[0,11.5)", "[11.5,23]"))
  expect_true(res[[1]] == 8)
  expect_true(res[[2]] == 8)
})

test_that("tableClassIntervals: params: cols, brks,  intervalClosure is right;
           result: identical class
          and values", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- classIntervals(var = var, style = style, n = n)
  cols <- findCols(clI)
  brks <- clI$brks
  res <- tableClassIntervals(cols = cols, brks = brks, intervalClosure = "right")

  expect_true(is.numeric(res))
  expect_true(inherits(res, "table"))
  expect_identical(names(res), c("(0,11.5]", "(11.5,23]"))
  expect_true(res[[1]] == 8)
  expect_true(res[[2]] == 8)
})

test_that("tableClassIntervals: params: cols, brks, cutlabels is F;
           result: identical names", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- classIntervals(var = var, style = style, n = n)
  cols <- findCols(clI)
  brks <- clI$brks
  res <- tableClassIntervals(cols = cols, brks = brks, cutlabels = F)

  expect_identical(names(res), c("0 - 11.5", "over 11.5"))
})

test_that("tableClassIntervals: params: cols, brks, cutlabels is F, between is ~;
           result: identical names", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- classIntervals(var = var, style = style, n = n)
  cols <- findCols(clI)
  brks <- clI$brks
  res <- tableClassIntervals(
    cols = cols, brks = brks, cutlabels = F,
    between = "~"
  )
  expect_identical(names(res), c("0 ~ 11.5", "over 11.5"))
})

test_that("tableClassIntervals: params: cols is NULL, brks;
           result: identical values 0", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- classIntervals(var = var, style = style, n = n)
  cols <- NULL
  brks <- clI$brks
  res <- tableClassIntervals(cols = cols, brks = brks)
  expect_identical(names(res), c("[0,11.5)", "[11.5,23]"))
  expect_true(res[[1]] == 0)
  expect_true(res[[2]] == 0)
})

test_that("tableClassIntervals: params: cols, brks is NULL;
           result: identical values 0", {
  var <- c(
    0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0, 13.0, 15.0,
    16.0, 18.0, 19.0, 21.0, 23.0
  )
  style <- "equal"
  n <- 2
  clI <- classIntervals(var = var, style = style, n = n)
  cols <- findCols(clI)
  brks <- NULL

  expect_error(tableClassIntervals(cols = cols, brks = brks))
})
#-------------tableClassIntervals END ------------------------------------------

#-------------roundEndpoint START-----------------------------------------------
test_that("roundEndpoint: params: x; result: character value", {
  x <- 12.025256
  dataPrecision <- 2
  res <- roundEndpoint(x = x, dataPrecision = dataPrecision)
  expect_true(is.character(res))
  expect_equal(res, "12.03")
})

test_that("roundEndpoint: params: x, intervalClosure is right;
          result: character value", {
  x <- 12.025256
  dataPrecision <- 2
  res <- roundEndpoint(
    x = x, intervalClosure = "right",
    dataPrecision = dataPrecision
  )
  expect_equal(res, "12.02")
})

test_that("roundEndpoint: params: x is  NULL, intervalClosure is right;
          result: character value", {
  x <- NULL
  dataPrecision <- 2
  res <- roundEndpoint(
    x = x,
    dataPrecision = dataPrecision
  )
  expect_equal(res, character(0))
})
#-------------roundEndpoint END ------------------------------------------------

#-------------print.classIntervals START----------------------------------------
test_that("print.classIntervals: params: x; result: character value", {
  var <- c(0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0)
  style <- "equal"
  n <- 2
  x <- classIntervals(var = var, style = style, n = n)
  res <- print.classIntervals(x = x, dataPrecision = 1)

  expect_true(inherits(res, "table"))
  expect_identical(names(res), c("[0,6)", "[6,12]"))
  expect_true(res[[1]] == 6)
  expect_true(res[[2]] == 3)
})

test_that("print.classIntervals: params: x is numeric; result: error", {
  x <- 12.025256

  expect_error(print.classIntervals(x = x))
})
#-------------print.classIntervals END -----------------------------------------

#-------------nPartitions START-------------------------------------------------
test_that("nPartitions: params: x; result: identical class and value", {
  var <- c(0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0)
  style <- "equal"
  n <- 2
  x <- classIntervals(var = var, style = style, n = n)
  res <- nPartitions(x = x)

  expect_true(inherits(res, "numeric"))
  expect_true(res == 8)
})

test_that("nPartitions: params: x is not classIntervals object;
          result: error", {
  var <- c(0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 7.0, 9.0, 12.0)
  style <- "equal"
  n <- 2
  x <- list(var = var, style = style, n = n)

  expect_error(nPartitions(x = x))
})
#-------------nPartitions END --------------------------------------------------
