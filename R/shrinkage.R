
#' Compute shrinkage
#'
#' @param estimates parameters estimation data
#' @param eta individual estimation data
#' @param fun can be either sd or var
#' @param by \code{character} vector to group by before shrinkage
#'
#' @return data.table computing the shrinkage by effect
#' @export

shrinkage <-
  function(estimates, eta, fun=c("sd", "var"), by="") {
    PARAM <- EFFECT <- VAR <- FUN <- NULL
    VALUE_ETA <- VALUE_OMEGA <- VALUE <- NULL
    if (missing(fun)) fun <- "sd"
    dx1 <- estimates[grepl("omega", PARAM)]
    dx1[, c("VAR", "EFFECT") := list("omega", gsub("(^ +)?omega_", "", PARAM))]
    if (!"EFFECT" %in% intersect(names(dx1), names(eta))) return(NULL)
    dx <- merge(eta, dx1, by = "EFFECT")
    setnames(dx, c("VALUE.x", "VALUE.y"), c("VALUE", "VALUE_OMEGA"))

    grp <- "EFFECT,VALUE_OMEGA"
    if (any(nzchar(by))) grp <- sprintf("%s,%s", grp, paste(by, collapse = ","))
    dx[, {
      coef <- if (fun == "sd") VALUE_OMEGA else VALUE_OMEGA ^ 2
      shrink <- 1 - get(fun)(VALUE) / coef

      list(SHRINK = shrink, POS = max(VALUE) / 2)
    }, grp]
  }



