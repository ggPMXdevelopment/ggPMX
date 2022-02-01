#' Creates parameter kable
#'
#' @param ctr Generated controller from e.g. \code{\link{pmx_mlx}} for Monolix.
#' @param return_table If TRUE, returns the same table as in get_data('estimates') otherwise it returns a kable
#' @param fun \code{character} can be "sd" or "var" for shrinkage computation, see \code{\link{pmx_comp_shrink}}
#' @param scientific \code{logical} set to TRUE to get scientific notation of parameter values, or FALSE otherwise
#' @param digits \code{integer} the number of significant digits to use when rounding parameter values
#' @return Returns a kable with the parameter estimates from get_data('estimates')
#' @export
#'
#' @examples
#' #ctr <- theophylline()
#' #my_params <- ctr %>% param_table(fun = "var")

param_table <- function(ctr, fun, return_table=FALSE, scientific=TRUE, digits=2) {

  # Argument Checks
  if(missing(fun)) fun <- c("var","sd")
  stopifnot(
    is.character(fun),
    fun %in% c("sd", "var") | identical(fun, c("var", "sd")),
    is.logical(return_table),
    length(return_table) == 1,
    is.logical(scientific),
    length(scientific) == 1,
    is.numeric(digits),
    length(digits) == 1,
    digits >= 0
  )

  ## Avoid error message in cmd check

  SE <- EFFECT <- SHRINK <- RSE <- VALUE <- PARAM <- . <- NULL

  pop_pars <- ctr %>% get_data("estimates")
  rownames(pop_pars) <- trimws(as.character(pop_pars[["PARAM"]]))

  if(return_table) {return(as.data.table(pop_pars))}

  config_sys_nm <- ctr[["config"]][["sys"]] == "nm"

  config_sys_nlmixr <- ctr[["config"]][["sys"]] == "nlmixr"

  # Selecting function to use for shrinkage calculation
  fun <- ifelse(
    identical(fun, c("var", "sd")),
    ifelse(config_sys_nlmixr, "sd", ifelse(config_sys_nm, "sd", "var")),
    fun
  )

  message("`", fun, "`",  " was used for shrinkage calculation")

  # For nonmem data is used of get_data("omega"), which is the SD
  shrinkage <- ctr %>% pmx_comp_shrink(fun=fun) %>%
    dplyr::transmute(
      # make % shrinkage
      SHRINK=paste0(round(SHRINK * 100), "%"),
      # For non-nm sys PARAM has "omega_" prefix
      PARAM=paste0(ifelse(config_sys_nm, "", ifelse(config_sys_nlmixr, "", "omega_")), EFFECT),
      )

  if (ctr[["config"]][["sys"]] == "nm") {
    i <- 1
    for(i in i:length(shrinkage[["PARAM"]])) {
      str <- shrinkage[["PARAM"]][i]
      num <- as.numeric(substring(str, 4, ))
      shrinkage[["PARAM"]][i] <- paste0("OMEGA(", num, ",", num, ")")
    }

    pop_pars <- dplyr::mutate(pop_pars, RSE, ifelse(SE == 1E10, "fixed", round(RSE)))
  } else if (ctr[["config"]][["sys"]] == "nlmixr") {
    eta_trans <- ctr[["config"]][["eta_trans"]]
    shrinkage[, PARAM := eta_trans[PARAM]]
  }

  blank_row <- data.frame(PARAM=".", VALUE="", RSE ="", SHRINK="")

  pop_pars %>%
  dplyr::transmute(
    PARAM=trimws(PARAM),
    VALUE=signif(VALUE, digits) %>% {
      if(scientific) {scales::label_scientific(digits=Inf)(.)}
      else {as.character(.)}
    },
    # Make RSE a %, and change NaN to "fixed"
    RSE=ifelse(is.infinite(RSE), "", ifelse(is.na(RSE), "fixed", paste0(round(RSE), "%"))),
  ) %>%
  dplyr::arrange(PARAM) %>%
  dplyr::left_join(shrinkage, by="PARAM") %>%
  tidyr::replace_na(list(SHRINK="")) %>%
  {suppressWarnings({
    if(ctr[["config"]][["sys"]] %in% c("mlx", "mlx18" )) {
      dplyr::bind_rows(
        blank_row,
        dplyr::filter(., grepl("_pop", PARAM)),
        blank_row,
        dplyr::filter(., grepl("omega_", PARAM)),
        blank_row,
        dplyr::filter(., grepl("beta_", PARAM)),
        blank_row,
        dplyr::filter(., PARAM %in% c("a", "b")),
     )
    } else if(ctr[["config"]][["sys"]] == "nm") {
      dplyr::bind_rows(
        dplyr::filter(., grepl("OBJ", PARAM)),
        blank_row,
        dplyr::filter(., grepl("THETA", PARAM)),
        blank_row,
        dplyr::filter(., grepl("SIGMA", PARAM)),
        blank_row,
        dplyr::filter(., grepl("OMEGA", PARAM)),
      )
    } else if (ctr[["config"]][["sys"]] == "nlmixr") {
      param_regs <- ctr[["config"]][["param_regs"]]
      dplyr::bind_rows(
        dplyr::filter(., grepl("OBJ", PARAM)),
        blank_row,
        dplyr::filter(., grepl(param_regs["theta"], PARAM)),
        blank_row,
        dplyr::filter(., grepl(param_regs["eta"], PARAM)),
        blank_row,
        dplyr::filter(., grepl(param_regs["err"], PARAM)))
    }
  })} %>%
  dplyr::bind_rows(data.frame(PARAM=".",VALUE="",RSE ="", SHRINK=paste("comp. with", fun))) %>%
  dplyr::rename(Parameter=PARAM, Estimate=VALUE, Relative_SE=RSE, Shrinkage=SHRINK) %>%
  knitr::kable(
    x=.,
    col.names=c("Parameter", "Value", "Relative Standard Error", "Shrinkage"),
    caption=paste0("Parameter estimates (", nrow(pop_pars) + 1, " lines)"),
    align="lrrr"
  )
}
