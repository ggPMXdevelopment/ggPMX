before_add_check <- function(self, private, x, pname) {
  assert_that(is_pmx_gpar(x))
  if (missing(pname)) stop("plot name is missing")
  pname <- tolower(pname)
  private$.plots_configs[[pname]] <- x
  ptype <- self[["config"]][["plots"]][[toupper(pname)]][["ptype"]]

  ##check if use sim_blq
  if(is.null(x$gp$sim_blq)){
    use_sim_blq <- self$sim_blq
  } else {
    use_sim_blq <- x$gp$sim_blq
  }
  
  if(use_sim_blq){
    if (x$ptype == "SCATTER" | x$ptype == "PMX_DENS" | x$ptype == "PMX_QQ"){
      x$dname <- "sim_blq"
    }
    
  } else{
    if (x$ptype == "SCATTER" | x$ptype == "IND" && !x$use.finegrid | x$ptype == "PMX_DENS" | x$ptype == "PMX_QQ") {
      x$dname <- "predictions"
    }
  }

  if(pname == "eta_qq") { #quick fix for eta
    x$dname <- "eta"
  }
  
  dname <- x$dname
  dx <- copy(self$data[[dname]])
  ## if(is.null(dx))return(NULL)
  if (is.null(dx) || nrow(dx) == 0) {
    private$.plots[[pname]] <- NULL
    if (!(dname %in% c("sim", "saem"))) {
      cat(sprintf(
        "No data %s provided for plot %s",
        sprintf("%s", dname), sprintf("%s", pname)
      ), "\n")
    }
    return(NULL)
  }
  assert_that(is.data.table(dx))
  x$input <- self %>% get_data("input")

  if(pname == "pmx_vpc" & use_sim_blq){
    x$input <- self %>% get_data("sim_blq")
  }
  
  x$dx <- dx
  x
}

.strat_supported <- function(x) {
  if (!x$strat && !is.null(x[["strat.facet"]])) {
    message("facet stratification is not yet implemented")
    x$strat.facet <- NULL
  }
  if (!x$strat && !is.null(x[["strat.color"]])) {
    message("color stratification is not yet implemented")
    x$strat.color <- NULL
  }
  invisible(x)
}

.filter_x <- function(x) {
  if (!is.null(x[["filter"]])) {
    x$dx <- x[["filter"]](x$dx)
    if (x$ptype == "IND") x$input <- x[["filter"]](x$input)
    if (x$ptype == "VPC") {
      x$db <- lapply(x$db, function(ds) ds <- x[["filter"]](ds))
      x$input <- x[["filter"]](x$input)
    }
  }
  invisible(x)
}

.strat_x <- function(x) {
  if (!is.null(x[["strat.color"]])) {
    strat.color <- x[["strat.color"]]
    if (is.formula(strat.color)) {
      strat.color <- setdiff(as.character(strat.color), "~")
    }
    gp <- x[["gp"]]
    gp[["labels"]][["legend"]] <- strat.color
    x[["gp"]] <- gp
  }

  if (isFALSE(x[["gp"]][["custom_title"]])) {
    x[["gp"]][["labels"]][["title"]] <- gsub(" by .*", "", x[["gp"]][["labels"]][["title"]])
    if (!is.null(x[["strat.facet"]])) {
      x[["gp"]][["labels"]][["title"]] <- sprintf(
          "%s by %s",
          x[["gp"]][["labels"]][["title"]],
          formula_to_text(x[["strat.facet"]])
        )
    }
  }
  invisible(x)
}

.trans_x <- function(x) {
  if (!is.null(x[["trans"]])) {
    dx1 <- copy(x$dx)
    x$dx <- pmx_transform(x, dx1, x[["trans"]])
    if (x$ptype == "IND") {
      inp <- copy(x$input)
      x$input <- pmx_transform(x, inp, x[["trans"]])
    }
  }
  invisible(x)
}

.filter_eta_x <- function(x) {

  dx <- x$dx
  grp <- as.character(unlist(lapply(x[["strat.facet"]], as.list)))
  grp <- unique(intersect(c(grp, as.character(x[["strat.color"]])), names(dx)))
  if (x$ptype == "DIS") {
    VAR <- FUN <- NULL
    if (exists("FUN", dx)) dx <- dx[grepl("mode", FUN)]
    cols <- c("ID", "EFFECT", "VALUE", grp)
    x$dx <- unique(dx[, cols, with = FALSE])
  }
  invisible(x)
}

.shrink_x <- function(x, self) {
  if (!is.null(x[["is.shrink"]]) && x$is.shrink) {
    x[["shrink.dx"]] <-
      self %>%
      pmx_comp_shrink(
        fun = x$shrink$fun,
        strat.color = x[["strat.color"]],
        strat.facet = x[["strat.facet"]],
        filter = x[["filter"]]
      )
  }
  invisible(x)
}

.add_cats_x <- function(x, self) {
  if (x$ptype == "ETA_COV") {
    x[["cats"]] <- self %>% get_cats()
    x[["conts"]] <- self %>% get_conts()
  }
  invisible(x)
}

.settings_x <- function(x, self) {
  
  if (!is.null(self$settings)) {
    x$gp$is.draft <- self$settings$is.draft
    x$gp$color.scales <- self$settings$color.scales
    ## If use.abbrev = TRUE - show abbreviation, else full description
    if ("use.abbrev" %in% names(self$settings) && !self$settings$use.abbrev) {
      x$gp$labels$x <- self %>% get_abbrev(x$gp$labels$x)
      x$gp$labels$y <- self %>% get_abbrev(x$gp$labels$y)
    }
    if ("labeller" %in% names(self$settings)) {
      x$facets$labeller <- self$settings$labeller
    }
    if (!self$settings$use.titles) {
      x$gp$labels$title <- ""
    }
    if (!is.null(self$settings$effects)) {
      EFFECT <- NULL
      effs <- self$settings$effects
      if (!is.null(x[["is.shrink"]]) && x$is.shrink) {
        x[["shrink.dx"]][, EFFECT := factor(EFFECT, levels = effs$levels, labels = effs$labels)]
      }
      if (exists("EFFECT", x$dx)) {
        x$dx[, EFFECT := factor(EFFECT, levels = effs$levels, labels = effs$labels)]
        if ((x$ptype == "ETA_COV") && (x$type == "cats"))
          x$dx <- x$dx[!(is.na(x$dx[["EFFECT"]])), ]
      }
    }
  }
  invisible(x)
}

.bloq_x <- function(x, self) {
  if (is.null(is.null(x[["bloq"]])) && !is.null(self$bloq)) {
    x[["bloq"]] <- self$bloq
  }
  if (!is.null(x[["bloq"]])) {
    dx <- self %>% get_data("input")
    if (!x$bloq$cens %in% names(dx)) {
      x$bloq <- NULL
      old_class <- class(x)
      x <- append(x, list(bloq = NULL))
      class(x) <- old_class
    } else {
      if (!x[["bloq"]]$show) {
        x$dx <- x$dx[!get(x$bloq$cens) %in% c(1, -1)]
        x$bloq <- NULL
        old_class <- class(x)
        x <- append(x, list(bloq = NULL))
        class(x) <- old_class
      } else {
        x[["bloq"]]$show <- NULL
      }
    }
  }

  invisible(x)
}

pmx_add_plot <- function(self, private, x, pname) {
  x <- before_add_check(self, private, x, pname)
  if (is.null(x)) {
    return(invisible(self))
  }
  assert_that(is_pmx_gpar(x))
  
  x <- x %>%
    .strat_supported() %>%
    .filter_x() %>%
    .strat_x() %>%
    .trans_x() %>%
    .filter_eta_x() %>%
    .shrink_x(self) %>%
    .add_cats_x(self) %>%
    .settings_x(self) %>%
    .bloq_x(self) %>%
    .vpc_x(self)
  self$set_config(pname, x)
  private$.plots[[pname]] <- plot_pmx(x, dx = x$dx)
  invisible(self)
}
