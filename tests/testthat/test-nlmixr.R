test_that("nlmixr test", {

  # Data' purpose illustrates the error and my data set
  df <- tibble::tibble(
    ID = c(rep(1, 6), rep(2, 6)),
    TIME = c(0.00, 12.11, 18.41, 23.89, 36.00, 43.51, 0.00, 12.00, 20.00, 24.00, 36.80, 45.00),
    AMT = c(1000, 1000, NA, 1000, 1000, NA, 1000, 2000, NA, 1000, 1000, NA),
    DUR = c(2.5, 2.5, NA, 2.5, 2.5, NA, 2.5, 2.5, NA, 2.5, 2.5, NA),
    DV = c(NA, NA, 3.0, NA, NA, 9.6, NA, NA, 7.0, NA, NA, 2.8),
    WT = c(rep(55, 6), rep(48, 6))
  ) %>%
    dplyr::mutate(EVID = ifelse(is.na(DV), 1, 0))

  fun <- function() {
    ini({
      tvCl <- c(0, 4, Inf)
      tvVc <- c(0, 48, Inf)

      eta.Vc ~ 0.62
      prop.sd <- 0.051529

    })
    model({
      Cl <- tvCl
      Vc <- tvVc*(WT/70)*exp(eta.Vc)

      # dynamical system
      linCmt() ~ prop(prop.sd)
    })
  }

  fit <- nlmixr2::nlmixr2(fun, df, list(print=0), est="posthoc")

  expect_error(pmx_nlmixr(fit), NA)

  #fit <- nlmixr::nlmixr(fun, df, list(print=0), est="posthoc")

})


test_that("warfarin example", {

  skip_on_cran()

  PKdata <- nlmixr2data::warfarin %>%
    dplyr::filter(dvid == "cp") %>%
    dplyr::select(-dvid) %>%
    dplyr::mutate(SEX = ifelse(sex == "male", 1, 0),
                  SPARSE=ifelse(id %in% c(2, 10, 17:33), 1, 0))


    One.comp.transit <- function() {
      ini({
        # Where initial conditions/variables are specified
        lktr <- log(1.15)  #log k transit (/h)
        lcl  <- log(0.135) #log Cl (L/hr)
        lv   <- log(8)     #log V (L)
        prop.err <- 0.15   #proportional error (SD/mean)
        add.err <- 0.6     #additive error (mg/L)
        eta.ktr ~ 0.5   #IIV ktr
        eta.cl ~ 0.1   #IIV Cl
        eta.v ~ 0.1   #IIV V
      })
      model({
        cl <- exp(lcl + eta.cl)
        v  <- exp(lv + eta.v)
        ktr <- exp(lktr + eta.ktr)
        # RxODE-style differential equations are supported
        d/dt(depot)   = -ktr * depot
        d/dt(central) =  ktr * trans - (cl/v) * central
        d/dt(trans)   =  ktr * depot - ktr * trans
        ## Concentration is calculated
        cp = central/v
        # And is assumed to follow proportional and additive error
        cp ~ prop(prop.err) + add(add.err)
      })
    }

    fitOne.comp.transit_S <-
      nlmixr2::nlmixr(One.comp.transit,
                      PKdata,
                      est = "saem",
                      nlmixr2::saemControl(print = 100),
                      nlmixr2::tableControl(cwres = TRUE,npde=TRUE))

    expect_error(pmx_nlmixr(fitOne.comp.transit_S, conts = c("wt","age"),
                            cats=c("SEX","SPARSE"), vpc=FALSE,settings=pmx_settings(is.draft=FALSE)), NA)

    ctl <- pmx_nlmixr(fitOne.comp.transit_S, conts = c("wt","age"),
                      cats=c("SEX","SPARSE"), vpc=FALSE,settings=pmx_settings(is.draft=FALSE))

    expect_error(ctl %>% pmx_plot_eta_conts, NA)

    ## expect_error(ctl %>% pmx_report(name="ggPMX_report",
    ##                                 save_dir=".",
    ##                                 format="report",
    ##                                 extension="word"), NA)

    ## if (file.exists("ggPMX_report.Rmd")) unlink("ggPMX_report.Rmd")
    ## if (file.exists("ggPMX_report.docx")) unlink("ggPMX_report.docx")

})



test_that("integrated demo", {

  skip_on_cran()

  dat <- xgxr::case1_pkpd %>%
    dplyr::rename(DV=LIDV) %>%
    dplyr::filter(CMT %in% 1:2) %>%
    dplyr::filter(TRTACT != "Placebo")

  doses <- unique(dat$DOSE)
  nid <- 3 # 7 ids per dose group
  dat2 <- do.call("rbind",
                  lapply(doses, function(x) {
                    ids <- dat %>%
                      dplyr::filter(DOSE == x) %>%
                      dplyr::summarize(ids=unique(ID)) %>%
                      dplyr::pull()
                    ids <- ids[seq(1, nid)]
                    dat %>%
                      dplyr::filter(ID %in% ids)
                  }))

  cmt2 <- function(){
    ini({
      lka <- log(0.1) # log Ka
      lv <- log(10) # Log Vc
      lcl <- log(4) # Log Cl
      lq <- log(10) # log Q
      lvp <- log(20) # Log Vp

      eta.ka ~ 0.01
      eta.v ~ 0.1
      eta.cl ~ 0.1
      logn.sd = 10
    })
    model({
      ka <- exp(lka + eta.ka)
      cl <- exp(lcl + eta.cl)
      v <- exp(lv + eta.v)
      q <- exp(lq)
      vp <- exp(lvp)
      linCmt() ~ lnorm(logn.sd)
    })
  }

  cmt2fit.logn <- nlmixr2::nlmixr(cmt2, dat2, "saem",
                                  control=list(print=0),
                                  table=nlmixr2::tableControl(cwres=TRUE, npde=TRUE))

  ctr <- pmx_nlmixr(cmt2fit.logn, conts = c("WEIGHTB"), cats="TRTACT", vpc=TRUE)

})
