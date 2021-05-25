context("vdiffr")
test_that("vdiffr", {
  skip_if_not(R.version$major >= 4 & R.version$minor >= 1.0)
  skip_if(TRUE)
  set.seed(42)
  ctr <- theophylline()
  args = commandArgs(trailingOnly = TRUE)

  if (length(args)==0) {
    wd <- getwd()
  } else if (length(args)==1) {
    # output directory
    wd = args[1]
  } else {
    stop("More than one argument was supplied! Need only 1 - root output directory", call.=FALSE)
  }

  f1 <- function(x) {pmx_plot_abs_iwres_ipred(x)}
  f2 <- function(x) {pmx_plot_iwres_ipred(x)}
  f3 <- function(x) {pmx_plot_iwres_time(x)}
  f4 <- function(x) {pmx_plot_iwres_dens(x)}
  f5 <- function(x) {pmx_plot_iwres_qq(x)}
  f6 <- function(x) {pmx_plot_npde_time(x)}
  f7 <- function(x) {pmx_plot_npde_pred(x)}
  f8 <- function(x) {pmx_plot_npde_qq(x)}
  f9 <- function(x) {pmx_plot_dv_pred(x)}
  f10 <- function(x) {pmx_plot_individual(x)}
  f11 <- function(x) {pmx_plot_dv_ipred(x)}
  f12 <- function(x) {pmx_plot_eta_hist(x)}
  f13 <- function(x) {pmx_plot_eta_box(x)}
  f14 <- function(x) {pmx_plot_eta_matrix(x)}
  f15 <- function(x) {pmx_plot_eta_cats(x)}
  f16 <- function(x) {pmx_plot_eta_conts(x)}
  f17 <- function(x) {pmx_plot_eta_qq(x)}
  f18 <- function(x) {pmx_plot_vpc(x)}
  fun_list <- list(fun_pmx_plot_abs_iwres_ipred = f1,
                   fun_pmx_plot_iwres_ipred = f2,
                   fun_pmx_plot_iwres_time = f3,
                   fun_pmx_plot_iwres_dens = f4,
                   fun_pmx_plot_iwres_qq = f5,
                   fun_pmx_plot_npde_time = f6,
                   fun_pmx_plot_npde_pred = f7,
                   fun_pmx_plot_npde_qq = f8,
                   fun_pmx_plot_dv_pred = f9,
                   fun_pmx_plot_individual = f10,
                   fun_pmx_plot_dv_ipred = f11,
                   fun_pmx_plot_eta_hist = f12,
                   fun_pmx_plot_eta_box = f13,
                   fun_pmx_plot_eta_matrix = f14,
                   fun_pmx_plot_eta_cats = f15,
                   fun_pmx_plot_eta_qq = f17,
                   fun_pmx_plot_vpc = f18,
                   fun_pmx_plot_eta_conts = f16
                   )

  lapply(c(names(fun_list)),
         function(n) {
           vdiffr::expect_doppelganger(n, fun_list[[n]](ctr))
         })

})
