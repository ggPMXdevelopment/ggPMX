if (helper_skip()) {

  context("Test shrinkage computation")
  pmxClassHelpers <- test_pmxClass_helpers()


  test_that("test shrinkage for standing config", {
    ctr <- pmxClassHelpers$ctr
    expect_is(ctr, "pmxClass")

    res <- ctr %>% pmx_comp_shrink()
    expect_is(res, "data.frame")
    expect_equal(colnames(res), c("EFFECT", "OMEGA", "SHRINK", "POS", "FUN"))
    expect_true(all(res$SHRNK < 1))
  })


  test_that("test shrinkage fun parameter", {
    ctr <- pmxClassHelpers$ctr
    expect_is(ctr, "pmxClass")
    res.var <- ctr %>% pmx_comp_shrink(fun = "var")
    res.sd <- ctr %>% pmx_comp_shrink(fun = "sd")
    expect_true(all(res.var$SHRINK > res.sd$SHRINK))
  })


  test_that("variance (var) is default shrinkage fun parameter", {
    ctr <- pmxClassHelpers[["ctr"]]
    expect_is(ctr, "pmxClass")

    lapply(
      c("ETA_HIST", "ETA_BOX", "ETA_MATRIX", "ETA_QQ"),
      function(plot) {
        expect_identical("var", ctr[["config"]][["plots"]][[plot]][["shrink"]][["fun"]])
      }
    )
  })
}
