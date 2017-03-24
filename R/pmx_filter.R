#' filter data in pmx controller
#'
#' @param ctr A controller. An object of 'pmxClass'
#' @param data_set A data_set within the controller to apply a filter to.
#' @param expression A filter expression
#'
#' @return Returns a pmx controller with a filtered data set.
#' @export
pmx_filter <- 
  function(ctr, data_set =c("par_est","mod_pred", 
                            "ind_pred", "shrink"), expression){
    assert_that(is_pmxclass(ctr))
    data_set <- match.arg(data_set)
    assert_that(is.expression_or_string(expression))
    oldData <- ctr[["data"]]
    newData <- ctr[["data"]][[data_set]][, expression]
    ctr$data <- newData
    ctr
  }
