#' filter data in pmx controller
#'
#' @param ctr A controller. An object of 'pmxClass'
#' @param data_set A data_set within the controller to apply a filter to.
#' @param expression A filter expression
#'
#' @return Returns a pmx controller with a filtered data set.
#' @export
pmx_filter <- function(ctr, data_set, expression){
  assert_that(is_pmxclass(ctr))
}
