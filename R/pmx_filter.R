#' filter data in a pmx controller
#'
#' @param ctr A controller. An object of 'pmxClass'
#' @param data_set A data_set within the controller to apply a filter to.
#' @param pmx_exp A filter expression
#'
#' @return Returns a pmx controller with a filtered data set.
#' @export
#' @examples 
#' \dontrun{
#' ## example of global filter
#' ctr <- pmx_mlx("standing")
#' ctr %>% pmx_filter(data_set = "prediction", ID == 5 & TIME <2)
#' ctr %>% get_data("prediction")
#' }
pmx_filter <- 
  function(ctr, data_set = c("estimates","predictions", 
                            "eta", "finegrid", "shrink"), pmx_exp){
    assert_that(is_pmxclass(ctr))
    data_set <- match.arg(data_set)
    assert_that(is_language_or_string(substitute(pmx_exp)))
    if(is_string(substitute(pmx_exp))){
      pmx_exp <- expression(pmx_exp)
    }
    oldData <- ctr[["data"]][[data_set]]
    e <- substitute(pmx_exp)
    r <- eval(e, oldData, parent.frame())
    if (!is.logical(r)) 
      stop("'expression' must evaluate to logical")
    r <- r & !is.na(r)
    
    newData <- oldData[r]
    ctr[["data"]][[data_set]] <- newData
    ctr
  }
