#' DV vs PRED plot
#'
#' @param labels list that contain title,subtitle, axis labels
#' @param point geom point graphical parameters
#' @param add_hline logical if TRUE add horizontal line y=0 ( TRUE by default)
#' @param dname name of dataset to be used
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.

#'
#' @return a ggplot2 object
#' @export
#' @family plot_pmx
#' @seealso \code{\link{plot_pmx.residual}}
#' @details 
#' Some parameters are a list of parameters :
#' 
#' \strong{point} is a list that contains:
#' \itemize{
#' \item {\strong{shape:}} {default to 1}
#' \item {\strong{color:}} {default to black}
#' \item {\strong{size:}} {default to 1}
#' }
#' 
#' \strong{labels} is a list that contains:
#' \itemize{
#' \item {\strong{title:}} {plot title default to "DV vs PRED"}
#' \item {\strong{subtitle:}} {plot subtitle default empty}
#' \item {\strong{x:}} {x axis label "PRED"}
#' \item {\strong{y:}} {y axis label "DV"}
#' }
dv_pred <- function(
  ctr,
  labels = list(
    title="DV vs PRED",
    subtitle = "",
    x = "PRED",
    y = "DV"
  ), 
  point = list(shape = 1, color = "black", size = 1), 
  add_hline=FALSE, 
  dname="predictions",
  has.smooth=TRUE,
  smooth=list(se=FALSE,color="red",linetype=1),
  has.identity_line=TRUE,
  identity_line=list(intercept=0,color="blue"),
  ...){
  
  
  stopifnot(is_pmxclass(ctr))
  cctr <- pmx_copy(ctr) 
  assert_that(is_list_or_null(labels))
  assert_that(is_string_or_null(dname))
  assert_that(is.list(point))
  
  cctr %>%
    pmx_update(
        "dv_pred",
        labels=labels,
        point=point,
        add_hline=add_hline,
        dname=dname,
        has.smooth=has.smooth,
        smooth=smooth,
        has.identity_line=has.identity_line,
        identity_line=identity_line
      )
  
  p <- cctr %>%  get_plot("dv_pred")
  rm(cctr)
  p
}


# ctr <- theophylline()
# ctr %>% dv_pred