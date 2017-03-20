#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
#' Merge 2 lists
#'
#' left join , the first list is updated by the seond one
#' @param base_list  list to update
#' @param overlay_list list used to update the first list
#' @param recursive logical if TRUE do the merge in depth
#'
#' @return list
#' @export
l_left_join <-
  function (base_list, overlay_list, recursive = TRUE)
  {
    if (length(base_list) == 0)
      overlay_list
    else if (length(overlay_list) == 0)
      base_list
    else {
      merged_list <- base_list
      for (name in names(overlay_list)) {
        base <- base_list[[name]]
        overlay <- overlay_list[[name]]
        if (is.list(base) && is.list(overlay) && recursive)
          merged_list[[name]] <- l_left_join(base, overlay)
        else {
          merged_list[[name]] <- NULL
          merged_list <-
            append(merged_list,
                   overlay_list[which(names(overlay_list) %in%
                                        name)])
        }
      }
      merged_list
    }
  }


## shiny:::anyUnnamed
anyUnnamed. <- function (x)
{
  if (length(x) == 0)
    return(FALSE)
  nms <- names(x)
  if (is.null(nms))
    return(TRUE)
  any(!nzchar(nms))
}

## shiny:::mergeVectors
mergeVectors. <- function (a, b)
{
  if (anyUnnamed.(a) || anyUnnamed.(b)) {
    stop("Vectors must be either NULL or have names for all elements")
  }
  x <- c(a, b)
  drop_idx <- duplicated(names(x), fromLast = TRUE)
  x[!drop_idx]
}
## shiny:::dropNulls
dropNulls. <-
function (x)
{
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


