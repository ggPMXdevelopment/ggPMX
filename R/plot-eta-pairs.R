

#' Creates eta correlation object
#'
#' @param title character the plot title
#' @param dname name of dataset to be used
#' @param type.eta \code{character} type of eat can be 'mode' or 'mean'.'mode' byd efault
#' @param ... others graphics arguments passed to \code{\link{pmx_gpar}} internal object.
#' @param text_color color of the correlation text in the upper matrix
#' @return ecorrel object
#' @family plot_pmx
#' @export
eta_pairs <- function(
  title,
  dname=NULL,
  type.eta=c("mode","mean"),
  text_color="black",
  has.shrink=TRUE,
  smooth = list(se = FALSE, linetype = 2, size = 1.5, method = 'loess',color="red"),
  point = list(shape = 1, color = "grey50", size = 1,colour="black"),
  shrink=list(fun="sd",size=5),
  ...){
  assert_that(is_string_or_null(dname))
  if(is.null(dname)) dname <- "eta"
  if(missing(title)) title <- "Correlations of random effects"
  labels <- list(
    title = title,
    subtitle ="",
    x = "",
    y = "")
  structure(list(
    dname = dname,
    labels=labels,
    point = point,
    type.eta =  match.arg(type.eta),
    text_color=text_color,
    has.shrink=has.shrink,
    shrink=shrink,
    smooth=smooth,
    point=point,
    gp = pmx_gpar(
      ptype="ETA_PAIRS",
      labels=labels,
      discrete = FALSE,
      has.smooth = FALSE,
      has.band = FALSE, ...)
    
  ), class =c("eta_pairs", "pmx_gpar"))
}


lower.plot <- function(data, x,y, point,smooth,gp) {
  p <- 
    ggplot(data = data,aes_string(x=x,y=y)) +
    with(point,geom_point(shape=shape,size=size,color=color))+
    with(smooth,geom_smooth(method = method, se=se, size=size,color=color))
  plot_pmx(gp, p)
}

diag.plot <- function(data, x, gp) {
  p <-  ggally_densityDiag(data = data, aes_string(x=x)) 
  plot_pmx(gp, p)
}


upper.plot <- function(data, x,y, text_color,gp) {
  p <-  ggally_cor(data = data, aes_string(x=x,y=y),colour=text_color) 
  plot_pmx(gp, p)
}



.plot_matrix <- 
  function(dx,text_color=text_color,point=point,smooth=smooth,gp){
  nn <- colnames(dx)
  mat <- outer(nn,nn,paste,sep="_")
  uppers <- 
    lapply(
      mat[upper.tri(mat)],
      function(z){
        z <- strsplit(z,"_")[[1]]
        upper.plot(dx,x=z[1],y=z[2],text_color=text_color,gp=gp)
      } )
  uppers <- setNames(uppers,mat[upper.tri(mat)])
  
  lowers <- 
    lapply(
      mat[lower.tri(mat)],
      function(z){
        z <- strsplit(z,"_")[[1]]
        lower.plot(dx,x=z[1],y=z[2],point=point,smooth=smooth,gp=gp)
      } )
  
  lowers <- setNames(lowers,mat[lower.tri(mat)])
  
  
  diags <-
    lapply(
      diag(mat),
      function(z){
        z <- strsplit(z,"_")[[1]]
        diag.plot(dx,x=z[1],gp=gp)
      } )
  
  diags <- setNames(diags,diag(mat))
  
  ll <- c(uppers,diags,lowers)
  
  ll[unlist(as.list(t(mat)))]
}



#' Remove named elements from gtable
#'
#' @param table The table from which grobs should be removed
#' @param names A character vector of the grob names (as listed in \code{table$layout})
#'   that should be removed
#' @param ... Other parameters passed through to \code{gtable_filter}.

gtable_remove_grobs <- function(table, names, ...)
{
  kept_names <- table$layout$name[!(table$layout$name %in% names)]
  gtable::gtable_filter(table, paste(kept_names, sep="", collapse="|"), ...)
}



plot_shrink <- 
  function(x,shrink.dx,shrink){
    
    EFFECT <- SHRINK <- NULL
  label <- shrink.dx[
      EFFECT==x,
      sprintf("%s%%",round(SHRINK*100))
      ]
  params <- c(label=label,shrink)
  do.call(ggally_text,params[names(params)!="fun"]) + 
  ## ggally_text(label=label)+  
  theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  }


#' Plot random effect correlation plot
#'
#' @param x distribution object
#' @param dx data set
#' @param ... not used for the moment
#' @return ggpairs plot
#' @export
#' @seealso \code{\link{distrib}}
#' @family plot_pmx
#' @import ggplot2
#' @import GGally
#'
plot_pmx.eta_pairs <- function(x, dx,...){
  ## avoid RCMDCHECK warning
  ID <- EFFECT <- VALUE <- FUN <- NULL
  
  
  ## filter by type of eta 
  dx <- dx[FUN==x$type.eta]
  if(nrow(dx)==0){
    message("No rows find for eta of type ",x$type.eta,"\n")
    return(NULL)
  }
  data_plot <- 
    dcast(dx[,list(ID,EFFECT,VALUE)],ID~EFFECT,
          fun.aggregate = max,value.var = "VALUE")[,-"ID",with=F]
  nn <- colnames(data_plot)
  p <- with(x,{
    
    plots <- .plot_matrix(data_plot,
                          text_color=text_color,
                          point=point,
                          smooth=smooth,
                          gp=gp)
    
    if(has.shrink){
      dd <- x[["shrink.dx"]]
      ll <- lapply(nn,plot_shrink,shrink.dx,shrink)
      plots <- c(ll,plots)
    }
    
    ggmatrix(plots ,
             title = labels$title,
             xAxisLabels=nn,
             yAxisLabels=if(has.shrink) c("Shrinkage",nn) else nn,
             showYAxisPlotLabels=TRUE,
             switch="both",
             xlab=labels$x,
             ylab=labels$y,
             byrow=TRUE,
             nrow=length(nn)+has.shrink*1,
             ncol=length(nn),
             yProportions=if(has.shrink)c(1,rep(5,length(nn))))
  })
  p$has.shrink <- x$has.shrink
  attributes(p)$class <- c("pmx_eta_matrix","gg", "ggmatrix")
  p +
    theme(
      strip.background = element_rect(fill = "white"), 
      strip.placement = "outside",
      strip.text = element_text( face = "bold",size=12)
    )
}


ggplot2_set_last_plot <- utils::getFromNamespace("set_last_plot", "ggplot2")
#' @export
#' @method print pmx_eta_matrix
#' @import grid 
print.pmx_eta_matrix <- function (x, newpage = is.null(vp), vp = NULL, ...) {
  if (newpage) {
    grid.newpage()
  }
  grDevices::recordGraphics(requireNamespace("GGally", quietly = TRUE),
                            list(), getNamespace("GGally"))
  eta_gtable <- ggmatrix_gtable(x, ...)
  if(x$has.shrink){
    eta_gtable <- gtable_remove_grobs(eta_gtable, "axis-l-1")
    strip_l_1 <- gtable::gtable_filter(eta_gtable,"strip-l-1")
    strip_l_1$grobs[[1]]$grobs[[1]]$children$GRID.stripGrob.395$children$GRID.text.394$rot=0
    strip_l_1$grobs[[1]]$grobs[[1]]$children$GRID.stripGrob.395$children$GRID.text.394$hjust =-0.03
    matches <- grepl("strip-l-1", eta_gtable$layout$name, fixed = TRUE)
    eta_gtable$grobs[[which(matches)]] <- strip_l_1
    
  }
  
  # must be done after gtable, as gtable calls many ggplot2::print.ggplot methods
  ggplot2_set_last_plot(x)
  
  if (is.null(vp)) {
    grid.draw(eta_gtable)
  } else {
    if (is.character(vp)) {
      seekViewport(vp)
    } else {
      pushViewport(vp)
    }
    grid.draw(eta_gtable)
    upViewport()
  }
}


