

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
  title,dname=NULL,type.eta=c("mode","mean"),
  text_color="black",
  has.shrink=TRUE,
  smooth = list(se = FALSE, linetype = 2, size = 1.5, method = 'loess',color="red"),
  point = list(shape = 1, color = "grey50", size = 1),
  shrink=list(
    fun="sd",size=5,color="black", hjust=-1,vjust=5),
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
    point = list(shape = 1, color = "grey50", size = 1),
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



.plot_matrix <- function(dx,text_color=text_color,point=point,smooth=smooth,gp){
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
  
  ll[unlist(as.list(mat))]
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
  if(nrow(dx)==0)
    stop("Now rows find for eta of type ",x$type.eta,"\n")
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
      ll <- 
        lapply(nn,
               function(x){
                 ggally_text(label=shrink.dx[EFFECT==x,round(SHRINK*100)])+
                   theme_bw()+
                   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank())
               })
      plots <- c(ll,plots)
    }
    
    ggmatrix(plots ,
             title = labels$title,
             xlab=labels$x,
             ylab=labels$y,
             byrow=TRUE,
             nrow=length(nn)+has.shrink*1,
             ncol=length(nn),
             yProportions=if(has.shrink)c(1,rep(5,length(nn))))
  })
  
  # p <- with(x, {
  #   ggpairs(
  #     data_plot, 
  #     lower = list(continuous = wrap(lower.plot, method = "loess",gp=gp,point=point)),
  #     diag = list(continuous = wrap(diag.plot,gp=gp)),
  #     upper = list(continuous = wrap(upper.plot,gp=gp,text_color=text_color)),
  #     switch="y",
  #     labeller = function (labels, multi_line = TRUE) 
  #     {
  #       if(names(labels)== "Var1" && x$has.shrink) 
  #         dd <- x[["shrink.dx"]]
  #       browser()
  #       labels$Var1 <- paste(labels$Var1,"10%",sep="\n")
  #       labels
  #       
  #     },
  #     ...)
  #   
  # })
  p
  
}
