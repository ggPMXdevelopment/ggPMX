#' Creates vpc bins 
#'
#' @param ... 
#'
#' @export
#' @family vpc

pmx_bin <- 
  function(...){
    
  }


#' Sets vpc observation layer
#'
#' @param show \code{logical} if TRUE show observation points
#' @param color \code{character} Color of the observed endpoint values. Default: "#000000".
#' @param size \code{numeric} Size of the observed endpoint values. Default: 1.
#' @param alpha \code{numeric} Transparency of the observed endpoint values. Default: 0.7.
#' @param shape \code{numeric} Shape of the observed endpoint values. Default: 1.
#'
#' @export
#' @family vpc
pmx_obs <- 
  function(show=TRUE,
           color ="#000000",
           size =1,
           alpha = 0.7,
           shape = 1){
    
    if (show){
      structure(
        list(
          color=color,
          size = size,
          alpha=alpha,
          shape=shape
        ),
        class= c("pmx_obs","list")
      )
    }
    
  }


#' Sets vpc percentile layer
#'
#' @param show  \code{charcater} how lines are displayed:
#' \itemize{
#' \item {\strong{show=all}} {lines will be displayed for each of 
#' the 3 percentiles. }
#' \item {\strong{show=median}} {Show only median line.}
#' }

#' @param interval \code{numeric} quantiles values default 
#' to \code{c(.05,.95)}
#' @param median \code{list} containg: \cr
#' \itemize{
#' \item {\strong{color}} {\code{charcater}  Color of the median percentile line. Default: "#000000". }
#' \item {\strong{size}} {\code{numeric}  Thickness of the median percentile line. Default: 1.}
#' \item {\strong{alpha}} {\code{numeric} Transparency of the median percentile line. Default: 0.7.}
#' \item {\strong{linetype}} {\code{charcater} Linetype of the median percentile line. Default: "solid".}
#' }

#' @param extreme \code{list} containg: \cr
#' \itemize{
#' \item {\strong{color}} {\code{charcater} Color of the median percentile line. Default: "#000000". }
#' \item {\strong{size}} {\code{numeric} Thickness of the median percentile line. Default: 1.}
#' \item {\strong{alpha}} {\code{numeric} Transparency of the median percentile line. Default: 0.7.}
#' \item {\strong{linetype}} {\code{charcater} Linetype of the median percentile line. Default: "solid"}
#' }
#'
#' @family vpc
#' @export
pmx_pi <- 
  function(show = c("all","median"),
           interval=c(.05,.95),
           median=list(color ="#000000",size =1,alpha = 0.7,linetype = "solid"),
           extreme=list(color ="#000000",size =1,alpha = 0.7,linetype = "solid")
  ){
    
    show = match.arg(show)
    structure(
      list(show=show,
           probs=interval,
           median=median,
           extreme=extreme),
      class= c("pmx_pi","list")
    )
    
  }



#' Sets vpc confidence interval layer

#' @param show  \code{charcater} how areas are displayed:
#' \itemize{
#' \item {\strong{show="all"}} {areas will be displayed for each of the 3 percentiles. }
#' \item {\strong{show="median"}} {Show only median area.}
#' }

#' @param interval \code{numeric} quantiles values default to \code{c(.05,.95)}
#' @param method \code{charcater} which areas are displayed:
#' \itemize{
#' \item {\strong{method="ribbon"}} {areas are ribbons.}
#' \item {\strong{method="rectangle"}} {ares are horizontal rectangles.}
#' }

#' @param median \code{list} containg: \cr
#' \itemize{
#' \item {\strong{fill}} { \code{character} Color of the area representing the CI for the median. Default: "#3388cc".}
#' \item {\strong{alpha}} {\code{numeric} Transparency of the area representing the PI for the median. Default=0.3.}
#' }

#' @param extreme \code{list} containg: \cr
#' \itemize{
#' \item {\strong{fill}} {\code{character} Color of the area representing the CI for the extreme percentiles. Default: "#3388cc".}
#' \item {\strong{alpha}} {\code{numeric} Transparency of the area representing the PI for the extreme percentiles. Default=0.3.}
#' }
#'
#' @export

#' @family vpc
pmx_ci <- 
  function(show = c("all","median"),
           interval=c(.05,.95),
           method = c("ribbon","rectangle"),
           median=list(fill="#3388cc",alpha=0.3),
           extreme=list(fill="#3388cc",alpha=0.3)){
    show = match.arg(show)
    method = match.arg(method)
    structure(
      list(show=show,
           method=method,
           probs=interval,
           median=median,
           extreme=extreme),
      class= c("pmx_ci","list")
    )
    
  }


#' Sets vpc rug layer 
#'
#' @param show  \code{logical} If TRUE show bin separators 
#' @param color \code{character} Color of the rug. Default: "#000000". 
#' @param size  \code{numeric} Thickness of the rug. Default: 1.
#' @param alpha  \code{numeric} Transparency of the rug. Default: 0.7.
#' 
#' @details 
#' 
#' When the vpc confidence interval layer methid  ???rectangles??? we don't show rug separators.

#'
#' @export
#'
#' @family vpc
pmx_rug <- 
  function(show=TRUE,
           color = "#000000",
           size = 1,
           alpha =0.7 ){
    
    if(show){
      structure(
        list(
          color=color,
          size=size,
          alpha=alpha
        ),
        class=c("pmx_rug","list")
      )
    }
    
  }










quantile_dt <- 
  function(dx,grp="time",ind="y",probs=c(.05,.95),prefix="p",wide=FALSE){
    
    probs <- sort(unique(c(0.5,probs)))
    fmt <- ifelse(probs<.1,paste0(prefix,"0%1.f"),paste0(prefix,"%1.f"))
    probs.n <- sprintf(fmt, probs*100)
    if(wide){
      dd <- dx[,as.list(quantile(get(ind),probs=probs)),grp]
      setnames(dd,grep("%",names(dd)),probs.n)
    }else{
      ds <- dx[,quantile(get(ind),probs=probs),grp]
      ds[,percentile:=rep(probs.n,.N/length(probs))]
      setnames(ds,  "V1","value")
    }
  }

vpc.data <- 
  function( type = c("percentile","scatter"),
            dobs,
            dsim,
            probs.pi,
            probs.ci,
            idv = "time",
            irun="stu",
            dv="y"){
    rug <- data.frame(
      x=unlist(unique(dobs[,idv,with=FALSE])), 
      y=NA_real_,
      stringsAsFactors = FALSE)
    if (type == "percentile"){ 
      pi <- quantile_dt(dobs,probs = probs.pi,grp = idv,ind = dv)
      res2 <- quantile_dt(dsim,probs = probs.pi,grp =c(irun,idv),ind=dv)
      ci <- quantile_dt(res2,probs = probs.ci,grp=c("percentile",idv),
                        prefix="CL",ind="value",wide=TRUE)
      
    }else{
      pi <- quantile_dt(dsim,probs = probs.pi,grp =c(idv),ind=dv)
    }
    
    res <- list(pi_dt = pi, rug_dt = rug)
    if (type == "percentile") res$ci_dt <- ci
    res
  }






vpc.plot <- function(x){
  
  with(x,{
    pp <- ggplot(data = db$pi_dt,aes_string(x=idv)) + 
      geom_line(aes_string(group="percentile",y="value")) +
      geom_point(data=input,aes_string(y=dv),alpha=.2) +
      geom_rug(data=db$rug_dt, sides = "t", aes(x = x, y=y), colour="red") +
      theme_bw()
    
    if (type=="percentile"){
      pp + geom_ribbon(data=db$ci_dt,
                       aes(ymin=CL05,ymax=CL95,group=percentile),fill='blue',alpha=0.2) +
        labs(title='Percentile VPC',subtitle = '(with observations)') 
    }else pp + labs(title='Scatter VPC')
  })
}

#' Creates vpc object 
#'
#' @param type \code{charcater} can be either perecentile or scatter
#' @param idv \code{chracater} individual variable
#' @param obs \code{vpc_obs} object observation layer \link{pmx_obs}
#' @param pi \code{vpc_pi} object percentile layer  \link{pmx_pi}
#' @param ci \code{vpc_ci} object confidence interval layer  \link{pmx_ci}
#' @param rug  \code{vpc_rub} object rug layer  \link{pmx_rug} 
#' @param bin \code{vpc_bin} object  \link{pmx_bin} 
#' @param labels \code{list} define title and axis labels
#' @param is.legend \code{logical} if TRUE add legend
#' @param dname added for compatibility with other ggPMX plots
#' @param strat \code{chracter} use stratification 
#'
#' @family vpc
#' @export
#'

vpc <- function(
  type = c("percentile","scatter"),
  idv  ="TIME", 
  dv = "y",
  strat=NULL,
  obs  = pmx_obs(),
  pi =  pmx_pi(),
  ci =  pmx_ci(),
  rug = pmx_rug(),
  bin = pmx_bin(),
  labels = NULL,
  is.legend=FALSE,
  dname=NULL,
  ...
){
  type = match.arg(type)
  ## check args here  
  
  structure(
    list(
      ptype = "VPC",
      strat = TRUE,
      idv = idv,
      dv = dv,
      dname = dname,
      labels = labels,
      is.legend = is.legend,
      type=type,
      obs = obs,pi = pi,ci = ci,rug=rug,bin=bin,
      gp = pmx_gpar(labels = labels, ...)
    ),
    class = c("vpc", "pmx_gpar")
  )
} 


plot_pmx.vpc <- function(x, dx, ...) {
  db <- x$db
  if (!is.null(db))  vpc.plot(x)
}