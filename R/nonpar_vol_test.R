#' Nonparametric test for clustered volatility in multiple time series model based on bootstrap resampling scheme
#'
#' @param data A dataframe that includes a column of cluster IDs, series IDs, observations `Y(t)`, and lagged values `Y(t-1)`.
#' Variables required are `clus`, `sid`, `y`, and `y1`.
#' @param alpha A numeric scalar in (0,1) specifying the level of significance for the bootstrap confidence interval.
#' @param B A positive integer specifying the number of bootstrap replicates for computing the confidence intervals.
#' @param max.iter A positive integer specifying the number of maximum allowable iteration before force stopping the algorithm.
#' @param cut A numeric scalar (close to zero) specifying the convergence criterion cut-off (absolute percent change).
#' @param seed A numeric scalar specifying a seed number for reproducibility.
#'
#' @return A list containing `Number of Iterations`, `Estimated AR coefficient`, `Predicted Series Random Effects`,
#' `Estimated ARCH(1) Slope Parameters` and `Bootstrap CI for Slope Parameters`.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' Mydata <- clus_vol_gendata(Nlength = 100,clusizes = c(5,5),phi = 0.6,rand.mean = rep(0,10),
#'                            rand.sd = rep(1,10),vol.par0 = c(1,1),vol.par1 = c(0,0),
#'                            nburn = 500,seed = 1)
#' nonpar_vol_test(Mydata,B=100,max.iter=100,cut=0.00001)
#'
#' set.seed(2)
#' Mydata <- clus_vol_gendata(Nlength = 100,clusizes = c(5,5),phi = 0.6,rand.mean = rep(0,10),
#'                            rand.sd = rep(1,10),vol.par0 = c(1,1),vol.par1 = c(0,0.4),
#'                            nburn = 500,seed = 1)
#' nonpar_vol_test(Mydata,B=100,max.iter=100,cut=0.00001)
#'
#' set.seed(3)
#' Mydata <- clus_vol_gendata(Nlength = 100,clusizes = c(5,5),phi = 0.6,rand.mean = rep(0,10),
#'                            rand.sd = rep(1,10),vol.par0 = c(1,1),vol.par1 = c(0.4,0.4),
#'                            nburn = 500,seed = 1)
#' nonpar_vol_test(Mydata,B=100,max.iter=100,cut=0.00001)
#'

nonpar_vol_test <- function(data,alpha=0.05,B=100,max.iter=100,cut=0.00001,seed=1){

  #Estimating the model from original data
  mainresult <- nonpar_vol_estimate(data,max.iter,cut)

  #Obtaining unique series IDs for iteration
  series.id <- unique(data$sid)

  #Obtaining number of clusters
  nclus <- length(unique(data$clus))

  #Obtaining number of series per cluster
  clusizes <- stats::aggregate(sid ~ clus,data[c("clus","sid")],FUN = dplyr::n_distinct)$sid

  #Obtaining number of timepoints per series
  Nlength <- nrow(data)/length(series.id)

  #Setting volatility parameters for resampling
  vol_par0 <- mainresult$vol_par$int_n
  vol_par0 <- ifelse(vol_par0 >= 0,vol_par0,0)
  vol_par1 <- mainresult$vol_par$slope_n
  vol_par1 <- ifelse(vol_par1 <= 0,0,ifelse(vol_par1>=1,0.99,vol_par1))

  #Setting seed number
  set.seed(seed)

  #Resampling
  vol_par_boot <- array(NA,dim=c(nclus,B,2))
  for(b in 1:B){
    bootdata <- clus_vol_gendata(Nlength,clusizes,mainresult$ar,mainresult$reff,rep(stats::sd(mainresult$reff),sum(clusizes)),
                                 vol_par0,vol_par1,b)
    bootresult <- nonpar_vol_estimate(bootdata,max.iter,cut)
    vol_par_boot[,b,1] <- bootresult$vol_par$int_n
    vol_par_boot[,b,2] <- bootresult$vol_par$slope_n
  }

  boot.upCI <- apply(vol_par_boot,MARGIN = c(3,1),FUN = max)
  boot.lowCI <- apply(vol_par_boot,MARGIN = c(3,1),FUN = stats::quantile,probs = alpha/nclus)

  vol_conf <- data.frame(Cluster=1:nclus,
                         "Slope Estimate"=mainresult$vol_par$slope_n,
                         "Adjusted CI Lower" = boot.lowCI[2,],"Adjusted CI Upper" = boot.upCI[2,])

  mainresult$vol_conf <- vol_conf

  names(mainresult$vol_par) <- c("Cluster","Intercept","Slope")
  names(mainresult) <- c("Number of Iterations","Estimated AR coefficient","Predicted Series Random Effects",
                         "Estimated ARCH(1) Slope Parameters","Bootstrap CI for Slope Parameters")
  return(mainresult)

}
