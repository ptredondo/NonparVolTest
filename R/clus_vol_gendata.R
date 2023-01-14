#' Generating observations from the multiple time series with clustered volatility (ARCH-type)
#'
#' @param Nlength A numeric scalar specifying the length of each time series.
#' @param clusizes A vector of length `k` specifying the number of time series in each of the `k` clusters.
#' @param phi A numeric scalar in (-1,1) specifying the autoregressive parameter shared by all time series.
#' @param rand.mean A vector of length `N` specifying the random effects means for all `N` time series.
#' @param rand.sd A vector of length `N` specifying the random effects standard deviations for all `N` time series.
#' @param vol.par0 A vector of length `k` specifying the ARCH volatility parameters (intercept) of each cluster.
#' @param vol.par1 A vector of length `k` specifying the ARCH volatility parameters (slope) of each cluster.
#' @param nburn A numeric scalar specifying the number of timepoints to discard from simulation.
#' @param seed A numeric scalar specifying a seed number for reproducibility.
#'
#' @return A dataframe that includes all simulated observations `Y(t)`, `Y(t-1)`, cluster and series IDs.
#' Variables included are `clus`, `sid`, `t`, `y`, and `y1`.
#' @export
#'
#' @examples
#' set.seed(1)
#' Mydata <- clus_vol_gendata(Nlength = 100,clusizes = c(5,5),phi = 0.6,rand.mean = rep(0,10),
#'                            rand.sd = rep(1,10),vol.par0 = c(1,1),vol.par1 = c(0,0),
#'                            nburn = 500,seed = 1)
#'

clus_vol_gendata <- function(Nlength,clusizes,phi,rand.mean,rand.sd,vol.par0,vol.par1,nburn=500,seed=1){

  #Setting the data frame for compilation to NULL
  finaldata <- NULL

  #Determining the number of clusters
  nclus <- length(clusizes)

  #Setting seed number for reproducibility
  set.seed(seed)

  #Iterating per clusters
  for(k in 1:nclus){

    #Iterating per series in the kth cluster
    for(j in 1:clusizes[k]){

      #Setting the data frame for each time series to NULL
      series <- NULL

      #Initial values for the series
      y1 <- s1 <- u1 <- 0

      #Simulating random effects per series
      randeff <- stats::rnorm(1,rand.mean[sum(clusizes[1:k])-clusizes[k]+j],rand.sd[sum(clusizes[1:k])-clusizes[k]+j])

      #Iterating to obtain data per time point in each series
      for(i in (-nburn):Nlength){

        #Simulating the data generating process of an AR(1)-ARCH(1)/GARCH(1,1) model with random effects
        vol <- vol.par0[k]+vol.par1[k]*u1^2
        s <- sqrt(vol)
        u <- stats::rnorm(1,0,s)
        y <- phi*y1 + randeff + u
        tpoint <- data.frame(clus=k,sid=paste(k,j,sep="_"),t=i,y=y,y1=y1)
        series <- rbind(series,tpoint)
        y1 <- y
        s1 <- s
        u1 <- u
      }

      #Obtaining data after the Markov Chain stabilizes
      series <- subset(series,t>0)

      #Compiling the series to a single data frame
      finaldata <- rbind(finaldata,series)
    }
  }
  return(finaldata)
}
