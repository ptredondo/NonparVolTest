#' Estimating the multiple time series with clustered volatility (ARCH-type) model through the backfitting algorithm
#'
#' @param data A dataframe that includes a column of cluster IDs, series IDs, observations `Y(t)`, and lagged values `Y(t-1)`.
#' Variables required are `clus`, `sid`, `y`, and `y1`.
#' @param max.iter A positive integer specifying the number of maximum allowable iteration before force stopping the algorithm.
#' @param cut A numeric scalar (close to zero) specifying the convergence criterion cut-off (absolute percent change).
#'
#' @return A list containing `num_iter`, `ar`, `reff`, and `vol_par`. These objects are required for the `nonpar_vol_test` function.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' Mydata <- clus_vol_gendata(Nlength = 100,clusizes = c(5,5),phi = 0.6,rand.mean = rep(0,10),
#'                            rand.sd = rep(1,10),vol.par0 = c(1,1),vol.par1 = c(0,0.4),
#'                            nburn = 500,seed = 1)
#' nonpar_vol_estimate(Mydata,max.iter=100,cut=0.00001)
#'

nonpar_vol_estimate <- function(data,max.iter=100,cut=0.00001){

  #Obtaining unique series IDs for iteration
  series.id <- unique(data$sid)

  #Obtaining number of clusters
  nclus <- length(unique(data$clus))

  #Setting initial set-up for convergence
  convergence <- FALSE
  m <- 0

  #Estimation proper
  while(!convergence  && m < max.iter){
    m <- m + 1
    if(m == 1){

      #Estimating random effects per time series
      rand_effects <- lme4::lmer(y~0+(1|sid),data=data)

      #Saving estimated random effects
      reff_p <- as.numeric(unlist(stats::coef(rand_effects)))

      #Defining residual R1
      data$reff <- as.numeric(stats::fitted(rand_effects))
      data$resid1 <- data$y - data$reff

      #Estimating an AR(1) model for each time series
      ar_comp <- NULL
      int_comp <- NULL
      for(n in 1:length(series.id)){
        sub <- subset(data,sid==series.id[n])
        if(assertthat::is.error(try(stats::arima(sub$resid1,order=c(1,0,0)),silent=TRUE))){next}
        ar_est <- as.numeric(stats::coef(stats::arima(sub$resid1,order=c(1,0,0))))
        ar_comp <- c(ar_comp,ar_est[1])
        int_comp <- c(int_comp,ar_est[2])
      }

      #Obtaining bootstrap resample means
      ar_bs_comp <- NULL
      int_bs_comp <- NULL
      for(b in 1:200){
        set.seed(b)
        samp_ind <- sample(length(ar_comp),length(ar_comp),replace=TRUE)
        ar_samp <- ar_comp[samp_ind]
        int_samp <- int_comp[samp_ind]
        ar_bs_comp <- c(ar_bs_comp,mean(ar_samp,na.rm=TRUE))
        int_bs_comp <- c(int_bs_comp,mean(int_samp,na.rm=TRUE))
      }

      #Saving the estimated bootstrap mean
      ar_p <- mean(ar_bs_comp,na.rm=TRUE)
      int_p <- mean(int_bs_comp,na.rm=TRUE)

      #Defining new residual R2 and an estimate for the random shocks (uhat)
      data$resid2 <- data$y - ar_p*data$y1
      data$uhat <- data$y - data$reff - ar_p*data$y1 - int_p

      #Defining estimates of the variance of the random shock at time point t
      data$shat <- abs(data$uhat)
      data$shat_sq <- data$shat^2
      data$uhat_sq <- data$uhat^2

      #Obtaining lagged values of sigma2hat and squared uhat
      vol_data <- NULL
      for(n in 1:length(series.id)){
        sub <- subset(data,sid==series.id[n])
        sub$s1hat_sq <- c(NA,sub$shat_sq[c(-length(sub$shat_sq))])
        sub$u1hat_sq <- c(NA,sub$uhat_sq[c(-length(sub$uhat_sq))])
        vol_data <- rbind(vol_data,sub)
      }
      data <- vol_data

      #Estimating the variance model
      vol_par_comp <- NULL

      for(n in 1:nclus){
        sub <- subset(data,clus==n)
        if(assertthat::is.error(try(stats::lm(shat_sq ~ u1hat_sq,data=sub),silent=TRUE))){next}
        vol_fit <- stats::lm(shat_sq ~ u1hat_sq,data=sub)
        vol_par_est <- data.frame(clus=n,int=as.numeric(stats::coef(vol_fit))[1],slope=as.numeric(stats::coef(vol_fit))[2])
        vol_par_comp <- rbind(vol_par_comp,vol_par_est)
      }

      vol_par_p <- vol_par_comp


    } else{

      #Estimating random effects per time series
      rand_effects <- lme4::lmer(resid2 ~ 0 + (1|sid), data = data)

      #Saving predicted random effects
      reff_n <- as.numeric(unlist(stats::coef(rand_effects)))

      #Defining residual R1
      data$reff <- as.numeric(stats::fitted(rand_effects))
      data$resid1 <- data$y - data$reff

      #Estimating an AR(1) model for each time series
      ar_comp <- NULL
      int_comp <- NULL
      for(n in 1:length(series.id)){
        sub <- subset(data,sid==series.id[n])
        if(assertthat::is.error(try(stats::arima(sub$resid1,order=c(1,0,0)),silent=TRUE))){next}
        ar_est <- as.numeric(stats::coef(stats::arima(sub$resid1,order=c(1,0,0))))
        ar_comp <- c(ar_comp,ar_est[1])
        int_comp <- c(int_comp,ar_est[2])
      }

      #Obtaining bootstrap resample means
      ar_bs_comp <- NULL
      int_bs_comp <- NULL
      for(b in 1:200){
        set.seed(b)
        samp_ind <- sample(length(ar_comp),length(ar_comp),replace=TRUE)
        ar_samp <- ar_comp[samp_ind]
        int_samp <- int_comp[samp_ind]
        ar_bs_comp <- c(ar_bs_comp,mean(ar_samp,na.rm=TRUE))
        int_bs_comp <- c(int_bs_comp,mean(int_samp,na.rm=TRUE))
      }

      #Saving the estimated bootstrap mean
      ar_n <- mean(ar_bs_comp,na.rm=TRUE)
      int_n <- mean(int_bs_comp,na.rm=TRUE)

      #Defining new residual R2 and an estimate for the random shocks (uhat)
      data$resid2 <- data$y - ar_n*data$y1
      data$uhat <- data$y - data$reff - ar_n*data$y1 - int_n

      #Defining estimates of the variance of the random shock at time point t
      data$shat <- abs(data$uhat)
      data$shat_sq <- data$shat^2
      data$uhat_sq <- data$uhat^2

      #Obtaining lagged values of sigma2hat and squared uhat
      vol_data <- NULL
      for(n in 1:length(series.id)){
        sub <- subset(data,sid==series.id[n])
        sub$s1hat_sq <- c(NA,sub$shat_sq[c(-length(sub$shat_sq))])
        sub$u1hat_sq <- c(NA,sub$uhat_sq[c(-length(sub$uhat_sq))])
        vol_data <- rbind(vol_data,sub)
      }
      data <- vol_data

      #Estimating the variance model
      vol_par_comp <- NULL

      for(n in 1:nclus){
        sub <- subset(data,clus==n)
        if(assertthat::is.error(try(stats::lm(shat_sq ~ u1hat_sq,data=sub),silent=TRUE))){next}
        vol_fit <- stats::lm(shat_sq ~ u1hat_sq,data=sub)
        vol_par_est <- data.frame(clus=n,int=as.numeric(stats::coef(vol_fit))[1],slope=as.numeric(stats::coef(vol_fit))[2])
        vol_par_comp <- rbind(vol_par_comp,vol_par_est)
      }

      vol_par_n <- vol_par_comp
      names(vol_par_n) <- c("clus","int_n","slope_n")

      #Checking for convergence of the estimation procedure
      check_p <- abs((ar_n - ar_p)/ar_p)
      #When at least one random effect mean is estimated to be zero, set convergence for the random effects to be TRUE
      if(!0 %in% reff_p){
        check_reff <- abs((reff_n - reff_p)/reff_p)
      }else{
        check_reff <- 0
      }


      check_vol_par <- dplyr::left_join(vol_par_p,vol_par_n,by="clus")
      check_vol_par$check_int <- ifelse(abs((check_vol_par$int_n - check_vol_par$int)/check_vol_par$int)<cut,0,1)
      check_vol_par$check_slope <- ifelse(abs((check_vol_par$slope_n - check_vol_par$slope)/check_vol_par$slope)<cut,0,1)
      if(check_p < cut && as.logical(cumprod(check_reff < cut)[length(check_reff)]) && sum(check_vol_par$check_int) == 0 && sum(check_vol_par$check_slope,na.rm = TRUE) == 0){
        convergence = TRUE
      }

      #Setting each estimate as previous estimates for the next iteration
      ar_p <- ar_n
      reff_p <- reff_n
      vol_par_p <- vol_par_n
      names(vol_par_p) <- c("clus","int","slope")

    }
  }
  return(list(num_iter=m,ar=ar_n,reff=reff_n,vol_par=vol_par_n))
}
