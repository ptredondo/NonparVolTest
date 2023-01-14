#' NonparVolTest: A package for simulating observations, estimating parameters and nonparametric testing for clustered volatility.
#'
#'
#' The `NonparVolTest` package provides three important functions, namely,
#' `clus_vol_gendata()`, `nonpar_vol_estimate()` and `nonpar_vol_test()`.
#'
#' @section `NonparVolTest` functions:
#' The `clus_vol_gendata()` function generates observations from the multiple time series with clustered volatility (ARCH-type).
#' The `nonpar_vol_estimate()` function estimates the parameters of the multiple time series with clustered volatility (ARCH-type) model
#' through the backfitting algorithm.
#' The `nonpar_vol_test()` function performs the nonparametric test for clustered volatility in multiple time series model
#' based on bootstrap resampling scheme.
#'
#' @docType package
#' @name NonparVolTest
#'
#' @import lme4 dplyr assertthat
NULL
