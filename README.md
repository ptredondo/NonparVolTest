
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NonparVolTest

<!-- badges: start -->
<!-- badges: end -->

`NonparVolTest` provides provides functions to perform the following:
(i) generate observations from the multiple time series with clustered
volatility (ARCH-type), (ii) estimate the parameters of the multiple
time series with clustered volatility (ARCH-type) model through the
backfitting algorithm, and (iii) compute the bootstrap confidence
interval for the slope parameters of the clustered volatility
(ARCH-type) model.

## Installation

You can install the most recent version of `NonparVolTest` in the
following way

``` r
library(devtools)
devtools::install_github("ptredondo/NonparVolTest", build_vignettes = TRUE)
```

## Example

As a simple example, suppose we want to simulate from the multiple time
series with clustered volatility model and perform the nonparametric
test for clustered volatility based on a bootstrap resampling scheme.
Here, we generate data with two clusters of time series from three
different scenarios: (i) no ARCH-type volatility, (ii) only one cluster
has ARCH-type volatility, and (iii) both clusters have ARCH-type
volatility.

``` r
library(NonparVolTest)

set.seed(1)
Mydata <- clus_vol_gendata(Nlength = 100,clusizes = c(5,5),phi = 0.6,rand.mean = rep(0,10),rand.sd = rep(1,10),
                           vol.par0 = c(1,1),vol.par1 = c(0,0),nburn = 500,seed = 1)
nonpar_vol_test(Mydata,B = 100,max.iter=100,cut=0.00001)
#> $`Number of Iterations`
#> [1] 3
#> 
#> $`Estimated AR coefficient`
#> [1] 0.5542969
#> 
#> $`Predicted Series Random Effects`
#>  [1] -0.7246840  0.3744794  0.7196013 -1.1297625  0.8950063  0.5618643
#>  [7]  0.5071534  0.7365715  0.6285816 -1.8214840
#> 
#> $`Estimated ARCH(1) Slope Parameters`
#>   Cluster Intercept        Slope
#> 1       1  1.156419 -0.019085717
#> 2       2  1.011227 -0.003209263
#> 
#> $`Bootstrap CI for Slope Parameters`
#>   Cluster Slope.Estimate Adjusted.CI.Lower Adjusted.CI.Upper
#> 1       1   -0.019085717       -0.07441480       0.009211671
#> 2       2   -0.003209263       -0.05248635       0.080123766

set.seed(2)
Mydata <- clus_vol_gendata(Nlength = 100,clusizes = c(5,5),phi = 0.6,rand.mean = rep(0,10),rand.sd = rep(1,10),
                          vol.par0 = c(1,1),vol.par1 = c(0,0.4),nburn = 500,seed = 1)
nonpar_vol_test(Mydata,B = 100,max.iter=100,cut=0.00001)
#> $`Number of Iterations`
#> [1] 3
#> 
#> $`Estimated AR coefficient`
#> [1] 0.5589581
#> 
#> $`Predicted Series Random Effects`
#>  [1] -0.7136386  0.3685026  0.7091443 -1.1126425  0.8817109  0.5839291
#>  [7]  0.4338834  0.7581394  0.5839062 -1.7678312
#> 
#> $`Estimated ARCH(1) Slope Parameters`
#>   Cluster Intercept       Slope
#> 1       1  1.156530 -0.01902007
#> 2       2  1.126812  0.30786436
#> 
#> $`Bootstrap CI for Slope Parameters`
#>   Cluster Slope.Estimate Adjusted.CI.Lower Adjusted.CI.Upper
#> 1       1    -0.01902007       -0.07449386        0.01265522
#> 2       2     0.30786436        0.14103822        0.34693313

set.seed(3)
Mydata <- clus_vol_gendata(Nlength = 100,clusizes = c(5,5),phi = 0.6,rand.mean = rep(0,10),rand.sd = rep(1,10),
                           vol.par0 = c(1,1),vol.par1 = c(0.4,0.4),nburn = 500,seed = 1)
nonpar_vol_test(Mydata,B = 100,max.iter=100,cut=0.00001)
#> $`Number of Iterations`
#> [1] 3
#> 
#> $`Estimated AR coefficient`
#> [1] 0.5538423
#> 
#> $`Predicted Series Random Effects`
#>  [1] -0.7911626  0.2788461  0.6978069 -1.1003293  0.8905095  0.5883400
#>  [7]  0.4368228  0.7635474  0.5880641 -1.7808141
#> 
#> $`Estimated ARCH(1) Slope Parameters`
#>   Cluster Intercept     Slope
#> 1       1  1.224327 0.3669280
#> 2       2  1.118172 0.3113498
#> 
#> $`Bootstrap CI for Slope Parameters`
#>   Cluster Slope.Estimate Adjusted.CI.Lower Adjusted.CI.Upper
#> 1       1      0.3669280         0.1215138         0.4148412
#> 2       2      0.3113498         0.1385184         0.3552675
```
