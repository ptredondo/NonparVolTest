test_that("verify if list contains the correct results", {
  Mydata <- clus_vol_gendata(Nlength = 100,clusizes = c(5,5),phi = 0.6,rand.mean = rep(0,10),
                             rand.sd = rep(1,10),vol.par0 = c(1,1),vol.par1 = c(0,0.4),
                             nburn = 500,seed = 1)
  Myresult <- nonpar_vol_test(Mydata,B = 30,max.iter=100,cut=0.00001)
  expect_equal(length(names(Myresult)), 5)
})
