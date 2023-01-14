test_that("verify if generated data has the required variables", {
  Mydata <- clus_vol_gendata(Nlength = 100,clusizes = c(5,5),phi = 0.6,rand.mean = rep(0,10),
                             rand.sd = rep(1,10),vol.par0 = c(1,1),vol.par1 = c(0,0),
                             nburn = 500,seed = 1)
  expect_equal(names(Mydata) %in% c("clus","sid","t","y","y1"), rep(TRUE,length(names(Mydata))))
})
