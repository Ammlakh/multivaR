test_that("covariance matrix significance test works", {
  x <- matrix(c(0.6856935, -0.0424340, -0.0424340, 0.1899794), ncol = 2)
  expect_equal(covmat.test(cov(iris[,1:2]), x, 150, 3)$p.value.u, 1)
})
