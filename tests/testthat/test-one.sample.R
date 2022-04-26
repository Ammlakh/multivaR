test_that("hotelling t-test works", {
  library(datasets)
  library(stat431package)
  one.sample.hotelling(iris[,1:4], c(5,3,4,1))
})
