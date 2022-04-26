test_that("hotelling t-test works", {
  expect_equal(one.sample.hotelling(iris[,1:4], c(mean(iris[,1]),mean(iris[,2]),mean(iris[,3]),mean(iris[,4]))), "F= 0 P-Value= 1")
})
