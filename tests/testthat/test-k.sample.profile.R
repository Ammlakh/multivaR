test_that("profile tests work" , {
  data <- cbind(iris[,5], iris[,1:4])
  expect_type(k.sample.profile(data, K = 3), "list")
})
