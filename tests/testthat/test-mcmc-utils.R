test_that("data.partition creates ordered folds with complete coverage", {
  out <- data.partition(
    10,
    list(partiMethod = "ordered", N.subsets = 3, testRatio = NULL)
  )

  expect_equal(out, list(1:4, 5:7, 8:10))
  expect_equal(sort(unlist(out)), 1:10)
})

test_that("set.crossvalid returns full sample when disabled", {
  out <- set.crossvalid(5, list(N.subsets = 0))

  expect_equal(out$training, list(1:5))
  expect_equal(out$testing, list(1:5))
})

test_that("parameter link functions invert implemented links", {
  mu <- matrix(c(0.25, 0.5, 0.75), ncol = 1)
  linkArgs <- list(type = "logit", a = 0, b = 1)
  eta <- parLinkFun(mu, linkArgs)

  expect_equal(parMeanFun(diag(3), eta, linkArgs), mu)
})
