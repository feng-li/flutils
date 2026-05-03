test_that("alternative beta parametrization matches base beta functions", {
  mean <- 0.4
  sd <- 0.1
  shape1 <- -mean * (mean^2 - mean + sd^2) / sd^2
  shape2 <- -1 + mean + (mean - 1)^2 * mean / sd^2

  expect_equal(dbeta2(0.5, mean, sd), stats::dbeta(0.5, shape1, shape2))
  expect_equal(qbeta2(c(0.25, 0.75), mean, sd),
               stats::qbeta(c(0.25, 0.75), shape1, shape2))

  set.seed(1)
  expect_length(rbeta2(8, mean, sd), 8)
})

test_that("normal mixture density matches weighted component densities", {
  means <- matrix(c(-1, 2), nrow = 1)
  sigmas <- array(c(1, 4), dim = c(1, 1, 2))
  weights <- c(0.25, 0.75)
  x <- 0.5

  expected <- sum(weights * c(
    mvtnorm::dmvnorm(matrix(x), mean = means[, 1], sigma = matrix(sigmas[, , 1])),
    mvtnorm::dmvnorm(matrix(x), mean = means[, 2], sigma = matrix(sigmas[, , 2]))
  ))

  expect_equal(dmixnorm(x, means, sigmas, weights), expected)
  expect_equal(dmixnorm(x, means, sigmas, weights, log = TRUE), log(expected))
})

test_that("time-series normal mixture density returns vector densities", {
  y <- 1:5
  means.ar.par.list <- list(c(0, 0.5))
  sigmas.list <- list(rep(1, length(y)))
  weights <- 1

  log_density <- dmixnorm.ts(y, means.ar.par.list, sigmas.list, weights, log = TRUE)
  density <- dmixnorm.ts(y, means.ar.par.list, sigmas.list, weights, log = FALSE)

  expect_length(density, length(y))
  expect_equal(density, exp(log_density))
})
