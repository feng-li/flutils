test_that("diagonal matrix shortcuts match dense multiplication", {
  M <- matrix(1:6, nrow = 2)
  d_row <- c(2, 3)
  d_col <- c(2, 3, 4)

  expect_equal(d_row %d*% M, diag(d_row) %*% M)
  expect_equal(M %*d% d_col, M %*% diag(d_col))

  S <- matrix(1:4, nrow = 2)
  expect_equal(d_row %d*d% S, diag(d_row) %*% S %*% diag(d_row))
  expect_equal(diag1(4), diag(4))
})

test_that("commutation matrix helpers match explicit matrix products", {
  left <- matrix(1:12, nrow = 6)
  right <- matrix(1:12, nrow = 2)

  expect_equal(K.X(2, 3, left, FALSE), K(2, 3) %*% left)
  expect_equal(K.X(2, 3, right, TRUE), right %*% K(2, 3))
})

test_that("summary helpers match base apply results", {
  x <- matrix(1:12, nrow = 3)

  expect_equal(colSds(x), apply(x, 2, stats::sd))
  expect_equal(rowSds(x), apply(x, 1, stats::sd))
  expect_equal(colVars(x), apply(x, 2, stats::var))
  expect_equal(rowVars(x), apply(x, 1, stats::var))
})

test_that("array2list slices arrays along requested margins", {
  x <- array(1:24, dim = c(2, 3, 4))
  out <- array2list(x, 3)

  expect_length(out, 4)
  expect_equal(out[[2]], x[, , 2])
})
