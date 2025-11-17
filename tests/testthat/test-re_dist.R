test_that("re_dist matches stats::dist for euclidean", {
  method <- "euclidean"
  set.seed(1)

  x <- matrix(runif(n=500, min=1, max=20), nrow=100)
  dist_base <- stats::dist(x, method)
  dist_reDist <- re_dist(x, method)

  expect_s3_class(dist_reDist, "dist")
  expect_equal(as.matrix(dist_reDist), as.matrix(dist_base))
})

test_that("re_dist matches stats::dist for maximum", {
  method <- "maximum"
  set.seed(2)

  x <- matrix(runif(n=500, min=1, max=20), nrow=100)
  dist_base <- stats::dist(x, method)
  dist_reDist <- re_dist(x, method)

  expect_s3_class(dist_reDist, "dist")
  expect_equal(as.matrix(dist_reDist), as.matrix(dist_base))
})

test_that("re_dist matches stats::dist for manhattan", {
  method <- "manhattan"
  set.seed(3)

  x <- matrix(runif(n=500, min=1, max=20), nrow=100)
  dist_base <- stats::dist(x, method)
  dist_reDist <- re_dist(x, method)

  expect_s3_class(dist_reDist, "dist")
  expect_equal(as.matrix(dist_reDist), as.matrix(dist_base))
})

test_that("re_dist matches stats::dist for canberra", {
  method <- "canberra"
  set.seed(4)

  x <- matrix(runif(n=500, min=1, max=20), nrow=100)
  dist_base <- stats::dist(x, method)
  dist_reDist <- re_dist(x, method)

  expect_s3_class(dist_reDist, "dist")
  expect_equal(as.matrix(dist_reDist), as.matrix(dist_base))
})

test_that("re_dist matches stats::dist for binary", {
  method <- "binary"
  set.seed(5)

  x <- matrix(runif(n=500, min=1, max=20), nrow=100)
  dist_base <- stats::dist(x, method)
  dist_reDist <- re_dist(x, method)

  expect_s3_class(dist_reDist, "dist")
  expect_equal(as.matrix(dist_reDist), as.matrix(dist_base))
})

test_that("re_dist matches stats::dist for minkowski", {
  method <- "minkowski"
  set.seed(6)

  x <- matrix(runif(n=500, min=1, max=20), nrow=100)
  dist_base <- stats::dist(x, method)
  dist_reDist <- re_dist(x, method)

  expect_s3_class(dist_reDist, "dist")
  expect_equal(as.matrix(dist_reDist), as.matrix(dist_base))
})

test_that("re_dist matches stats::dist for minkowski with custom p", {
  method <- "minkowski"
  p <- 3
  set.seed(7)

  x <- matrix(runif(n=500, min=1, max=20), nrow=100)
  dist_base <- stats::dist(x, method, p = p)
  dist_reDist <- re_dist(x, method, p)

  expect_s3_class(dist_reDist, "dist")
  expect_equal(as.matrix(dist_reDist), as.matrix(dist_base))
})

test_that("re_dist errors on invalid distance method", {
  set.seed(8)
  x <- matrix(runif(n=500, min=1, max=20), nrow=100)
  expect_error(re_dist(x, method = "Euclidean"), "invalid distance method", ignore.case = TRUE)
  expect_error(re_dist(x, method = "Very real distance metric"), "invalid distance method", ignore.case = TRUE)
})


test_that("re_dist errors on non-numeric input", {
  x <- data.frame(a = c("a", "b"))
  expect_error(re_dist(x), "numeric", ignore.case = TRUE)
})
