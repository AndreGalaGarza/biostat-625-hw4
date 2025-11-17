#' Reimplementation of dist()
#'
#' @description
#' This package reimplements the base R function dist() as accurately as possible.
#'
#' @param x An object coercible to a numeric matrix, such as a matrix, data frame, or \code{dist} object. Any non-numerical columns in x will cause an error.
#'
#' @param method The distance measure to use for calculating the distance between rows. This must be one of the following: \code{euclidean},  \code{maximum},  \code{manhattan},  \code{canberra},  \code{binary}, or  \code{minkowski}.
#'
#' @param p The power of the Minkowski distance. This argument is only relevant if the chosen \code{method} = \code{minkowski}; otherwise, it is ignored.
#'
#' @return
#' A \code{dist} object in R, which is a special single-dimensional vector object that resembles a lower triangular matrix.
#'
#' @seealso
#' [stats::dist()]
#' <https://stat.ethz.ch/R-manual/R-patched/library/stats/html/dist.html>
#'
#' @examples
#' re_dist(mat, method = "euclidean")
#' re_dist(mat, method = "maximum")
#' re_dist(mat, method = "minkowski", p = 3)
#' @export
re_dist <- function(x, method, p = 2) {
  # Validate distance method
  if (!(method %in% c("euclidean", "maximum", "manhattan",
                    "canberra", "binary", "minkowski"))) {
    stop("invalid distance method")
  }

  # Assign input matrix
  x <- as.matrix(x)
  if (!is.numeric(x)) {
    stop("x was not coerced to numeric")
  }
  row_names <- rownames(x)

  # Transpose for column-major operations
  x <- t(x)

  # Calculate distance
  calc_dist <- function(x, method, p) {
    n <- nrow(x)
    dist_matrix <- matrix(0, nrow = n, ncol = n)

    # Compare all rows to each other
    for (i in 1:(n - 1)) {
      min_row <- i + 1
      for (j in min_row:n) {
        # Calculate distance between two rows
        a <- x[, i]
        b <- x[, j]
        if (method == "euclidean" || method == "minkowski") {
          dist_row <- abs(a - b)
          dist_row <- dist_row^p
          dist_row <- sum(dist_row)
          dist_row <- dist_row^(1/p)
        } else if (method == "maximum") {
          dist_row <- abs(a - b)
          dist_row <- max(dist_row)
        } else if (method == "manhattan") {
          dist_row <- abs(a - b)
          dist_row <- sum(dist_row)
        } else if (method == "canberra") {
          dist_row <- abs(a - b) / (abs(a) + abs(b))
          dist_row <- sum(dist_row)
        } else { # Binary (Jaccard) distance
          a1 <- which(a != 0)
          b1 <- which(b != 0)
          inter <- length(intersect(a1, b1))
          uni   <- length(union(a1, b1))
          dist_row <- 1 - inter / uni
        }

        # Assign values to dist_matrix
        # so that it is square and symmetric
        dist_matrix[i, j] <- dist_row
        dist_matrix[j, i] <- dist_row
      }
    }
    dist_matrix
  }

  dist_matrix <- calc_dist(x, method, p)
  rownames(dist_matrix) <- row_names
  as.dist(dist_matrix)
}
