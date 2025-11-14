re_dist <- function(x, method, diag = FALSE, upper = FALSE, p = 2) {
  # Validate distance method
  if (!(method %in% c("euclidean", "maximum", "manhattan",
                    "canberra", "binary", "minkowski"))) {
    stop("invalid distance_method")
  }
  
  # Assign input matrix
  x <- as.matrix(x)
  if (!is.numeric(x)) {
    stop("x was not coerced to numeric")
  }
  
  n <- nrow(x)
  row_names <- rownames(x)
  
  # Transpose for column-major operations
  x <- t(x)
  
  # Calculate distance
  calc_dist <- function(x, method) {
    dist_matrix <- matrix(0, nrow = n, ncol = n)
    
    # Compare all rows to each other
    for (i in 1:(n - 1)) {
      min_row <- i + 1
      for (j in min_row:n) {
        # Calculate distance between two rows
        a <- x[, i]
        b <- x[, j]
        dist_row <- vector(mode = "numeric", length = length(a))
        if (method == "euclidean" || method == "minkowski") {
          if (method == "euclidean") {
            p <- 2
          }
          dist_row <- abs(a - b)
          dist_row <- dist_row^p
          dist_row <- sum(dist_row)
          dist_row <- dist_row^(1/p)
        }
        else if (method == "maximum") {
          dist_row <- abs(a - b)
          dist_row <- max(dist_row)
        }
        else if (method == "manhattan") {
          dist_row <- abs(a - b)
          dist_row <- sum(dist_row)
        }
        else if (method == "canberra") {
          dist_row <- abs(a - b) / (abs(a) + abs(b))
          dist_row <- sum(dist_row)
        }
        else { # Binary distance
          dist_row <- len(intersect(a, b))
        }
        
        # Assign values to dist_matrix
        # so that it is square and symmetric
        dist_matrix[i, j] <- dist_row
        dist_matrix[j, i] <- dist_row
      }
    }
    return(dist_matrix)
  }
  
  dist_matrix <- calc_dist(x, method)
  rownames(dist_matrix) <- row_names
  dist_obj <- as.dist(dist_matrix)
  if (!diag && !upper) {
    return(dist_obj)
  }
  return(print(dist_obj, diag = diag, upper = upper))
}

# Test the function
a <- c(1, 2, 3, 4)
b <- c(6, 7, 6, 1)
c <- c(1, 2, 2, 5)
d <- c(8, 9, 0, 1)

mat <- rbind(a, b, c, d)

mat

stats::dist(mat, method="euclidean")
re_dist(mat, method="euclidean")

library(microbenchmark)
library(ggplot2)
bm_results <- microbenchmark(
  stats::dist(mat, method="euclidean"),
  re_dist(mat, method="euclidean"),
  times = 100
)
autoplot(bm_results)
