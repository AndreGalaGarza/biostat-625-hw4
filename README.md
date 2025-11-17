<!-- badges: start -->
  [![R-CMD-check](https://github.com/AndreGalaGarza/reDist/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AndreGalaGarza/reDist/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# reDist
## Overview
This package reimplements the base R function [dist()](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/dist) as accurately as possible.

When given a data matrix as input, ``re_dist()`` calculates the distance between each pair of rows in the input matrix using the specified distance metric. The result is stored as a ``dist`` object from base R, which is a special single-dimensional vector object that resembles a lower triangular matrix.

## Arguments
- ``x`` - An object coercible to a numeric matrix, such as a matrix, data frame, or ``dist`` object. Any non-numerical columns in ``x`` will cause an error.
- ``method`` - The distance measure to use for calculating the distance between rows. This must be one of the following: ``euclidean``, ``maximum``, ``manhattan``, ``canberra``, ``binary``, or ``minkowski``.
- ``p`` - The power of the Minkowski distance. This is only relevant if the chosen ``method`` = ``minkowski``; otherwise, it is ignored.

Note that the ``diag`` and ``upper`` parameters from ``dist()`` in base R can still be utilized with the output of ``re_dist()``, since both functions output a ``dist`` object. These parameters control whether to display the diagonal and upper triangular part of the matrix, respectively.

## Installation
```
library(devtools)

devtools::install_github("AndreGalaGarza/reDist")
```

## Usage
```
library(reDist)

# Sample matrix
a <- c(1, 2, 3, 4)
b <- c(6, 7, 6, 1)
c <- c(1, 2, 2, 5)
d <- c(8, 9, 0, 1)
mat <- rbind(a, b, c, d)

re_dist(mat, method = "euclidean")
##           a         b         c
## b  8.246211                    
## c  1.414214  9.055385          
## d 10.770330  6.633250 10.862780

re_dist(mat, method = "maximum")
##   a b c
## b 5    
## c 1 5  
## d 7 6 7

re_dist(mat, method = "manhattan")
##    a  b  c
## b 16      
## c  2 18   
## d 20 10 20

re_dist(mat, method = "canberra")
##           a         b         c
## b 2.2031746                    
## c 0.3111111 2.4365079          
## d 3.0141414 1.2678571 3.0808081

re_dist(mat, method = "binary")
##      a    b    c
## b 0.00          
## c 0.00 0.00     
## d 0.25 0.25 0.25

re_dist(mat, method = "minkowski")
##           a         b         c
## b  8.246211                    
## c  1.414214  9.055385          
## d 10.770330  6.633250 10.862780

print(re_dist(mat, method = "euclidean"), diag = TRUE, upper = TRUE)
##           a         b         c         d
## a  0.000000  8.246211  1.414214 10.770330
## b  8.246211  0.000000  9.055385  6.633250
## c  1.414214  9.055385  0.000000 10.862780
## d 10.770330  6.633250 10.862780  0.000000        
```