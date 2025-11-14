# re_dist(): Reimplementation of dist() from base R
## Overview
This package reimplements the base R function [dist()](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/dist).

When given a data matrix as input, ``re_dist()`` calculates the distance between each pair of rows in the input matrix using the specified distance metric. The result is stored as a special single-dimensional vector object that resembles a lower triangular matrix.

## Arguments
- ``x`` - A numeric matrix, data frame, or ``dist`` object.
- ``method`` - The distance measure to use for calculating the distance between rows. This must be one of the following: ``euclidean``, ``maximum``, ``manhattan``, ``canberra``, ``binary``, or ``minkowski``.
- ``diag`` - Default value = FALSE. Set to TRUE to print the diagonal of the distance matrix.
- ``upper`` - Default value = FALSE. Set to TRUE to print the upper triangular part of the distance matrix.
- ``p`` - The power of the Minkowski distance. This is only relevant if the chosen ``method`` = ``minkowski``.
