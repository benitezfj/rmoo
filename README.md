
<!-- README.md is generated from README.Rmd. Please edit that file -->
nsga3r
======

<!-- badges: start -->
<!-- badges: end -->
Overview
--------

A Non-Dominance-Based Multi-Objective Optimization package, built on top of the [GA package](http://www.jstatsoft.org/v53/i04/). nsga3r provides a complete and flexible framework for optimizing multiple supplied objectives. You will have at your disposal a wide range of configuration options for the NSGA, NSGA-II and NSGA-III algorithms.

Installation
------------

``` r
# You can install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("benitezfj/nsga3r")
```

Usage
-----

This is a basic example which shows you how to solve a common problem:

``` r
library(nsga3r)

DTLZ1 <- function (x, nobj = 3) 
{
    if (is.null(dim(x))) {
        x <- matrix(x, 1)
    }
    n <- ncol(x)
    y <- matrix(x[, 1:(nobj - 1)], nrow(x))
    z <- matrix(x[, nobj:n], nrow(x))
    g <- 100 * (n - nobj + 1 + rowSums((z - 0.5)^2 - cos(20 * 
        pi * (z - 0.5))))
    tmp <- t(apply(y, 1, cumprod))
    tmp <- cbind(t(apply(tmp, 1, rev)), 1)
    tmp2 <- cbind(1, t(apply(1 - y, 1, rev)))
    f <- tmp * tmp2 * 0.5 * (1 + g)
    return(f)
}

result <- nsga3(fitness = GPareto::DTLZ1,
                type = "real-valued",
                lower = c(0,0,0),
                upper = c(1,1,1),
                popSize = 92,
                n_partitions = 12,
                maxiter = 300)
```

![](https://github.com/benitezfj/nsga3r/blob/master/man/figures/README-example-1.jpeg)<!-- -->

![](https://github.com/benitezfj/nsga3r/blob/master/man/figures/README-example-2.png)<!-- -->
