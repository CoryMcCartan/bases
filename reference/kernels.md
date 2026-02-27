# Kernel functions

These functions return vectorized kernel functions that can be used to
calculate kernel matrices, or provided directly to other basis
functions. These functions are designed to take a maximum value of one
when identical inputs are provided. Kernels can be combined with
arithmetic expressions; see
[`?kernel-arith`](http://corymccartan.com/bases/reference/kernel-arith.md).

## Usage

``` r
k_rbf(scale = 1)

k_lapl(scale = 1)

k_rq(scale = 1, alpha = 2)

k_matern(scale = 1, nu = 1.5)

k_per(scale = 1, period = 1)
```

## Arguments

- scale:

  The kernel length scale.

- alpha:

  The shape/df parameter. \\\alpha=1\\ is the Cauchy kernel.

- nu:

  The smoothness parameter. \\\nu=0.5\\ is the Ornstein–Uhlenbeck
  kernel.

- period:

  The period, in the same units as `scale`.

## Value

A function which calculates a kernel matrix for vector arguments `x` and
`y`. The function has class `c("kernel", "function")`.

## Functions

- `k_rbf()`: Radial basis function kernel

- `k_lapl()`: Laplace kernel

- `k_rq()`: Rational quadratic kernel.

- `k_matern()`: Matérn kernel.

- `k_per()`: Periodic (exp-sine-squared) kernel.

## Examples

``` r
k = k_rbf()
x = seq(-1, 1, 0.5)
k(0, 0)
#>      [,1]
#> [1,]    1
k(0, x)
#>           [,1]      [,2] [,3]      [,4]      [,5]
#> [1,] 0.6065307 0.8824969    1 0.8824969 0.6065307
k(x, x)
#>           [,1]      [,2]      [,3]      [,4]      [,5]
#> [1,] 1.0000000 0.8824969 0.6065307 0.3246525 0.1353353
#> [2,] 0.8824969 1.0000000 0.8824969 0.6065307 0.3246525
#> [3,] 0.6065307 0.8824969 1.0000000 0.8824969 0.6065307
#> [4,] 0.3246525 0.6065307 0.8824969 1.0000000 0.8824969
#> [5,] 0.1353353 0.3246525 0.6065307 0.8824969 1.0000000

k = k_per(scale=0.2, period=0.3)
round(k(x, x))
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    0    0    1    0
#> [2,]    0    1    0    0    1
#> [3,]    0    0    1    0    0
#> [4,]    1    0    0    1    0
#> [5,]    0    1    0    0    1
```
