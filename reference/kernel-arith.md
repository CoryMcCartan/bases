# Kernel arithmetic

Kernel functions (see
[`?kernels`](http://corymccartan.com/bases/reference/kernels.md)) may be
multiplied by constants, multiplied by each other, or added together.

## Usage

``` r
# S3 method for class 'kernel'
x * k2

# S3 method for class 'kernel'
k1 + k2
```

## Arguments

- x:

  a numeric or a `kernel` function

- k2:

  a `kernel` function

- k1:

  a `kernel` function

## Value

A new kernel function, with class `c("kernel", "function")`.

## Examples

``` r
x = seq(-1, 1, 0.5)
k = k_rbf()
k2 = k_per(scale=0.2, period=0.3)

k_add = k2 + 0.5*k
print(k_add)
#> function (x, y) 
#> {
#>     k1(x, y) + k2(x, y)
#> }
#> <bytecode: 0x562ec8babed0>
#> <environment: 0x562ec8ba6c00>
#> attr(,"name")
#> [1] "per + rbf"
image(k_add(x, x))

```
