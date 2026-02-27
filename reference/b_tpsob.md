# Tensor-product Sobolev space basis

Generates features from a tensor-product Sobolev space basis for
estimating functions in a Sobolev space with dominating mixed
derivatives. Basis functions are of the form \$\$
\psi\_{\mathbf{j}}(\mathbf{x}) = \prod\_{k=1}^d \psi\_{j_k}(x_k), \$\$
where \$\$ \phi_1(x) = 1 \quad\text{and}\quad \phi_j(x) =
\sqrt{2}\cos(\pi (j-1) x). \$\$ The multi-indices \\\mathbf{j}\\ are
generated in a specific order to maximize statistical efficiency. All
inputs are standardized to lie in the unit hypercube \\\[0, 1\]^d\\.

## Usage

``` r
b_tpsob(..., p = 100, shift = NULL, scale = NULL)
```

## Arguments

- ...:

  The variable(s) to build features for. A single data frame or matrix
  may be provided as well. Missing values are not allowed.

- p:

  The number of basis functions to generate.

- shift:

  Vector of shifts, or single shift value, to use. If provided,
  overrides those calculated according to `stdize`.

- scale:

  Vector of scales, or single scale value, to use. If provided,
  overrides those calculated according to `stdize`.

## Value

A matrix of tensor-product Sobolev space basis features.

## References

Zhang, T., & Simon, N. (2023). Regression in tensor product spaces by
the method of sieves. *Electronic journal of statistics*, 17(2), 3660.

## Examples

``` r
data(quakes)

m = ridge(depth ~ b_tpsob(lat, long, p = 100), quakes)
plot(fitted(m), quakes$depth)


x = 1:150
y = as.numeric(BJsales)
m = lm(y ~ b_tpsob(x, p = 10))
plot(x, y)
lines(x, fitted(m), col="blue")
```
