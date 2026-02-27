# Neural network basis

Generates random features from a one-layer neural network, i.e., a
random linear transformation followed by a nonlinear activation
function: \$\$ \phi(x) = g(w^\top x + b), \$\$ where *w* and *b* are
randomly sampled weights and bias, and *g* is the chosen activation
function.

## Usage

``` r
b_nn(
  ...,
  p = 100,
  activation = function(x) (x + abs(x))/2,
  stdize = c("scale", "box", "symbox", "none"),
  weights = NULL,
  biases = NULL,
  shift = NULL,
  scale = NULL
)
```

## Arguments

- ...:

  The variable(s) to build features for. A single data frame or matrix
  may be provided as well. Missing values are not allowed.

- p:

  The number of random features.

- activation:

  The activation function, which should take in a numeric vector and
  return another of the same length. The default is the rectified linear
  unit, i.e. `pmax(0, x)`; the implementation here is faster.

- stdize:

  How to standardize the predictors, if at all. The default `"scale"`
  applies [`scale()`](https://rdrr.io/r/base/scale.html) to the input so
  that the features have mean zero and unit variance, `"box"` scales the
  data along each dimension to lie in the unit hypercube, and `"symbox"`
  scales the data along each dimension to lie in \\\[-0.5, 0.5\]^d\\.

- weights:

  Matrix of weights; `nrow(weights)` must match the number of
  predictors. If provided, overrides those calculated automatically,
  thus ignoring `p`.

- biases:

  Vector of biases to use. If provided, overrides those calculated
  automatically, thus ignoring `p`.

- shift:

  Vector of shifts, or single shift value, to use. If provided,
  overrides those calculated according to `stdize`.

- scale:

  Vector of scales, or single scale value, to use. If provided,
  overrides those calculated according to `stdize`.

## Value

A matrix of random neural network features.

## Details

As with [`b_rff()`](http://corymccartan.com/bases/reference/b_rff.md),
to reduce the variance, a moment-matching transformation is applied to
ensure the sampled weights and biases have mean zero and the sampled
weights have unit covariance.

## Examples

``` r
data(quakes)

m = ridge(depth ~ b_nn(lat, long, p = 100, activation = tanh), quakes)
plot(fitted(m), quakes$depth)


# In 1-D with ReLU (default), equivalent to piecewise
# linear regression with random knots
x = 1:150
y = as.numeric(BJsales)
m = lm(y ~ b_nn(x, p = 8))
plot(x, y)
lines(x, fitted(m), col="blue")
```
