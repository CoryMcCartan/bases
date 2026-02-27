# Random Fourier feature basis

Generates a random Fourier feature basis matrix for a provided kernel,
optionally rescaling the data to lie in the unit hypercube. A good
review of random features is the Liu et al. (2021) review paper cited
below. Random features here are of the form \$\$ \phi(x) = \cos(\omega^T
x + b), \$\$ where \\\omega\\ is a vector of frequencies sampled from
the Fourier transform of the kernel, and \\b\sim\mathrm{Unif}\[-\pi,
\pi\]\\ is a random phase shift. The input data `x` may be shifted and
rescaled before the feature mapping is applied, according to the
`stdize` argument.

## Usage

``` r
b_rff(
  ...,
  p = 100,
  kernel = k_rbf(),
  stdize = c("scale", "box", "symbox", "none"),
  n_approx = nextn(4 * p),
  freqs = NULL,
  phases = NULL,
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

- kernel:

  A kernel function. If one of the recognized kernel functions such as
  [`k_rbf()`](http://corymccartan.com/bases/reference/kernels.md) is
  provided, then the computations will be exact. Otherwise, the fast
  Fourier transform of the provided kernel function is used to generate
  the random features. The kernel should be shift-invariant and decay to
  zero at positive and negative infinity.

- stdize:

  How to standardize the predictors, if at all. The default `"scale"`
  applies [`scale()`](https://rdrr.io/r/base/scale.html) to the input so
  that the features have mean zero and unit variance, `"box"` scales the
  data along each dimension to lie in the unit hypercube, and `"symbox"`
  scales the data along each dimension to lie in \\\[-0.5, 0.5\]^d\\.

- n_approx:

  The number of discrete frequencies to use in calculating the Fourier
  transform of the provided kernel. Not used for certain kernels for
  which an analytic Fourier transform is available; see above.

- freqs:

  Matrix of frequencies to use; `nrow(freqs)` must match the number of
  predictors. If provided, overrides those calculated automatically,
  thus ignoring `p` and `kernel`.

- phases:

  Vector of phase shifts to use. If provided, overrides those calculated
  automatically, thus ignoring `p` and `kernel`.

- shift:

  Vector of shifts, or single shift value, to use. If provided,
  overrides those calculated according to `stdize`.

- scale:

  Vector of scales, or single scale value, to use. If provided,
  overrides those calculated according to `stdize`.

## Value

A matrix of random Fourier features.

## Details

To reduce the variance of the approximation, a moment-matching
transformation is applied to ensure the sampled frequencies have mean
zero, per Shen et al. (2017). For the Gaussian/RBF kernel, second
moment-matching is also applied to ensure the analytical and empirical
frequency covariance matrices agree.

## References

Rahimi, A., & Recht, B. (2007). *Random features for large-scale kernel
machines.* Advances in Neural Information Processing Systems, 20.

Liu, F., Huang, X., Chen, Y., & Suykens, J. A. (2021). Random features
for kernel approximation: A survey on algorithms, theory, and beyond.
*IEEE Transactions on Pattern Analysis and Machine Intelligence*,
44(10), 7128-7148.

Shen, W., Yang, Z., & Wang, J. (2017, February). Random features for
shift-invariant kernels with moment matching. In *Proceedings of the
AAAI Conference on Artificial Intelligence* (Vol. 31, No. 1).

## Examples

``` r
data(quakes)

m = ridge(depth ~ b_rff(lat, long), quakes)
plot(fitted(m), quakes$depth)


# more random featues means a higher ridge penalty
m500 = ridge(depth ~ b_rff(lat, long, p = 500), quakes)
c(default = m$penalty, p500 = m500$penalty)
#>      default         p500 
#> 2.124304e-06 5.353267e-05 

# A shorter length scale fits the data better (R^2)
m_025 = ridge(depth ~ b_rff(lat, long, kernel = k_rbf(scale = 0.25)), quakes)
c(
  len_1 = cor(quakes$depth, fitted(m))^2,
  len_025 = cor(quakes$depth, fitted(m_025))^2
)
#>     len_1   len_025 
#> 0.9185883 0.9345562 
```
