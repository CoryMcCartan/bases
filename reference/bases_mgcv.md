# `mgcv` integration

Provides methods so that `bases` expansions can be used as user-defined
smooth classes in [`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html).
The `k` argument to `s()` maps to the main dimension parameter of each
basis. Other arguments should be passed via the `xt` argument to `s()`,
and will be forwarded to the basis function.

## Examples

``` r
if (requireNamespace("mgcv", quietly = TRUE)) {
    x = 1:150
    z = c(1:50, rep(1, 100))
    y = as.numeric(BJsales)
    m = mgcv::gam(y ~ s(x, bs = "b_bart", k=10) + s(z, bs = "b_bart", k=20))
    summary(m)
    plot(x, y)
    lines(x, fitted(m), type="s", col="blue")
}

```
