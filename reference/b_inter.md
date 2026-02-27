# N-way interaction basis

Generates a design matrix that contains all possible interactions of the
input variables up to a specified maximum depth. The default `"symbox"`
standardization, which maps inputs to \\\[-0.5, 0.5\]^d\\, is strongly
recommended, as it means that the interaction terms will have smaller
variance and thus be penalized more by methods like the Lasso or ridge
regression (see Gelman et al., 2008).

## Usage

``` r
b_inter(
  ...,
  depth = 2,
  stdize = c("symbox", "box", "scale", "none"),
  shift = NULL,
  scale = NULL
)
```

## Arguments

- ...:

  The variable(s) to build features for. A single data frame or matrix
  may be provided as well. Missing values are not allowed.

- depth:

  The maximum interaction depth. The default is 2, which means that all
  pairwise interactions are included.

- stdize:

  How to standardize the predictors, if at all. The default `"scale"`
  applies [`scale()`](https://rdrr.io/r/base/scale.html) to the input so
  that the features have mean zero and unit variance, `"box"` scales the
  data along each dimension to lie in the unit hypercube, and `"symbox"`
  scales the data along each dimension to lie in \\\[-0.5, 0.5\]^d\\.

- shift:

  Vector of shifts, or single shift value, to use. If provided,
  overrides those calculated according to `stdize`.

- scale:

  Vector of scales, or single scale value, to use. If provided,
  overrides those calculated according to `stdize`.

## Value

A matrix with the rescaled and interacted features.

## References

Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y. S. (2008). A weakly
informative default prior distribution for logistic and other regression
models.

## Examples

``` r
# default: all pairwise interactions
lm(mpg ~ b_inter(cyl, hp, wt), mtcars)
#> 
#> Call:
#> lm(formula = mpg ~ b_inter(cyl, hp, wt), data = mtcars)
#> 
#> Coefficients:
#>                (Intercept)     b_inter(cyl, hp, wt)cyl  
#>                     15.225                       2.914  
#>     b_inter(cyl, hp, wt)hp      b_inter(cyl, hp, wt)wt  
#>                    -11.443                     -13.041  
#> b_inter(cyl, hp, wt)cyl:hp  b_inter(cyl, hp, wt)cyl:wt  
#>                     13.724                       6.855  
#>  b_inter(cyl, hp, wt)hp:wt  
#>                      7.050  
#> 

# how number of features depends on interaction depth
for (d in 1:6) {
    X = with(mtcars, b_inter(cyl, disp, hp, drat, wt, depth=d))
    print(ncol(X))
}
#> [1] 5
#> [1] 15
#> [1] 25
#> [1] 30
#> [1] 31
#> [1] 31
```
