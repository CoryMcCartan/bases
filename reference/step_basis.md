# Recipe step for basis expansions

`step_basis()` is a single function that creates a *specification* of a
recipe step that will create new columns that are basis expansions,
using any of the basis expansion functions in this package.

## Usage

``` r
step_basis(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  fn = NULL,
  options = list(),
  object = NULL,
  prefix = deparse(substitute(fn)),
  skip = FALSE,
  id = recipes::rand_id("basis")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose variables for this step. See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as predictors in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- fn:

  The basis function to use, e.g.,
  [`b_rff()`](http://corymccartan.com/bases/reference/b_rff.md).

- options:

  A list of options for the basis function `fn`.

- object:

  The basis object created once the step has been trained.

- prefix:

  The prefix to use for the new column names. Numbers will be appended,
  per
  [`recipes::names0()`](https://recipes.tidymodels.org/reference/names0.html),
  to create column names.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)?

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of recipe with the new step added to the sequence of
any existing operations.

## Tuning Parameters

There are no tuning parameters made available to the `tunable`
interface.

## Case Weights

The underlying operation does not use case weights.

## Examples

``` r
rec = recipes::recipe(depth ~ lat + long + mag, quakes)
rec_rff = step_basis(rec, lat, long, fn = b_rff,
                     options = list(p = 5, kernel = k_rbf(2), stdize="none"))
recipes::bake(recipes::prep(rec_rff), new_data=NULL)
#> # A tibble: 1,000 × 7
#>      mag depth   b_rff1  b_rff2  b_rff3  b_rff4  b_rff5
#>    <dbl> <int>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1   4.8   562  0.00952 -0.415  -0.116   0.444  -0.370 
#>  2   4.2   650  0.0444  -0.320  -0.140   0.447  -0.444 
#>  3   5.4    42 -0.155    0.447   0.156  -0.447   0.0492
#>  4   4.1   626  0.0150  -0.212   0.389   0.270   0.278 
#>  5   4     649 -0.0110  -0.444  -0.129   0.439  -0.315 
#>  6   4     195 -0.148   -0.0484 -0.0459  0.404   0.389 
#>  7   4.8    82  0.367    0.300  -0.196   0.221   0.361 
#>  8   4.4   194 -0.0339   0.240   0.431  -0.424   0.171 
#>  9   4.7   211 -0.0244   0.255   0.447  -0.383  -0.0598
#> 10   4.3   622  0.139    0.423   0.446  -0.0279 -0.0813
#> # ℹ 990 more rows
```
