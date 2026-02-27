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
#>      mag depth  b_rff1  b_rff2  b_rff3   b_rff4 b_rff5
#>    <dbl> <int>   <dbl>   <dbl>   <dbl>    <dbl>  <dbl>
#>  1   4.8   562 -0.426   0.445  -0.406  -0.414   -0.419
#>  2   4.2   650 -0.446   0.390  -0.388  -0.377   -0.374
#>  3   5.4    42  0.249   0.419  -0.206   0.00833  0.123
#>  4   4.1   626  0.121   0.350  -0.447   0.194   -0.304
#>  5   4     649 -0.404   0.445  -0.412  -0.438   -0.438
#>  6   4     195  0.124  -0.0616 -0.447  -0.382   -0.345
#>  7   4.8    82 -0.384   0.429  -0.388   0.174    0.402
#>  8   4.4   194  0.160  -0.292   0.0317 -0.109   -0.118
#>  9   4.7   211 -0.0462 -0.380   0.0849 -0.241   -0.100
#> 10   4.3   622 -0.111   0.413  -0.442   0.425    0.120
#> # ℹ 990 more rows
```
