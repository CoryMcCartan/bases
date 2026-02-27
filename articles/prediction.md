# Valid Prediction

This article provides more detail on how **bases** ensures valid
prediction, i.e., prevents any data leakage when new predictions are
made.

Every basis function provides the
[`makepredictcall()`](https://rdrr.io/r/stats/makepredictcall.html)
generic, which is called by
[`model.frame()`](https://rdrr.io/r/stats/model.frame.html) and whose
job it is to save any statistics used by the basis expansion (such as a
set of randomly sampled frequencies and phase shifts) for reuse later.

Basis functions support the
[`predict()`](https://rdrr.io/r/stats/predict.html) generic, so that if
they are called outside of a model formula, they can be updated with new
data. Behind the scenes,
[`predict()`](https://rdrr.io/r/stats/predict.html) for the various
basis functions is just a small wrapper around
[`makepredictcall()`](https://rdrr.io/r/stats/makepredictcall.html).

To demonstrate these points, we will use the
[`b_rff()`](http://corymccartan.com/bases/reference/b_rff.md) basis
function, which uses random features. However, the features are sampled
once on construction and then retained for further use. First, in the
modeling context, we’ll fit a model with
[`b_rff()`](http://corymccartan.com/bases/reference/b_rff.md) in the
formula.

``` r
library(bases)
data(mtcars)

m = lm(mpg ~ b_rff(cyl, disp, hp, wt, p = 10), mtcars)
```

Repeated calls to [`predict()`](https://rdrr.io/r/stats/predict.html)
will yield the same predictions, even if the `newdata` argument is not
empty.

``` r
all.equal(predict(m), predict(m, newdata = mtcars))
#> [1] TRUE
all.equal(predict(m, newdata = mtcars[5:10, ]), 
          predict(m, newdata = mtcars[5:10, ]))
#> [1] TRUE
```

The same is true if
[`b_rff()`](http://corymccartan.com/bases/reference/b_rff.md) is used
outside of a formula.

``` r
B = with(mtcars, b_rff(cyl, disp, hp, wt, p = 10))

all.equal(B, predict(B))
#> [1] TRUE
all.equal(B, predict(B, newdata = mtcars), check.attributes = FALSE)
#> [1] TRUE
nrow(predict(B, newdata = mtcars[1:3, ]))
#> [1] 3
```
