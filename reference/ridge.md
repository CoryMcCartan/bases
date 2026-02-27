# Ridge regression

Lightweight routine for ridge regression, fitted via a singular value
decomposition. The penalty may be automatically determined by
leave-one-out cross validation. The intercept term is unpenalized.

## Usage

``` r
ridge(formula, data, penalty = "auto", ...)

# S3 method for class 'ridge'
fitted(object, ...)

# S3 method for class 'ridge'
coef(object, ...)

# S3 method for class 'ridge'
predict(object, newdata, ...)
```

## Arguments

- formula:

  A model formula; see [formula](https://rdrr.io/r/stats/formula.html).
  The intercept term is unpenalized; to fit a penalized intercept,
  remove the intercept and add your own to the design matrix.

- data:

  An optional data frame or object in which to interpret the variables
  occurring in formula.

- penalty:

  The ridge penalty. Must be a single numeric or the string "auto", in
  which case the penalty will be determined via leave-one-out cross
  validation to minimize the mean squared error.

- ...:

  Further arguments, passed on to
  [`model.frame()`](https://rdrr.io/r/stats/model.frame.html) and
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html). These
  must be provided to `predict.ridge()` as well, if used.

- object:

  A fitted `ridge()` model.

- newdata:

  A data frame containing the new data to predict

## Value

An object of class `ridge` with components including:

- `coef`, a vector of coefficients.

- `fitted`, a vector of fitted values.

- `penalty`, the penalty value.

## Methods (by generic)

- `fitted(ridge)`: Fitted values

- `coef(ridge)`: Coefficients

- `predict(ridge)`: Predicted values

## Examples

``` r
m_lm = lm(mpg ~ ., mtcars)
m_ridge = ridge(mpg ~ ., mtcars, penalty=1e3)
plot(fitted(m_lm), fitted(m_ridge), ylim=c(10, 30))
abline(a=0, b=1, col="red")

```
