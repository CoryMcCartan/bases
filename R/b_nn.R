#' Neural network basis
#'
#' Generates random features from a one-layer neural network, i.e., a random
#' linear transformation followed by a nonlinear activation function: \deqn{
#'   \phi(x) = g(w^\top x + b),
#' } where *w* and *b* are randomly sampled weights and bias, and *g* is the
#' chosen activation function.
#'
#' As with [b_rff()], to reduce the variance, a moment-matching transformation
#' is applied to ensure the sampled weights and biases have mean zero and the
#' sampled weights have unit covariance.
#'
#' @param ... The variable(s) to build features for. A single data frame or
#'   matrix may be provided as well. Missing values are not allowed.
#' @param p The number of random features.
#' @param activation The activation function, which should take in a numeric
#'   vector and return another of the same length. The default is the rectified
#'   linear unit, i.e. `pmax(0, x)`; the implementation here is faster.
#' @param stdize How to standardize the predictors, if at all. The default
#'   `"scale"` applies `scale()` to the input so that the features have mean
#'   zero and unit variance, `"box"` scales the data along each dimension
#'   to lie in the unit hypercube, and `"symbox"` scales the data along each
#'   dimension to lie in \eqn{[-0.5, 0.5]^d}.
#' @param weights Matrix of weights; `nrow(weights)` must match the number of
#'   predictors. If provided, overrides those calculated automatically, thus
#'   ignoring `p`.
#' @param biases Vector of biases to use. If provided, overrides those
#'   calculated automatically, thus ignoring `p`.
#' @param shift Vector of shifts, or single shift value, to use. If provided,
#'   overrides those calculated according to `stdize`.
#' @param scale Vector of scales, or single scale value, to use. If provided,
#'   overrides those calculated according to `stdize`.
#'
#' @returns A matrix of random neural network features.
#'
#' @examples
#' data(quakes)
#'
#' m = ridge(depth ~ b_nn(lat, long, p=100, activation=tanh), quakes)
#' plot(fitted(m), quakes$depth)
#'
#' # In 1-D with ReLU (default), equivalent to piecewise
#' # linear regression with random knots
#' x = 1:150
#' y = as.numeric(BJsales)
#' m = lm(y ~ b_nn(x, p = 8))
#' plot(x, y)
#' lines(x, fitted(m), col="blue")
#' @export
b_nn <- function(..., p = 100, activation = function(x) (x + abs(x)) / 2,
                 stdize = c("scale", "box", "symbox", "none"),
                 weights = NULL, biases = NULL, shift = NULL, scale = NULL) {
    x = as.matrix(cbind(...))
    n = nrow(x)
    d = ncol(x)

    std = do_std(x, stdize, shift, scale)
    x = std$x

    if (is.null(weights)) {
        weights = matrix(rnorm(p * d), nrow=d, ncol=p)

        weights = weights - rowMeans(weights)
        chol_wt = chol(cov(t(weights)))
        weights = crossprod(solve(chol_wt), weights)
    } else if (nrow(weights) != d) {
        abort("`weights` must have the same number of rows as the number of input variables")
    }
    if (is.null(biases)) {
        biases = rnorm(p)
        biases = biases - mean(biases)
    } else if (length(biases) != ncol(weights)) {
        abort("`biases` must have length matching the number of columns in `weights`")
    }

    m = activation(std$x %*% weights + rep(biases, each=n))
    attr(m, "weights") = weights
    attr(m, "biases") = biases
    attr(m, "shift") = std$shift
    attr(m, "scale") = std$scale
    attr(m, "call") = rlang::current_call()
    class(m) = c("b_nn", "matrix", "array")

    m
}


#' @export
predict.b_nn <- function (object, newdata, ...)  {
    if (missing(newdata)) {
        return(object)
    }
    rlang::eval_tidy(makepredictcall(object, attr(object, "call")), newdata)
}

#' @export
makepredictcall.b_nn <- function(var, call) {
    if (as.character(call)[1L] == "b_nn" ||
        (is.call(call) && identical(eval(call[[1L]]), b_nn))) {
        at = attributes(var)[c("shift",  "scale", "weights", "biases")]
        call[names(at)] = at
    }
    call
}
